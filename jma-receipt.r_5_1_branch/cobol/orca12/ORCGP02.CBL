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
       PROGRAM-ID.             ORCGP02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 患者情報（Ｐ０２）（メイン）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC-太田      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-太田     01/04/18  生年月日未来日チェック追加
      *  01.00.02    MCC-太田     01/04/20  受付戻り時の患者IDセット
      *  01.00.03    MCC-太田     01/04/25  終了時の更新確認等
      *  01.00.04    MCC-太田     01/05/24  終了時の更新確認等修正
      *  01.00.05    MCC-多々納   01/06/13  登録後、診療行為遷移を追加
      *  01.00.06    MCC-多々納   01/07/02  日付画面用チェックサブ追加
      *                                     年齢表示追加
      *                                     画面遷移後のカーソル移動修正
      *  01.00.07    MCC-多々納   01/12/04  画面遷移変更に伴う修正
      *  01.00.08    MCC-多々納   02/04/05  労災保険の追加
      *  01.00.09    MCC-太田     02/05/23  患者照会からの遷移を考慮
      *  01.00.10    MCC-太田     02/05/31  カナ氏名候補画面を追加
      *  01.00.11    MCC-多々納   02/06/18  月代り受給者番号の追加
      *  02.00.00    MCC-多々納   02/06/21  画面レイアウト変更
      *  02.00.01    NACL-多々納  02/09/10  平成１４年１０月改正対応
      *  02.00.02    NACL-多々納  02/10/23  入院追加
      *  02.00.03    NACL-多々納  02/11/07  入院基本画面遷移追加
      *  02.00.04    NACL-多々納  02/11/20  ＣＬＡＩＭ処理追加
      *  02.00.05    NACL-多々納  02/11/21  共済下船３月追加
      *  02.00.06    NACL-多々納  03/01/28  一括削除に患者定期請求追加
      *  02.00.07    NACL-多々納  03/03/05  排他制御処理追加
      *  02.00.08    NACL-多々納  03/03/24  公費負担額追加
      *  02.00.07    NACL-多々納  03/04/22  保険組合変更時メッセージ追加
      *  02.00.08    NACL-多々納  03/07/08  保険者異動日チェック追加
      *  02.00.09    NACL-多々納  04/04/01  患者削除のＤＢ追加
      *  02.00.10    NACL-多々納  05/11/11  MONFUNC 対応 他
      *  02.00.11    NACL-多々納  06/02/24  主科対応対応
      *  03.00.01    NACL-多々納  06/08/25  平成１８年１０月改正対応
      *                                     （老人割合変更）
      *  03.04.00    NACL-多々納  07/02/16  ＱＲ受付対応
      *  03.04.01    NACL-多々納  07/02/20  収納履歴マスタ削除追加
      *  03.05.00    NACL-多々納  07/05/XX  グループ診療対応
      *  03.05.01    NACL-多々納  07/08/16  患者メモ削除追加
      *                                     ＱＲ受付削除
      *  04.01.00    NACL-多々納  07/12/XX  患者禁忌薬剤マスタ追加
      *  04.02.00    NACL-多々納  08/03/XX  平成２０年４月改正対応
      *  04.03.00    NACL-多々納  08/07/22  特記事項追加
      *  04.04.00    NACL-多々納  09/01/27  本院分院対応
      *  04.04.00    NACL-多々納  09/03/12  テーブル削除追加
      *  04.05.00    NACL-多々納  09/07/XX  患者公費負担額機能追加
      *                                     公費負担額未入力警告対応
      *  04.06.00    NACL-多々納  11/03/24  患者情報削除サブ対応
      *  04.07.00    NACL-多々納  12/01/11  クライアント印刷対応
      *  04.07.00    NACL-多々納  12/08/08  患者削除再確認対応
      *  05.00.00    NACL-多々納  17/07/XX  PUSH通信対応
      *  05.00.00    NACL-多々納  19/10/XX  削除ログ追加対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    画面非表示
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    患者登録スパ領域
           COPY    "P02COMMON-SPA".
      *
      *    患者登録全画面スパ領域
           COPY    "P02SCR-SPA".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PARA            PIC 9(01).
      *    03  FLG-UKETUKE         PIC 9(01).
      *    03  FLG-PTNUMO          PIC 9(01).
           03  FLG-PTNUM           PIC 9(01).
      *    03  FLG-YYK             PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
      *    03  FLG-PTHKNINF        PIC 9(01).
      *    03  FLG-PTKOHINF        PIC 9(01).
      *    03  FLG-HKNCOMBI        PIC 9(01).
      *    03  FLG-KYUSEIRRK       PIC 9(01).
      *    03  FLG-PTGETNUM        PIC 9(01).
      *    03  FLG-TSYRRK          PIC 9(01).
      *    03  FLG-TNKRRK          PIC 9(01).
      *    03  FLG-RECECOM         PIC 9(01).
      *    03  FLG-PTRSIINF        PIC 9(01).
      *    03  FLG-MONTHLYNUM      PIC 9(01).
           03  FLG-QRCDHKN         PIC 9(01).
      *
      *    03  FLG-PTNYUINRRK      PIC 9(01).
      *    03  FLG-PTTAINFUKA      PIC 9(01).
      *    03  FLG-NYUINACT        PIC 9(01).
      *    03  FLG-NYUINACCT       PIC 9(01).
      *    03  FLG-PTSAME          PIC 9(01).
      *
           03  FLG-SYSKANRI        PIC 9(01).
      *
           03  FLG-HAKKOU          PIC 9(01).
      *
           03  FLG-BACK            PIC 9(01).
           03  FLG-CLAIM           PIC 9(01).
           03  FLG-CLAIM-OK        PIC 9(01).
      *    ユーザプログラム起動
           03  FLG-USERPG          PIC 9(01).
      *
           03  FLG-CLAIM-MULTIHOST     PIC 9(01).
           03  FLG-SOUSIN              PIC 9(01).
      *    クライアント印刷有
           03  FLG-CLIENT-OK       PIC 9(01).
           03  FLG-PRT             PIC 9(01).
      *H29.7
      *PUSH通信用
           03  FLG-PUSHSYORI       PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-YMD             PIC X(08).
           03  WRK-ERRMSG          PIC X(40).
           03  WRK-SAKIPG          PIC X(08).
           03  WRK-WIDGET          PIC X(20).
           03  WRK-P02-GAMEN       PIC X(06).
           03  WRK-PIDCD           PIC X(04).
      *
           03  WRK-EVENT           PIC X(20).
           03  WRK-STATUS          PIC X(04).
           03  WRK-ACCEPT          PIC X(03).
           03  WRK-ID-AREA.
               05  WRK-HKNID           PIC 9(10).
               05  WRK-KOHID           PIC 9(10).
               05  WRK-AUTOCOMBINUM    PIC 9(04).
               05  WRK-MANUCOMBINUM    PIC 9(04).
           03  WRK-PIDMSG          PIC X(100).
      *
           03  WRK-MOTOPG              PIC X(08).
           03  WRK-ERRCD               PIC X(04).
      *
           03  WRK-MEISAI1         PIC 9(06).
           03  WRK-MEISAI2         PIC 9(06).
           03  WRK-MEISAI3         PIC 9(06).
      *    文字数
           03  WRK-KETA                PIC 9(02).
           03  FLG-KETA                PIC 9(01).
      *
      *保存元ＰＧ
           03  WRK-HZN-PTNUM              PIC X(20).
           03  WRK-HZN-MOTOPG             PIC X(08).
      *    受付時間
           03  WRK-UKETIME                PIC X(06).
           03  WRK-PTID                   PIC X(10).
      *
           03  WRK-IDOYMD-H            PIC X(22).
      *
           03  WRK-GMN-SELNUM          PIC 9(02). 
      *
           03  WRK-RENNUM              PIC 9(02).
      *
      *    カルテＰＧ名
           03  WRK-KARUTE-PG           PIC X(20).
      *    処方せんＰＧ名
           03  WRK-SYOHOU-PG           PIC X(20).
      *
      *H29.7
      *PUSH通信用
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY          PIC X(04).
               05  WRK-HEN-H1          PIC X(01).
               05  WRK-HEN-MM          PIC X(02).
               05  WRK-HEN-H2          PIC X(01).
               05  WRK-HEN-DD          PIC X(02).
      *    時間編集
           03  WRK-THMS.
               05  WRK-THHH             PIC X(02).
               05  WRK-THMM             PIC X(02).
               05  WRK-THSS             PIC X(02).
           03  WRK-HEN-TIME.
               05  WRK-HEN-THH         PIC X(02).
               05  WRK-HEN-T1          PIC X(01).
               05  WRK-HEN-TMM         PIC X(02).
               05  WRK-HEN-T2          PIC X(01).
               05  WRK-HEN-TSS         PIC X(02).
      *
      *
      *    ＣＬＡＩＭファイル名
       01  CLAIM-FILE.
           03  FILLER              PIC X(30)   VALUE
               "scripts/claim/HL01.sh".
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
      *
      *    画面カーソル移動用
       01  WRK-WIDGET-AREA.
           03  WRK-P020-WIDGET         PIC X(64).
           03  WRK-P02W1-WIDGET        PIC X(64).
           03  WRK-P027-WIDGET         PIC X(64).
           03  WRK-P021-WIDGET         PIC X(64).
           03  WRK-P022-WIDGET         PIC X(64).
           03  WRK-P024-WIDGET         PIC X(64).
           03  WRK-P025-WIDGET         PIC X(64).
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
      *    スパ領域 ワーク
           COPY    "COMMON-SPA"      REPLACING
                                     //SPA-// BY //WRK-SPA-//.
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *　　（ＣＬＡＩＭ接続）
           COPY    "CPSK9000.INC".
           COPY    "CPSK9001.INC".
      *　　（帳票プログラム）
           COPY    "CPSK1032.INC".
      *
      *    患者番号変換マスタ
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
      *
      *    QRコード(被保険者証)（使用なし）
       01  QRCDHKN-REC.
           COPY    "CPQRCDHKN.INC".
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
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *   患者番号発生サブ
           COPY    "CPORCSP01.INC".
      *   画面編集サブ
           COPY    "CPORCSP02.INC".
      *   チェックデジット
      *    COPY    "CPORCCHKDGT.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ユーザプログラム起動チェックパラメタ
           COPY    "CPORCSUSERCHK.INC".
      *
      *    カルテパラメータ
           COPY    "CPORCHC01.INC".
      *
      *    処方箋印刷パラメータ
           COPY    "CPORCHC19.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    クライアント印刷制御
           COPY    "CPORCSPRTCTRL.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA"          REPLACING
                                     //MCP// BY //WRK-MCP//.
      *
      *===> For Claim Start
           COPY    "CPSHELLTBL.INC".
      *****COPY    "ORCA-DBPATH".
      *===> For Claim End
      *H29.7
      *PUSH通信
           COPY    "CPPUSHPATIENT01.INC".
      *
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
           COPY    "ORCA12SCRAREA.INC".
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
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  SPA-FLG-END
           MOVE    ZERO            TO  FLG-END
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
           END-EVALUATE
      *
      *    画面編集後、表示へ
           IF     (FLG-END             =   ZERO)   AND
                  (SPA-FLG-END         =   ZERO)
               PERFORM 500-SET-SCREEN
      *
           END-IF
      *
      *    クライアント印刷
      *    IF      FLG-CLIENT-OK       =   1
      *        PERFORM 600-CLIENT-DMY-SEC
      *    END-IF
      *
           MOVE    SPA-P02KYOUTU   TO  SPA-WORK
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
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "PERR"
               MOVE    SPACE           TO  SPA-MOTOPG
               MOVE    SPA-WIDGET      TO  MCP-WIDGET
               MOVE    SPACE           TO  SPA-WIDGET
               IF      SPA-KEI-ERRCD       =   "9100"
                                           OR  "9101"
                                           OR  "9102"
                   PERFORM 49021-KOUSIN-AFTER-SEC
               IF  ( FLG-END           =   1 )  OR
                   ( SPA-FLG-END       =   1 )
                   CONTINUE
               ELSE
      *            画面編集
                   PERFORM 500-GMNHEN-SEC
               END-IF
               END-IF
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *
               IF  ( FLG-END           =   1 )  OR
                   ( SPA-FLG-END       =   1 )
                   CONTINUE
               ELSE
      *            画面編集
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF  ( FLG-END           =   1 )  OR
               ( SPA-FLG-END       =   1 )
               CONTINUE
           ELSE
               MOVE    SPACE               TO  LINKAREA
      *
               MOVE   SPACE                TO  MCP-PUTTYPE
               MOVE   "P02"                TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
               MOVE    1                   TO  FLG-END
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF      LINKAREA            NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
      *!
               CALL    "ORCSSPACHK"        USING    SPA-AREA
      *!
      *        元の画面へ戻す
               MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
               IF      WRK-MOTOPG(1:3)     =   "Y01"  OR  "U01"  OR
                                               "P97"
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-P02SCRAREA
                   MOVE    SPA-WORK        TO  SPA-P02KYOUTU
               END-IF
      *        予約の時、そのまま画面表示へ
               IF      WRK-MOTOPG(1:3)     =   "Y01"
                   GO      TO              300-SCREEN-EXT
               END-IF
      *        外部より選択がある時
               IF      WRK-MOTOPG(1:3)     =   "U01"  OR
                                               "P97"
      *            表示画面設定
                   PERFORM 3001-P97SPASET-SEC
                   GO      TO              300-SCREEN-EXT
               ELSE
                   MOVE    SPA-MOTOPG      TO  SPA-MOTOPG2
               END-IF
           END-IF
      *!
           CALL    "ORCSSPACHK"        USING    SPA-AREA
      *!
      *
           MOVE    SPA-WORK                TO  SPA-P02KYOUTU
           MOVE    ZERO                    TO  SPA-FLG-END
           MOVE    SPACE                   TO  SPA-ERRCD
      *
           EVALUATE    SPA-MOTOPG
      *        業務選択画面より
               WHEN    "M01"
                   INITIALIZE                  SPA-P02SCRAREA
                   INITIALIZE                  SPA-P02KYOUTU
                   MOVE    "P02"               TO  SPA-P02-GAMEN
                   MOVE    ZERO                TO  SPA-GMN-CUR
      *            各画面を初期編集する
                   PERFORM 310-SPA-SET-SEC
      *        ユーザプログラム実行
               WHEN    "XD01"
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-P02SCRAREA
                   MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *            各画面を初期編集する
                   MOVE    SPA-HZN-PTNUM       TO  WRK-HZN-PTNUM
                   MOVE    SPA-HZN-MOTOPG      TO  WRK-HZN-MOTOPG
                   INITIALIZE                  SPA-P02SCRAREA
                   INITIALIZE                  SPA-P02KYOUTU
                   MOVE    WRK-HZN-PTNUM       TO  SPA-HZN-PTNUM
                   MOVE    WRK-HZN-MOTOPG      TO  SPA-HZN-MOTOPG
                   MOVE    "P02"               TO  SPA-P02-GAMEN
                   MOVE    ZERO                TO  SPA-GMN-CUR
                   PERFORM 310-SPA-SET-SEC
      *        受付、診療行為、患者照会より
               WHEN    "K02"
               WHEN    "K02N"
               WHEN    "U02"
               WHEN    "Q02"
               WHEN    "I01"
                   INITIALIZE                  SPA-P02SCRAREA
                   INITIALIZE                  SPA-P02KYOUTU
                   MOVE    "P02"               TO  SPA-P02-GAMEN
                   MOVE    ZERO                TO  SPA-GMN-CUR
                   MOVE    SPA-PTNUM           TO  SPA-HZN-PTNUM
                   MOVE    SPA-MOTOPG          TO  SPA-HZN-MOTOPG
                   IF      SPA-PTNUM       NOT =   SPACE
                       MOVE    SPA-PTNUM           TO  P02-PTNUM
                       PERFORM 4100-PTNUM-SYORI-SEC
      *                拡張構成画面は表示しない
                       IF     (FLG-END              =   1  )
                       AND    (MCP-WINDOW           =   "P014" )
      *                各画面を初期編集する
                           MOVE    "   "               TO  MCP-PUTTYPE
                       END-IF
                   ELSE
      *                各画面を初期編集する
                       MOVE    1                   TO  SPA-P02-SYORI
                       PERFORM 310-SPA-SET-SEC
                   END-IF
      *        確認メッセージ
               WHEN    "PID1"
                   PERFORM 3001-PID1-SYORI-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       PERFORM 510-ERRSET-SEC
                   END-IF
                   IF      SPA-ERRCD           =   SPACE
                   AND     SPA-PIDCD       NOT =   SPACE
                       PERFORM 520-JIDSET-SEC
                   END-IF
      *        拡張患者番号設定
               WHEN    "P014"
                   MOVE    "P02"           TO  SPA-P02-GAMEN
                   IF      SPA-GMN014-PTNUM    =   SPACE
                       CONTINUE
                   ELSE
                       MOVE    SPA-GMN014-PTNUM    TO  SPA-GMN-PTNUM
                       MOVE    1                   TO  SPA-P02-SYORI
                       IF      SPA-GMN014-SYORI    =   ZERO
                          AND  SPA-QR-FLG          =   ZERO
                           PERFORM 310-SPA-SET-SEC
                       ELSE
                           MOVE    1                   TO  P02-PAGENO
                       END-IF
                       MOVE    ZERO                TO  SPA-QR-FLG
                       MOVE    1                   TO  SPA-GMN-CUR
                       MOVE    SPACE               TO  SPA-P02-WIDGET
                   END-IF
      *        患者番号再設定
               WHEN    "P015"
                   MOVE    "P02"               TO  SPA-P02-GAMEN
                   MOVE    1                   TO  SPA-GMN-CUR
                   MOVE    SPACE               TO  SPA-P02-WIDGET
                   MOVE    1                   TO  P02-PAGENO
      *        患者複写
               WHEN    "P02E"
                   MOVE    "P02"               TO  SPA-P02-GAMEN
                   IF      SPA-GMN2E-PTNUM     =   SPACE
                       MOVE    1                   TO  SPA-GMN-CUR
                   ELSE
                       MOVE    "1"                 TO  SPA-UPDFLG
      *                複写あり（各画面の初期編集）
                       MOVE    "LINK"              TO  MCP-STATUS
                       MOVE    "P02E"              TO  SPA-MOTOPG
                       PERFORM 49012-GMNSPA-SET-SEC
                       MOVE    1                   TO  SPA-GMN-CUR
                   END-IF
      *        二重登録画面より
               WHEN    "P02A"
                   IF      SPA-NAI2A-PTNUM NOT =   SPACE
                       MOVE    SPA-NAI2A-PTNUM     TO  P02-PTNUM
                       PERFORM 4100-PTNUM-SYORI-SEC
                       MOVE    SPACE              TO  SPA-NAI2A-PTNUM
                   ELSE
      *                カーソルを移動する
                       MOVE    "LINK"              TO  MCP-STATUS
                       PERFORM 3002-GMNSET-SEC
                   END-IF
      *        他医院患者複写
               WHEN    "P02G"
                   MOVE    "P02"               TO  SPA-P02-GAMEN
                   IF      SPA-P02G-SYORI      =   ZERO
                       MOVE    1                   TO  SPA-GMN-CUR
                   ELSE
                       MOVE    "1"                 TO  SPA-UPDFLG
      *                複写あり（各画面の初期編集）
                       MOVE    "LINK"              TO  MCP-STATUS
                       MOVE    "P02G"              TO  SPA-MOTOPG
                       PERFORM 49012-GMNSPA-SET-SEC
                       MOVE    1                   TO  SPA-GMN-CUR
                   END-IF
      *        患者紐付け画面
               WHEN    "P02G2"
                   MOVE    "P02"               TO  SPA-P02-GAMEN
                   MOVE    1                   TO  SPA-GMN-CUR
                   MOVE    SPACE               TO  SPA-P02-WIDGET
                   MOVE    1                   TO  P02-PAGENO
      *        主科設定画面
               WHEN    "P02S"
                   MOVE    "P02"               TO  SPA-P02-GAMEN
                   MOVE    1                   TO  SPA-GMN-CUR
                   MOVE    SPACE               TO  SPA-P02-WIDGET
                   MOVE    1                   TO  P02-PAGENO
      *        禁忌薬剤
               WHEN    "P02Y"
                   CONTINUE
      *        ＣＬＡＩＭ画面より
               WHEN    "P02X"
                   MOVE    "P02"               TO  SPA-P02-GAMEN
                   IF      SPA-P02X-FLG        =   SPACE
                       MOVE    1                   TO  SPA-GMN-CUR
                   ELSE
                       PERFORM 3009-CLAIM-SYORI-SEC
                   END-IF
      *        他一部負担金設定画面
               WHEN    "P02K"
                   MOVE    "P024"              TO  SPA-P02-GAMEN
                   MOVE    31                  TO  SPA-GMN24-CUR
                   MOVE    SPACE               TO  SPA-P02-WIDGET
                   MOVE    "P024_FTNSELNUM2"   TO  SPA-P02-WIDGET
                   MOVE    4                   TO  P02-PAGENO
      *ver.4.7
      *        確認年月日履歴画面より
               WHEN    "P02T"
                   IF      SPA-P02-GAMEN       =   "P02"
                       MOVE    1                   TO  P02-PAGENO
                   ELSE
                       MOVE    2                   TO  P02-PAGENO
                   END-IF
      *!
      *DDDDDDD ＱＲより
      *        WHEN    "P02Q"
      *            MOVE    "P02"               TO  SPA-P02-GAMEN
      *            IF      SPA-QR-SYORI        =   SPACE
      *                IF      SPA-GMN-PTNUM       =   SPACE
      *                    MOVE    0                   TO  SPA-GMN-CUR
      *                ELSE
      *                    MOVE    1                   TO  SPA-GMN-CUR
      *                END-IF
      *            ELSE
      *                PERFORM 3010-QRDATA-SYORI-SEC
      *DDDDDDDDDD  END-IF
      *        各画面より
               WHEN    OTHER
                   MOVE    "LINK"              TO  MCP-STATUS
                   PERFORM 3002-GMNSET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｐ９７より処理
      *****************************************************************
       3001-P97SPASET-SEC              SECTION.
      *
           MOVE    LNK-KYOUTU          TO  WRK-SPA-KYOUTU
      *
      *    受付、患者検索より
           IF    ( WRK-SPA-PTNUM   NOT =   SPA-GMN-PTNUM )  AND
                 ( WRK-SPA-PTNUM   NOT =   SPACE         )
      *        患者番号選択あり
               MOVE    WRK-SPA-PTNUM       TO  P02-PTNUM
               MOVE    WRK-SPA-P02-UKEID   TO  SPA-P02-UKEID
               PERFORM 4100-PTNUM-SYORI-SEC
           ELSE
      *        受付氏名のみ選択あり
              IF      (WRK-SPA-PTNUM       =   SPACE   )  AND
                      (WRK-SPA-NAME    NOT =   SPACE   )  AND
                      (WRK-SPA-P02-UKEID   NOT =   ZERO    )
      *H29.11
                   MOVE    SPACE               TO  SPA-GMN-PTNUM
                   MOVE    ZERO                TO  SPA-GMN-PTID
                   MOVE    WRK-SPA-NAME        TO  SPA-NAME
                   MOVE    WRK-SPA-P02-UKEID   TO  SPA-P02-UKEID
      *            各画面を初期編集する
                   MOVE    1                   TO  SPA-P02-SYORI
                   PERFORM 310-SPA-SET-SEC
               ELSE
      *            選択なし
                   IF     (WRK-SPA-PTNUM       =   SPACE  )  AND
                          (SPA-GMN-PTID        =   ZERO   )
                       PERFORM 30011-PTNUM-CHK-SEC
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-GMN-PTNUM       =   SPACE
               MOVE    ZERO                TO  SPA-GMN-CUR
           END-IF
           .
       3001-P97SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    未選択時、患者番号チェック処理
      *****************************************************************
       30011-PTNUM-CHK-SEC              SECTION.
      *
           INITIALIZE                      ORCSPTNUMAREA
           MOVE    SPA-GMN-PTNUM       TO  SPTNUM-PTNUM
           CALL    "ORCSPTNUM"         USING   ORCSPTNUMAREA
                                               SPA-AREA
           MOVE    SPTNUM-PTNUM        TO  SPA-GMN-PTNUM
           EVALUATE    SPTNUM-RC
               WHEN    00
      *            患者あり
                   CONTINUE
               WHEN    20
      *            患者検索へ
                   MOVE    ZERO                TO  SPA-P02-SYORI
                   MOVE    SPTNUM-PTNUM        TO  SPA-GMN-PTNUM
               WHEN    10
      *            新規患者検索へ
                   MOVE    SPTNUM-PTNUM        TO  SPA-GMN-PTNUM
                   IF      SPTNUM-SYUBETU       =   1
                       CONTINUE
                   ELSE
                       MOVE    ZERO                TO  SPA-P02-SYORI
                   END-IF
               WHEN    OTHER
      *            エラー
                   MOVE    ZERO                TO  SPA-P02-SYORI
           END-EVALUATE
           .
       30011-PTNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面より処理
      *****************************************************************
       3001-PID1-SYORI-SEC              SECTION.
      *
           EVALUATE    SPA-PIDCD
      *        戻る確認
               WHEN    "0101"
                   IF      SPA-PID1-FLG        =   "OK"
                       MOVE    SPACE           TO  SPA-UPDFLG
                       PERFORM 210-BACK-SYORI-SEC
                   ELSE
                       MOVE    "1"             TO  SPA-UPDFLG
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
      *        削除確認
               WHEN    "0103"
                   MOVE    SPACE               TO  SPA-PIDCD
                   IF      SPA-PID1-FLG        =   "OK"
      *H24.9
      ***************  PERFORM 4991-DELETE-SYORI-SEC
                       PERFORM 4990-DELETE-CHEK-SEC
                   END-IF
                   IF      SPA-PIDCD       NOT =   SPACE
                       GO      TO      3001-PID1-SYORI-EXT
                   END-IF
      *        削除再確認
               WHEN    "0113"
                   IF      SPA-PID1-FLG        =   "OK"
                       PERFORM 4991-DELETE-SYORI-SEC
                   END-IF
      *        保険変更確認
      *******  WHEN    "9100"
      *        WHEN    "9101"
      *        WHEN    "9102"
      *            IF      SPA-PID1-FLG        =   "OK"
      *                PERFORM  49021-KOUSIN-AFTER-SEC
      *            ELSE
      *                PERFORM  49021-KOUSIN-AFTER-SEC
      ***********  END-IF
      *        保険変更確認
               WHEN    "K910"
               WHEN    "K911"
               WHEN    "K912"
                   IF      SPA-PID1-FLG        =   "OK"
                       MOVE    SPACE               TO  SPA-PIDCD
                       PERFORM 4900-TOROKU-SYORI2-SEC
                   ELSE
                       MOVE    ZERO            TO  SPA-KEI2-FLG
                   END-IF
      *        保険重複確認
               WHEN    "0105"
               WHEN    "0104"
               WHEN    "0110"
               WHEN    "0111"
               WHEN    "0106"
               WHEN    "0107"
      *        保険変換確認
               WHEN    "0201"
               WHEN    "0202"
               WHEN    "0203"
               WHEN    "0204"
                   IF      SPA-PID1-FLG        =   "OK"
                       MOVE    "P02 "          TO  SPA-P02-GAMEN
                       PERFORM 3002-GMNSET-SEC
                   END-IF
      *VER.4.7
      *        旧姓履歴確認
               WHEN    "0250"
                       MOVE    "P02 "          TO  SPA-P02-GAMEN
                       PERFORM 3002-GMNSET-SEC
      ***
      *        住所確認
               WHEN    "1098"
                   IF      SPA-PID1-FLG        =   "OK"
                       EVALUATE    SPA-GMN21-INKBN
                       WHEN    1
                           MOVE    SPA-GMN982-TOWNNAME
                                            TO  SPA-GMN21-RENRAKU-ADRS
                           MOVE    SPA-GMN21-RENRAKU-ADRS
                                               TO  SPA-MAE-RENRAKU-ADRS
                       WHEN    2
                           MOVE    SPA-GMN982-TOWNNAME
                                               TO  SPA-GMN21-OFFICE-ADRS
                           MOVE    SPA-GMN21-OFFICE-NAME
                                               TO  SPA-MAE-OFFICE-ADRS
                       WHEN    3
                           MOVE    SPA-GMN982-TOWNNAME
                                               TO  SPA-GMN21-KISEI-ADRS
                           MOVE    SPA-GMN21-KISEI-ADRS
                                               TO  SPA-MAE-KISEI-ADRS
                       WHEN    OTHER
                           MOVE    "P02 "          TO  SPA-P02-GAMEN
                           PERFORM 3002-GMNSET-SEC
                       END-EVALUATE
                   END-IF
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-PIDCD
      *
           .
       3001-PID1-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移各画面チェック処理
      *****************************************************************
       3002-GMNSET-SEC                 SECTION.
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
           EVALUATE    SPA-P02-GAMEN
      *       受付
               WHEN     "P020"
                   CALL    "ORCGP020"      USING   MCPAREA
                                                   SPAAREA
                                                   LINKAREA
                                                   SCRAREA
      *        基本
               WHEN     "P02 "
                   CALL    "ORCGP02W1"      USING   MCPAREA
                                                   SPAAREA
                                                   LINKAREA
                                                   SCRAREA
      *        保険組合せ
               WHEN     "P027"
                   CALL    "ORCGP027"      USING   MCPAREA
                                                   SPAAREA
                                                   LINKAREA
                                                   SCRAREA
      *        連絡先
               WHEN     "P021"
                   CALL    "ORCGP021"      USING   MCPAREA
                                                   SPAAREA
                                                   LINKAREA
                                                   SCRAREA
      *        所得者情報
               WHEN     "P024"
                   CALL    "ORCGP024"      USING   MCPAREA
                                                   SPAAREA
                                                   LINKAREA
                                                   SCRAREA
      *        各種履歴
               WHEN     "P025"
                   CALL    "ORCGP025"      USING   MCPAREA
                                                   SPAAREA
                                                   LINKAREA
                                                   SCRAREA
      *        特記事項情報
               WHEN     "P026"
                   CALL    "ORCGP026"      USING   MCPAREA
                                                   SPAAREA
                                                   LINKAREA
                                                   SCRAREA
      *        個別設定情報
               WHEN     "P028"
                   CALL    "ORCGP028"      USING   MCPAREA
                                                   SPAAREA
                                                   LINKAREA
                                                   SCRAREA
      ******   紹介情報入力
      *        WHEN     "P023"
      *            CALL    "ORCGP023"      USING   MCPAREA
      *                                            SPAAREA
      *                                            LINKAREA
      ****************                             SCRAREA
           END-EVALUATE
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-P02KYOUTU
           MOVE    SPA-FREE            TO  SPA-P02SCRAREA
           MOVE    MCP-WIDGET          TO  SPA-P02-WIDGET
           .
       3002-GMNSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPA-SET-SEC              SECTION.
      *
      *    パラメタファイル、患者情報のセット
           MOVE    "INIT"              TO  ORCSP02-SYORI
           CALL    "ORCSP02"           USING   ORCSP02AREA
                                               SPA-AREA
                                               SPA-P02KYOUTU
                                               SPA-P02SCRAREA
                                               P02
           MOVE    SPA-ERRCD           TO  WRK-ERRCD
      *
      *    ＱＲデータ判定
      *DD  PERFORM 3101-QRDATA-CHK-SEC
      *
      *    患者情報以外のセット
           IF      SPA-P02-SYORI       =   2
      *        画面ＳＰＡ編集のみ（各画面の初期編集）
               MOVE    "LINK"              TO  MCP-STATUS
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 49012-GMNSPA-SET-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    WRK-ERRCD       TO  SPA-ERRCD
           END-IF
           .
       310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＱＲデータ処理
      *****************************************************************
       3101-QRDATA-CHK-SEC              SECTION.
      *
           INITIALIZE                      QRCDHKN-REC
           MOVE    SPA-HOSPNUM          TO  QRCDHKN-HOSPNUM
           MOVE    QRCDHKN-REC         TO  MCPDATA-REC
           MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-QRCDHKN-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-QRCDHKN
               INITIALIZE                      QRCDHKN-REC
           END-IF
           MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-QRCDHKN         =   ZERO
               MOVE    1                   TO  SPA-QRFLG
           ELSE
               MOVE    ZERO                TO  SPA-QRFLG
           END-IF
           .
       3101-QRDATA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＬＡＭＥ画面よりの処理
      *****************************************************************
       3009-CLAIM-SYORI-SEC              SECTION.
      *
           IF      SPA-P02X-FLG        =  "OK"
               MOVE    1               TO  FLG-CLAIM
           ELSE
               MOVE    ZERO            TO  FLG-CLAIM
           END-IF
      *
           MOVE    ZERO                TO  FLG-CLAIM-MULTIHOST
      *    更新処理
           MOVE    1                   TO  FLG-CLAIM-OK
           PERFORM 4902-KOUSIN-SYORI-SEC
      *
           IF     (FLG-END             =   ZERO )  AND
                  (SPA-ERRCD       NOT =   SPACE)
               PERFORM 510-ERRSET-SEC
           END-IF
      *
           IF     (FLG-END             =   ZERO )  AND
                  (SPA-PIDCD       NOT =   SPACE)
               PERFORM 520-JIDSET-SEC
           END-IF
           .
       3009-CLAIM-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    ＱＲ受付データより処理
      *****************************************************************
       3010-QRDATA-SYORI-SEC              SECTION.
      *
           INITIALIZE                      QRCDHKN-REC
           MOVE    SPA-HOSPNUM          TO  QRCDHKN-HOSPNUM
           MOVE    SPA-QR-REGISTYMD    TO  QRCDHKN-REGISTYMD
           MOVE    SPA-QR-REGISTID     TO  QRCDHKN-REGISTID
           MOVE    QRCDHKN-REC         TO  MCPDATA-REC
           MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-QRCDHKN-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-QRCDHKN
               INITIALIZE                      QRCDHKN-REC
           END-IF
           MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           EVALUATE    SPA-QR-SYORI
               WHEN    "1"
      *            患者新規
                   PERFORM 30101-QRDATA-INS-SEC
               WHEN    "2"
               WHEN    "4"
      *            保険更新
                   MOVE    SPA-QR-PTNUM        TO  P02-PTNUM
                   MOVE    1                   TO  SPA-QR-FLG
      *            患者番号入力処理
                   PERFORM 4100-PTNUM-SYORI-SEC
      *            保険更新
                   PERFORM 30102-HKNUPD-SYORI-SEC
                   MOVE    ZERO                TO  SPA-QR-FLG
      *
               WHEN    "3"
      *            保険追加
                   MOVE    SPA-QR-PTNUM        TO  P02-PTNUM
                   MOVE    1                   TO  SPA-QR-FLG
      *            患者番号入力処理
                   PERFORM 4100-PTNUM-SYORI-SEC
                   MOVE    ZERO                TO  SPA-QR-FLG
      *            保険追加
                   PERFORM 30103-HKNINS-SYORI-SEC
           END-EVALUATE
      *
          .
       3010-QRDATA-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者新規処理
      *****************************************************************
       30101-QRDATA-INS-SEC          SECTION.
      *
           IF      SPA-GMN-PTNUM   NOT =   SPACE
               MOVE    ZERO                TO  SPA-GMN-PTID
               MOVE    SPACE               TO  SPA-GMN-PTNUM
      *        各画面を初期編集する
               MOVE    1                   TO  SPA-P02-SYORI
               PERFORM 310-SPA-SET-SEC
           END-IF
      *    各画面を初期編集する
           MOVE    ZERO                TO  SPA-GMN-PTID
           MOVE    SPACE               TO  SPA-GMN-PTNUM
           MOVE    2                   TO  SPA-QR-FLG
           MOVE    1                   TO  SPA-P02-SYORI
           MOVE    "P02 "              TO  SPA-P02-GAMEN
           MOVE    SPACE               TO  MCP-EVENT
           MOVE    SPACE               TO  MCP-WIDGET
           MOVE    SPACE               TO  MCP-STATUS
      *    ＱＲ情報
           MOVE    QRCDHKN-KANANAME    TO  P02-P02-KANANAME
           MOVE    QRCDHKN-NAME        TO  P02-P02-NAME
           IF      QRCDHKN-SEX         =   "1"
               MOVE    SPA-GMN-SEX-LST (1) TO  P02-P02-SEX
           ELSE
               MOVE    SPA-GMN-SEX-LST (2) TO  P02-P02-SEX
           END-IF
           MOVE    QRCDHKN-BIRTHDAY    TO  P02-P02-BIRTHDAY
           MOVE    QRCDHKN-HKNJANUM    TO  P02-P02-HKNJANUM
           MOVE    QRCDHKN-KIGO        TO  P02-P02-KIGO
           MOVE    QRCDHKN-NUM         TO  P02-P02-NUM
           MOVE    QRCDHKN-HONKZKKBN   TO  P02-P02-HONKZKBN
           MOVE    SPA-SYSYMDWH        TO  P02-P02-HKAKUNINYMD
      *
           MOVE    "P02 "              TO  SPA-P02-GAMEN
           PERFORM 3002-GMNSET-SEC
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           .
       30101-QRDATA-INS-EXT.
           EXIT.
      *****************************************************************
      *    ＱＲ保険更新処理
      *****************************************************************
       30102-HKNUPD-SYORI-SEC          SECTION.
      *
      *    保険更新
           IF     (SPA-QR-HKNID         =   SPA-NAI-HKNID )  AND
                  (SPA-GMN-HKNJANUM     =   QRCDHKN-HKNJANUM) AND
                  (SPA-GMN-HONKZKKBN    =   QRCDHKN-HONKZKKBN) AND
                  (SPA-GMN-KIGO         =   QRCDHKN-KIGO )     AND
                  (SPA-GMN-NUM          =   QRCDHKN-NUM  )
      *        確認日付変更
               MOVE    SPA-SYSYMDWH        TO  SPA-GMN-HKAKUNINYMD
      *        当日有効な場合、確認日付のみ変更
               IF     (SPA-NAI-HTEKSTYMD   <=  SPA-SYSYMD )
                  AND (SPA-NAI-HTEKEDYMD   >=  SPA-SYSYMD )
                   MOVE    19                  TO  SPA-GMN-CUR
                   MOVE    "P02_HKAKUNINYMD"   TO  MCP-WIDGET
               ELSE
      *            有効終了日
                   MOVE    SPACE               TO  SPA-GMN-HTEKEDYMD
                   MOVE    17                  TO  SPA-GMN-CUR
                   MOVE    "P02_HTEKEDYMD"     TO  MCP-WIDGET
               END-IF
      *        保険情報チェックサブ
               CALL    "ORCSP03A"      USING   SPA-AREA
                                               SPA-P02SCRAREA
                                               SPA-P02KYOUTU
               MOVE    MCP-WIDGET          TO  SPA-P02-WIDGET
           ELSE
      *        保険追加
               PERFORM 30103-HKNINS-SYORI-SEC
           END-IF
      *
           MOVE    "1"                 TO  SPA-UPDFLG
      *
           .
       30102-HKNUPD-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＱＲ保険追加処理
      *****************************************************************
       30103-HKNINS-SYORI-SEC          SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *    追加の判定
           INITIALIZE                  SPA-GMN-HKN-AREA
           MOVE    ZERO                TO  SPA-NAI-HKNSTR
           MOVE    8                   TO  SPA-GMN-CUR
      *
      *    保険情報のクリア
           MOVE    SPACE               TO  P02-P02-HKNNUM
           MOVE    SPACE               TO  P02-P02-HOJOKBN
           MOVE    SPACE               TO  P02-P02-CONTKBN
           MOVE    SPACE               TO  P02-P02-HTEKSTYMD
           MOVE    SPACE               TO  P02-P02-HTEKEDYMD
           MOVE    SPACE               TO  P02-P02-SKKGETYMD
           MOVE    SPACE               TO  P02-P02-HIHKNJANAME
      *    ＱＲ情報
           MOVE    QRCDHKN-HKNJANUM    TO  P02-P02-HKNJANUM
           MOVE    QRCDHKN-KIGO        TO  P02-P02-KIGO
           MOVE    QRCDHKN-NUM         TO  P02-P02-NUM
           MOVE    QRCDHKN-HONKZKKBN   TO  P02-P02-HONKZKBN
           MOVE    SPA-SYSYMDWH        TO  P02-P02-HKAKUNINYMD
      *
           MOVE    "P02 "              TO  SPA-P02-GAMEN
           MOVE    SPACE               TO  MCP-EVENT
           MOVE    "P02_HKNJANUM"      TO  MCP-WIDGET
           MOVE    SPACE               TO  MCP-WIDGET
           MOVE    SPACE               TO  MCP-STATUS
           PERFORM 3002-GMNSET-SEC
      *    資格取得日へ
           MOVE    15                  TO  SPA-GMN-CUR
           MOVE    "P02_SKKGETYMD"     TO  MCP-WIDGET
           MOVE    MCP-WIDGET          TO  SPA-P02-WIDGET
      *
           MOVE    "1"                 TO  SPA-UPDFLG
      *
          .
       30103-HKNINS-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       49012-GMNSPA-SET-SEC          SECTION.
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
      *    基本
           MOVE    MCPAREA             TO  WRK-MCPAREA
           CALL    "ORCGP02W1"      USING   WRK-MCPAREA
                                            SPAAREA
                                            LINKAREA
                                            SCRAREA
      *
      *    受付
           MOVE    MCPAREA             TO  WRK-MCPAREA
           CALL    "ORCGP020"      USING   WRK-MCPAREA
                                           SPAAREA
                                           LINKAREA
                                           SCRAREA
      *
      *    保険組合せ
           MOVE    MCPAREA             TO  WRK-MCPAREA
           CALL    "ORCGP027"      USING   WRK-MCPAREA
                                           SPAAREA
                                           LINKAREA
                                           SCRAREA
      *    連絡先
           MOVE    MCPAREA             TO  WRK-MCPAREA
           CALL    "ORCGP021"      USING   WRK-MCPAREA
                                           SPAAREA
                                           LINKAREA
                                           SCRAREA
      *    所得者情報
           MOVE    MCPAREA             TO  WRK-MCPAREA
           CALL    "ORCGP024"      USING   WRK-MCPAREA
                                           SPAAREA
                                           LINKAREA
                                           SCRAREA
      *    各種履歴
           MOVE    MCPAREA             TO  WRK-MCPAREA
           CALL    "ORCGP025"      USING   WRK-MCPAREA
                                           SPAAREA
                                           LINKAREA
                                           SCRAREA
      *    特記事項
           MOVE    MCPAREA             TO  WRK-MCPAREA
           CALL    "ORCGP026"      USING   WRK-MCPAREA
                                           SPAAREA
                                           LINKAREA
                                           SCRAREA
      *    紹介情報入力
      *    MOVE    MCPAREA             TO  WRK-MCPAREA
      *    CALL    "ORCGP023"      USING   WRK-MCPAREA
      *                                            SPAAREA
      *                                            LINKAREA
      ****                                         SCRAREA
      *
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-P02KYOUTU
           MOVE    SPA-FREE            TO  SPA-P02SCRAREA
      *
          .
       49012-GMNSPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE    "NO"                TO  SPA-ERRMSG
                   PERFORM 210-BACK
      *    クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 410-CLREA-SEC
      *    前回患者
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 420-ZENPTNUM-SEC
      *    削除
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 499-DELETE-SEC
      *    再発行
               WHEN    "CLICKED"       ALSO    "B05S"
                   PERFORM 450-SAINUM-SEC
      *    タグ切り替え（→）
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 430-BETUGAMEN-SEC
      *    タグ切り替え（←）
               WHEN    "CLICKED"       ALSO    "B08S"
                   PERFORM 430-BETUGAMEN2-SEC
      *    氏名検索（Ｐ９７）
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 470-SIMEIKEN-SEC
      *    予約登録へ
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 460-YOYAKU-SEC
      *    受付一覧へ
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 490-UKEICHI-SEC
      *    主科設定
               WHEN    "CLICKED"       ALSO    "B10S"
                   PERFORM 498-SYUKA-SEC
      *    複写処理
               WHEN    "CLICKED"       ALSO    "B11S"
                   PERFORM 499-COPY-SEC
      *    登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-TOUROKU-SEC
      ***  ＱＲ一覧へ
      *        WHEN    "CLICKED"       ALSO    "B12S"
      ***          PERFORM 4902-QRSYORI-SEC
      *    患者薬剤登録へ
               WHEN    "CLICKED"       ALSO    "B12S"
                   PERFORM 4902-P02YSYORI-SEC
      *    患者紐付け
               WHEN    "CLICKED"       ALSO    "B03S"
                   PERFORM 4032-GRPPTNUM-SEC
      *ver.4.7
      *    前・次切替え
               WHEN    "CLICKED"       ALSO    "B02S"
                   PERFORM 4034-NEXTCHG-SEC
      *ver.4.7
      *    タグ切り替え
      *        WHEN    "CLICKED"       ALSO    "N01"
      *        WHEN    "CLICKED"       ALSO    "N02"
      *        WHEN    "CLICKED"       ALSO    "N03"
      *        WHEN    "CLICKED"       ALSO    "N04"
      *********WHEN    "CLICKED"       ALSO    "N06"
      *        WHEN    "CLICKED"       ALSO    "N07"
      *        WHEN    "CLICKED"       ALSO    "N08"
      *        WHEN    "CLICKED"       ALSO    "N09"
      *****        PERFORM 460-TAGUCHG-SEC
      *    各画面へ
               WHEN    OTHER
                   MOVE    ZERO                TO  SPA-TAGU-FLG
      *            患者未設定
                   IF      SPA-GMN-PTNUM   NOT =   SPACE
                       PERFORM 3002-GMNSET-SEC
                   ELSE
      *                受付で未登録受付一覧は対象
                       IF     (SPA-P02-GAMEN   =   "P020") AND
                              (MCP-WIDGET      =   "BM1" )
                           PERFORM 3002-GMNSET-SEC
                       END-IF
                   END-IF
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    次画面　処理
      *****************************************************************
       430-BETUGAMEN-SEC               SECTION.
      *
           MOVE    SPA-P02-GAMEN       TO  WRK-P02-GAMEN
      *
      *    患者指定がない時、チェックなし
           IF      SPA-GMN-PTNUM       =   SPACE
               CONTINUE
           ELSE
      *
      *        表示画面の登録処理
               MOVE    "CLICKED"           TO  MCP-EVENT
               MOVE    "B12"               TO  MCP-WIDGET
               MOVE    "PUTG"              TO  MCP-STATUS
               MOVE    1                   TO  SPA-TAGU-FLG
               PERFORM 3002-GMNSET-SEC
               IF     (SPA-ERRCD       NOT =   SPACE )  OR
                      (SPA-FLG-END     NOT =   ZERO  )
                   GO  TO                  430-BETUGAMEN-EXT
               END-IF
      *
           END-IF
      *
           EVALUATE    WRK-P02-GAMEN
               WHEN    "P020"
                   MOVE    "P02 "          TO  SPA-P02-GAMEN
                   MOVE    "N02"           TO  SPA-P02-WIDGET
               WHEN    "P02"
                   MOVE    "P027"          TO  SPA-P02-GAMEN
                   MOVE    "N03"           TO  SPA-P02-WIDGET
               WHEN    "P027"
                   MOVE    "P021"          TO  SPA-P02-GAMEN
                   MOVE    "N04"           TO  SPA-P02-WIDGET
               WHEN    "P021"
                   MOVE    "P024"          TO  SPA-P02-GAMEN
                   MOVE    "N07"           TO  SPA-P02-WIDGET
      ***      WHEN    "P022"
      *++          MOVE    "P023"          TO  SPA-P02-GAMEN
      *++          MOVE    "N06"           TO  SPA-P02-WIDGET
               WHEN    "P024"
                   MOVE    "P025"          TO  SPA-P02-GAMEN
                   MOVE    "N08"           TO  SPA-P02-WIDGET
               WHEN    "P025"
                   MOVE    "P026"          TO  SPA-P02-GAMEN
                   MOVE    "N09"           TO  SPA-P02-WIDGET
               WHEN    "P026"
      **           MOVE    "P020"          TO  SPA-P02-GAMEN
      **           MOVE    "N01"           TO  SPA-P02-WIDGET
                   MOVE    "P028"          TO  SPA-P02-GAMEN
                   MOVE    "N10"           TO  SPA-P02-WIDGET
               WHEN    "P028"
                   MOVE    "P020"          TO  SPA-P02-GAMEN
                   MOVE    "N01"           TO  SPA-P02-WIDGET
           END-EVALUATE
      *
           MOVE    SPA-P02-WIDGET     TO  MCP-WIDGET
      *    画面ＳＰＡ編集のみ（次の画面の初期編集）
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    ZERO                TO  SPA-TAGU-FLG
           MOVE    SPACE               TO  SPA-MOTOPG
           PERFORM 3002-GMNSET-SEC
           MOVE    SPACE              TO  SPA-P02-WIDGET
           MOVE    ZERO               TO  SPA-FLG-END
      *
           .
       430-BETUGAMEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    前画面　処理
      *****************************************************************
       430-BETUGAMEN2-SEC               SECTION.
      *
           MOVE    SPA-P02-GAMEN       TO  WRK-P02-GAMEN
      *    患者指定がない時、チェックなし
           IF      SPA-GMN-PTNUM       =   SPACE
               CONTINUE
           ELSE
      *
      *        表示画面の登録処理
               MOVE    "CLICKED"           TO  MCP-EVENT
               MOVE    "B12"               TO  MCP-WIDGET
               MOVE    "PUTG"              TO  MCP-STATUS
               MOVE    1                   TO  SPA-TAGU-FLG
               PERFORM 3002-GMNSET-SEC
               IF     (SPA-ERRCD       NOT =   SPACE )  OR
                      (SPA-FLG-END     NOT =   ZERO  )
                   GO  TO                  430-BETUGAMEN2-EXT
               END-IF
      *
           END-IF
      *
           EVALUATE    WRK-P02-GAMEN
               WHEN    "P020"
      ****         MOVE    "P025"          TO  SPA-P02-GAMEN
      ****         MOVE    "N08"           TO  SPA-P02-WIDGET
      ****         MOVE    "P026"          TO  SPA-P02-GAMEN
      ****         MOVE    "N09"           TO  SPA-P02-WIDGET
                   MOVE    "P028"          TO  SPA-P02-GAMEN
                   MOVE    "N10"           TO  SPA-P02-WIDGET
               WHEN    "P02"
                   MOVE    "P020"          TO  SPA-P02-GAMEN
                   MOVE    "N01"           TO  SPA-P02-WIDGET
               WHEN    "P027"
                   MOVE    "P02 "          TO  SPA-P02-GAMEN
                   MOVE    "N02"           TO  SPA-P02-WIDGET
               WHEN    "P021"
                   MOVE    "P027"          TO  SPA-P02-GAMEN
                   MOVE    "N03"           TO  SPA-P02-WIDGET
               WHEN    "P024"
                   MOVE    "P021"          TO  SPA-P02-GAMEN
                   MOVE    "N04"           TO  SPA-P02-WIDGET
               WHEN    "P025"
                   MOVE    "P024"          TO  SPA-P02-GAMEN
                   MOVE    "N07"           TO  SPA-P02-WIDGET
               WHEN    "P026"
                   MOVE    "P025"          TO  SPA-P02-GAMEN
                   MOVE    "N08"           TO  SPA-P02-WIDGET
               WHEN    "P028"
                   MOVE    "P026"          TO  SPA-P02-GAMEN
                   MOVE    "N09"           TO  SPA-P02-WIDGET
           END-EVALUATE
      *
           MOVE    SPA-P02-WIDGET     TO  MCP-WIDGET
      *    画面ＳＰＡ編集のみ（各画面の初期編集）
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    ZERO                TO  SPA-TAGU-FLG
           MOVE    SPACE               TO  SPA-MOTOPG
           PERFORM 3002-GMNSET-SEC
           MOVE    SPACE              TO  SPA-P02-WIDGET
           MOVE    ZERO               TO  SPA-FLG-END
      *
           .
       430-BETUGAMEN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    タグ切り替え処理
      *****************************************************************
       460-TAGUCHG-SEC           SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-WIDGET
      *ver.4.7
           EVALUATE    P02-PAGENO
               WHEN    0
                   MOVE    "N01"               TO  WRK-WIDGET
               WHEN    1
                   MOVE    "N02"               TO  WRK-WIDGET
               WHEN    2
                   MOVE    "N03"               TO  WRK-WIDGET
               WHEN    3
                   MOVE    "N04"               TO  WRK-WIDGET
               WHEN    4
                   MOVE    "N07"               TO  WRK-WIDGET
               WHEN    5
                   MOVE    "N08"               TO  WRK-WIDGET
               WHEN    6
                   MOVE    "N09"               TO  WRK-WIDGET
               WHEN    7
                   MOVE    "N10"               TO  WRK-WIDGET
           END-EVALUATE
      *
      *    患者指定がない時、チェックなし
           IF      SPA-GMN-PTNUM       =   SPACE
               CONTINUE
           ELSE
      *        表示画面の登録処理
               MOVE    "CLICKED"           TO  MCP-EVENT
               MOVE    "B12"               TO  MCP-WIDGET
               MOVE    "PUTG"              TO  MCP-STATUS
               MOVE    1                   TO  SPA-TAGU-FLG
               PERFORM 3002-GMNSET-SEC
               IF     (SPA-ERRCD       NOT =   SPACE )  OR
                      (SPA-PIDCD       NOT =   SPACE )  OR
                      (SPA-FLG-END     NOT =   ZERO  )
                  GO  TO                  460-TAGUCHG-EXT
               END-IF
           END-IF
      *
           EVALUATE    WRK-WIDGET
      *        受付
               WHEN    "N01"
                   MOVE    "P020"          TO  SPA-P02-GAMEN
      *        基本
               WHEN    "N02"
                   MOVE    "P02 "          TO  SPA-P02-GAMEN
      *        保険組合せ
               WHEN    "N03"
                   MOVE    "P027"         TO  SPA-P02-GAMEN
      *        連絡先
               WHEN    "N04"
                   MOVE    "P021"         TO  SPA-P02-GAMEN
      *        禁忌
      *********WHEN    "N05"
      *            MOVE    "P022"         TO  SPA-P02-GAMEN
      *        紹介
      **       WHEN    "N06"
      *++             MOVE    "P023"         TO  SPA-P02-GAMEN
      *        履歴
               WHEN    "N07" 
                   MOVE    "P024"          TO  SPA-P02-GAMEN
      *        旧姓
               WHEN    "N08"
                   MOVE    "P025"          TO  SPA-P02-GAMEN
      *        特記事項
               WHEN    "N09"
                   MOVE    "P026"          TO  SPA-P02-GAMEN
      *        個別設定
               WHEN    "N10"
                   MOVE    "P028"          TO  SPA-P02-GAMEN
           END-EVALUATE
      *
      *    画面ＳＰＡ編集のみ（各画面の初期編集）
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    ZERO                TO  SPA-TAGU-FLG
           MOVE    SPACE               TO  SPA-MOTOPG
           PERFORM 3002-GMNSET-SEC
      *
           MOVE    SPACE              TO  SPA-P02-WIDGET
           MOVE    ZERO               TO  SPA-FLG-END
           MOVE    ZERO               TO  FLG-END
      *
           .
       460-TAGUCHG-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        患者番号入力
               WHEN    "ACTIVATE"      ALSO    "PTNUM"
                   PERFORM 4100-PTNUM-SYORI-SEC
      *ver.4.7
      *    タグ切り替え
               WHEN    "SWITCH"       ALSO    "nb2"
                   PERFORM 460-TAGUCHG-SEC
      *        各画面へ
               WHEN    OTHER
                   IF     (SPA-GMN-PTNUM   NOT =   SPACE  )  AND
                          (SPA-P02-SYORI   NOT =   ZERO   )
                       MOVE    "1"             TO  SPA-UPDFLG
                       MOVE    ZERO            TO  SPA-TAGU-FLG
                       PERFORM 3002-GMNSET-SEC
                   ELSE
                       MOVE    "1001"              TO  SPA-ERRCD
                   END-IF
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号入力処理
      *****************************************************************
       4100-PTNUM-SYORI-SEC            SECTION.
      *
      *    排他制御解除処理
           IF      SPA-GMN-PTID    NOT =   ZERO
               PERFORM 990-LOCK-OUT-SEC
           END-IF
      *
           IF     (SPA-QR-FLG          =   ZERO  )
      *        ＱＲ解除
               PERFORM 9901-QRCDHKN-OUT-SEC
           END-IF
      *
           MOVE    ZERO                TO  SPA-GMN-CUR
           MOVE    ZERO                TO  SPA-P02-SYORI
           MOVE    P02-PTNUM           TO  SPA-GMN-PTNUM
      *
           IF      SPA-GMN-PTNUM(1:1)      =   "*"
               MOVE    ZERO                TO  SPA-GMN-PTID
      *        新規自動採番
               IF      SPA-1009-PTNUMKSIKBN    =   "1"
      *            自由構成
                   MOVE    "1002"              TO  SPA-ERRCD
               ELSE
                   MOVE    ZERO                TO  FLG-HAKKOU
                   PERFORM 41001-AUTONUM-SEC
               END-IF
               GO      TO     4100-PTNUM-SYORI-EXT
           END-IF
      *
           IF      SPA-GMN-PTNUM       =   SPACE
               MOVE    "1009"              TO  SPA-ERRCD
               GO      TO     4100-PTNUM-SYORI-EXT
           END-IF
      *
           INITIALIZE                      ORCSPTNUMAREA
           MOVE    SPA-GMN-PTNUM       TO  SPTNUM-PTNUM
           CALL    "ORCSPTNUM"         USING   ORCSPTNUMAREA
                                               SPA-AREA
           MOVE    SPTNUM-PTNUM        TO  SPA-GMN-PTNUM
           EVALUATE    SPTNUM-RC
               WHEN    00
      *            患者あり
                   MOVE    SPTNUM-PTID         TO  SPTID-PTID
                   MOVE    SPTNUM-PTNUM        TO  SPTID-PTNUM
                   IF     (SPA-P02-UKEID       NOT =   ZERO   )  AND
                          (SPA-PTNUM               =   SPACE  )  AND
                          (SPA-PTID                =   ZERO   )  AND
                          (SPA-NAME            NOT =   SPACE  )
                       MOVE    "1010"              TO  SPA-ERRCD
                   ELSE
                       IF      SPA-QR-FLG          =   2
                           MOVE    "1010"              TO  SPA-ERRCD
                       ELSE
                           PERFORM 41001-PTNUM-HENSYU-SEC
                       END-IF
                   END-IF
               WHEN    20
                   IF      SPA-QR-FLG          =   2
                       MOVE    "1010"              TO  SPA-ERRCD
                   ELSE
                   MOVE    SPTNUM-PTNUM        TO  SPA-GMN-PTNUM
      *            患者検索へ
                   MOVE    SPACE               TO  LINKAREA
                   MOVE    SPA-KYOUTU          TO  LNK-COMMON
                   MOVE    SPA-P02SCRAREA      TO  LNK-SCRSPA
      *            画面移行用のパラメタ変更
                   MOVE    "P02"               TO  SPA-MOTOPG
                   MOVE    "P97"               TO  SPA-SAKIPG
      *            氏名セット
                   MOVE    SPA-GMN-PTNUM       TO  SPA-NAME
                   MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
                   MOVE    "CHANGE"            TO  MCP-PUTTYPE
                   MOVE    "P97"               TO  MCP-WINDOW
                   PERFORM 900-PUT-WINDOW
                   MOVE    1                   TO  FLG-END
      *
                   END-IF
               WHEN    10
      *            新規患者検索へ
                   IF      SPTNUM-SYUBETU       =   1
                       PERFORM 41002-PTNUMCHK-SEC
                   ELSE
                       MOVE    "1003"              TO  SPA-ERRCD
                   END-IF
               WHEN    OTHER
      *            エラー
                   MOVE    "1003"              TO  SPA-ERRCD
           END-EVALUATE
           .
       4100-PTNUM-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号決定処理
      *****************************************************************
       41001-PTNUM-HENSYU-SEC            SECTION.
      *
      *    患者マスターあり
           INITIALIZE                  SPA-P02SCRAREA
           MOVE    SPTID-PTNUM         TO  SPA-GMN-PTNUM
           MOVE    SPTID-PTID          TO  SPA-GMN-PTID
      *    患者マスター存在チェック
           MOVE    SPA-HOSPNUM          TO  PTINF-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTINF-PTID
           PERFORM  900-PTINF-READ-SEC
           IF      FLG-PTINF       NOT =   ZERO
               MOVE    SPACE               TO  SPA-GMN-NAME
               MOVE    "1003"              TO  SPA-ERRCD
           ELSE
               MOVE    "P02 "              TO  SPA-P02-GAMEN
           END-IF
      *
      *    排他制御処理
           IF      SPA-ERRCD           =   SPACE
               PERFORM 990-LOCK-IN-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPA-ERRCD           TO  WRK-ERRCD
               MOVE    2                   TO  SPA-P02-SYORI
               MOVE    1                   TO  SPA-GMN-CUR
               PERFORM 310-SPA-SET-SEC
               MOVE    SPACE               TO  SPA-P02-WIDGET
      *
      *        前回患者
               MOVE    SPA-GMN-PTNUM       TO  SPA-LAST-PTNUM
               MOVE    SPA-GMN-PTID        TO  SPA-LAST-PTID
               MOVE    WRK-ERRCD           TO  SPA-ERRCD
           END-IF
      *
           .
       41001-PTNUM-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    患者番号入力チェック処理
      *****************************************************************
       41002-PTNUMCHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-PTID
      *
           IF      SPTNUM-PTNUM    NOT =   SPACE
               MOVE    SPTNUM-PTNUM        TO  SPA-GMN-PTNUM
           END-IF
      *
      *    桁数チェック
           MOVE    ZERO                TO  WRK-KETA
           MOVE    ZERO                TO  FLG-KETA
           PERFORM VARYING     IDX     FROM    20  BY  -1
                   UNTIL       IDX         <   1
               IF      SPA-GMN-PTNUM(IDX:1)    NOT =   SPACE
                   MOVE    IDX                 TO  WRK-KETA
                   MOVE    1                   TO  IDX
               END-IF
           END-PERFORM
      *
           IF      WRK-KETA            =   ZERO
               MOVE    "1005"              TO  SPA-ERRCD
           END-IF
      *
      *    間に空白がある時エラー
           MOVE    ZERO                TO  FLG-KETA
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX         >   WRK-KETA
               IF      SPA-GMN-PTNUM(IDX:1)    =   SPACE
                   IF      FLG-KETA            =   1
                       MOVE    "1005"          TO  SPA-ERRCD
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-KETA
               END-IF
           END-PERFORM
      *
      *    患者構成チェック
           IF      SPA-ERRCD           =   SPACE
               INITIALIZE                  ORCSP01AREA
               MOVE    "2"                 TO  ORCSP01-SYORI
               MOVE    SPA-GMN-PTNUM       TO  ORCSP01-PTNUM
               MOVE    SPA-GMN-PTID        TO  ORCSP01-PTID
      *
               CALL    "ORCSP01"           USING
                                           ORCSP01AREA
                                           SPA-AREA
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPA-HZN-PTNUM       TO  WRK-HZN-PTNUM
               MOVE    SPA-HZN-MOTOPG      TO  WRK-HZN-MOTOPG
               IF      SPA-QR-FLG          =   2
                   MOVE    WRK-HZN-PTNUM       TO  SPA-HZN-PTNUM
                   MOVE    WRK-HZN-MOTOPG      TO  SPA-HZN-MOTOPG
                   MOVE    ORCSP01-PTNUM       TO  SPA-GMN-PTNUM
                   MOVE    ORCSP01-PTID        TO  SPA-GMN-PTID
                   MOVE    1                   TO  SPA-P02-SYORI
                   MOVE    "P02 "              TO  SPA-P02-GAMEN
                   MOVE    1                   TO  SPA-GMN-CUR
                   MOVE    SPACE               TO  SPA-P02-WIDGET
                   MOVE    ZERO                TO  SPA-QR-FLG
               ELSE
                   INITIALIZE                  SPA-P02KYOUTU
                   INITIALIZE                  SPA-P02SCRAREA
                   MOVE    WRK-HZN-PTNUM       TO  SPA-HZN-PTNUM
                   MOVE    WRK-HZN-MOTOPG      TO  SPA-HZN-MOTOPG
                   MOVE    ORCSP01-PTNUM       TO  SPA-GMN-PTNUM
                   MOVE    ORCSP01-PTID        TO  SPA-GMN-PTID
                   MOVE    1                   TO  SPA-P02-SYORI
                   MOVE    "P02 "              TO  SPA-P02-GAMEN
                   PERFORM 310-SPA-SET-SEC
                   MOVE    1                   TO  SPA-GMN-CUR
                   MOVE    SPACE               TO  SPA-P02-WIDGET
               END-IF
           END-IF
      *
           .
       41002-PTNUMCHK-EXT.
           EXIT.
      *****************************************************************
      *    患者番号自動連番処理
      *****************************************************************
       41001-AUTONUM-SEC           SECTION.
      *
      *
           EVALUATE    SPA-1009-PTNUMKSIKBN
      *    1:自由構成
               WHEN    "1"
                   CONTINUE
      *    2:標準構成
               WHEN    "2"
                   PERFORM 410011-AUTO-SYORI-SEC
      *    3:拡張構成
               WHEN    "3"
                   PERFORM 410012-P014-SET-SEC
           END-EVALUATE
      *
           .
       41001-AUTONUM-EXT.
           EXIT.
      *****************************************************************
      *    患者番号自動採番処理
      *****************************************************************
       410011-AUTO-SYORI-SEC           SECTION.
      *
           INITIALIZE                  ORCSP01AREA
           IF      FLG-HAKKOU          =   1
               MOVE    "3"                 TO  ORCSP01-SYORI
           ELSE
               MOVE    "1"                 TO  ORCSP01-SYORI
           END-IF
           MOVE    SPA-GMN-PTNUM       TO  ORCSP01-PTNUM
           MOVE    SPA-GMN-PTID        TO  ORCSP01-PTID
      *
           CALL    "ORCSP01"           USING
                                       ORCSP01AREA
                                       SPA-AREA
      *
           IF      SPA-ERRCD           =   SPACE  OR  "8000"
               MOVE    SPA-ERRCD           TO  WRK-ERRCD
               MOVE    SPACE               TO  SPA-ERRCD
      *
               MOVE    ORCSP01-PTNUM       TO  SPA-GMN-PTNUM
               MOVE    ORCSP01-PTID        TO  SPA-GMN-PTID
               MOVE    1                   TO  SPA-P02-SYORI
               MOVE    "P02 "              TO  SPA-P02-GAMEN
               IF      FLG-HAKKOU          =   ZERO
                  AND  SPA-QR-FLG          =   ZERO
                   PERFORM 310-SPA-SET-SEC
               ELSE
                   MOVE    1                   TO  P02-PAGENO
               END-IF
               MOVE    1                   TO  SPA-GMN-CUR
               MOVE    SPACE               TO  SPA-P02-WIDGET
               IF      SPA-ERRCD           =   SPACE
                   MOVE    WRK-ERRCD           TO  SPA-ERRCD
               END-IF
               IF      SPA-QR-FLG          =   2
                   MOVE    ZERO            TO  SPA-QR-FLG
               END-IF
           END-IF
      *
           .
       410011-AUTO-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    拡張番号編集画面へ（Ｐ０１４）
      *****************************************************************
       410012-P014-SET-SEC           SECTION.
      *
           INITIALIZE                  ORCSP01AREA
           IF      FLG-HAKKOU          =   1
               MOVE    "3"                 TO  ORCSP01-SYORI
           ELSE
               MOVE    "1"                 TO  ORCSP01-SYORI
           END-IF
           MOVE    SPA-GMN-PTNUM       TO  ORCSP01-PTNUM
           MOVE    SPA-GMN-PTID        TO  ORCSP01-PTID
      *
           CALL    "ORCSP01"           USING
                                       ORCSP01AREA
                                       SPA-AREA
      *
           IF      SPA-ERRCD           =   SPACE  OR  "8000"
               CONTINUE
           ELSE
               GO      TO      410012-P014-SET-EXT
           END-IF
           MOVE    SPA-ERRCD           TO  WRK-ERRCD
           MOVE    SPACE               TO  SPA-ERRCD
      *
           MOVE    ORCSP01-PTID        TO  SPA-GMN-PTID
           INITIALIZE                  SPA-P014-AREA
           MOVE    ORCSP01-PTNUM1      TO  SPA-GMN014-PTNUM1
           MOVE    ORCSP01-PTNUM2      TO  SPA-GMN014-PTNUM2
           MOVE    ORCSP01-PTNUM3      TO  SPA-GMN014-PTNUM3
      *
           MOVE    FLG-HAKKOU          TO  SPA-GMN014-SYORI
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP014"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON          TO  SPA-AREA.
           MOVE    SPA-FREE            TO  SPA-P02SCRAREA
           MOVE    SPA-WORK            TO  SPA-P02KYOUTU
      *
           MOVE    "P02"               TO  SPA-MOTOPG
           MOVE    "P014"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "P014"              TO  MCP-WINDOW.
      *    カーソル移動
           EVALUATE    SPA-1009-KKCKSIKBN
      *        拡張構成 <フリー + 連番号 + フリー>
               WHEN    "1"
                   MOVE    "PTNUM1"            TO  MCP-WIDGET
      *        拡張構成 <フリー + 連番号>
               WHEN    "2"
                   MOVE    "PTNUM1"            TO  MCP-WIDGET
      *        拡張構成 <連番号 + フリー>
               WHEN    "3"
                   MOVE    "PTNUM2"            TO  MCP-WIDGET
           END-EVALUATE
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       410012-P014-SET-EXT.
           EXIT.
      *****************************************************************
      *    クリア処理
      *****************************************************************
       410-CLREA-SEC           SECTION.
      *
      *    ＱＲ解除
           PERFORM 9901-QRCDHKN-OUT-SEC
      *
           MOVE    SPA-HZN-PTNUM       TO  WRK-HZN-PTNUM
           MOVE    SPA-HZN-MOTOPG      TO  WRK-HZN-MOTOPG
           INITIALIZE                  SPA-P02SCRAREA
           INITIALIZE                  SPA-P02KYOUTU
           MOVE    SPACE               TO  SPA-NAME
           MOVE    WRK-HZN-PTNUM       TO  SPA-HZN-PTNUM
           MOVE    WRK-HZN-MOTOPG      TO  SPA-HZN-MOTOPG
           IF     (SPA-HZN-MOTOPG      =   "U02" )  AND
                  (SPA-HZN-PTNUM       =   SPACE )
               CONTINUE
           ELSE
               MOVE    ZERO                TO  SPA-P02-UKEID
           END-IF
      *
           MOVE    "P02"               TO  SPA-P02-GAMEN
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           PERFORM 310-SPA-SET-SEC
      *
           .
       410-CLREA-EXT.
           EXIT.
      *****************************************************************
      *    前回患者処理
      *****************************************************************
       420-ZENPTNUM-SEC           SECTION.
      *
      *    ＱＲ解除
           PERFORM 9901-QRCDHKN-OUT-SEC
      *
           IF      SPA-LAST-PTNUM      NOT =   SPACE
               MOVE    SPA-LAST-PTNUM      TO  P02-PTNUM
               PERFORM 4100-PTNUM-SYORI-SEC
           END-IF
      *
           .
       420-ZENPTNUM-EXT.
           EXIT.
      *****************************************************************
      *    主科設定処理
      *****************************************************************
       498-SYUKA-SEC           SECTION.
      *
           IF      SPA-GMN-PTNUM       =   SPACE
               MOVE    "1021"              TO  SPA-ERRCD
           ELSE
      *        主科設定画面へ
               PERFORM 4981-P02S-SYORI-SEC
           END-IF
      *
           .
       498-SYUKA-EXT.
           EXIT.
      *****************************************************************
      *    主科設定画面処理
      *****************************************************************
       4981-P02S-SYORI-SEC            SECTION.
      *
           INITIALIZE                  SPA-P02S-AREA
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP02S"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-P02SCRAREA
           MOVE    SPA-WORK            TO  SPA-P02KYOUTU
      *
           MOVE    "P02"               TO  SPA-MOTOPG
           MOVE    "P02S"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "P02S"              TO  MCP-WINDOW
      *    カーソル移動
           MOVE    "SYUKA"             TO  MCP-WIDGET
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4981-P02S-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    患者紐付け処理
      *****************************************************************
       4032-GRPPTNUM-SEC           SECTION.
      *
           IF      SPA-GMN-PTNUM       =   SPACE
               MOVE    "1021"              TO  SPA-ERRCD
           ELSE
      *        患者紐付け設定画面へ
               PERFORM 40321-P02G2-SYORI-SEC
           END-IF
      *
           .
       4032-GRPPTNUM-EXT.
           EXIT.
      *****************************************************************
      *    患者紐付け設定画面処理
      *****************************************************************
       40321-P02G2-SYORI-SEC            SECTION.
      *
           INITIALIZE                  SPA-P02S-AREA
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP02G2"         USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-P02SCRAREA
           MOVE    SPA-WORK            TO  SPA-P02KYOUTU
      *
      *    カーソル移動
           IF      SPA-P02G2-MAX       >   ZERO
               MOVE    "B01"               TO  MCP-WIDGET
           ELSE
               MOVE    "PTNUM"             TO  MCP-WIDGET
           END-IF
      *
           MOVE    "P02"               TO  SPA-MOTOPG
           MOVE    "P02G2"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "P02G2"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       40321-P02G2-SYORI-EXT.
           EXIT.
      *ver.4.7
      *
      *****************************************************************
      *    前・次切替え処理
      *****************************************************************
       4034-NEXTCHG-SEC            SECTION.
      *
           EVALUATE    SPA-NAI-NEXTFLG
               WHEN    0
                   MOVE    1           TO  SPA-NAI-NEXTFLG
               WHEN    1
                   MOVE    2           TO  SPA-NAI-NEXTFLG
               WHEN    2
                   MOVE    0           TO  SPA-NAI-NEXTFLG
           END-EVALUATE
           .
       4034-NEXTCHG-EXT.
           EXIT.
      *****************************************************************
      *    ＱＲ一覧へ処理
      *****************************************************************
       4902-QRSYORI-SEC            SECTION.
      *
           IF     (SPA-QR-REGISTYMD   NOT =   SPACE )  AND
                  (SPA-QR-REGISTID    NOT =   ZERO  )
      *        ＱＲ展開中
               MOVE    "1024"             TO  SPA-ERRCD
           ELSE
               PERFORM 49021-P02Q-SYORI-SEC
           END-IF
      *
           .
       4902-QRSYORI-EXT.
           EXIT.
      *****************************************************************
      *    ＱＲ一覧へ処理
      *****************************************************************
       49021-P02Q-SYORI-SEC            SECTION.
      *
           INITIALIZE                  SPA-P02Q-AREA
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP02Q"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-P02SCRAREA
           MOVE    SPA-WORK            TO  SPA-P02KYOUTU
      *
           IF      SPA-P02Q-MAX        =   ZERO
               MOVE    "B05"               TO  MCP-WIDGET
           ELSE
               MOVE    "SELNUM"            TO  MCP-WIDGET
           END-IF
           MOVE    "P02"               TO  SPA-MOTOPG
           MOVE    "P02Q"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "P02Q"              TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                   TO  FLG-END
           .
       49021-P02Q-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    患者禁忌薬剤登録へ処理
      *****************************************************************
       4902-P02YSYORI-SEC            SECTION.
      *
           IF     (SPA-P02-SYORI       =   1     )
      *        登録不可
               MOVE    "1025"              TO  SPA-ERRCD
           ELSE
               IF     (SPA-GMN-PTID        =   ZERO  )
                   MOVE    "1021"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        表示画面の登録処理
               MOVE    "CLICKED"           TO  MCP-EVENT
               MOVE    "B12"               TO  MCP-WIDGET
               MOVE    "PUTG"              TO  MCP-STATUS
               MOVE    1                   TO  SPA-TAGU-FLG
               PERFORM 3002-GMNSET-SEC
               IF     (SPA-ERRCD       NOT =   SPACE )
                  OR  (SPA-PIDCD       NOT =   SPACE )
                  GO  TO                  4902-P02YSYORI-EXT
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 49021-P02Y-SYORI-SEC
           END-IF
           .
       4902-P02YSYORI-EXT.
           EXIT.
      *****************************************************************
      *    禁忌薬剤一覧へ処理
      *****************************************************************
       49021-P02Y-SYORI-SEC            SECTION.
      *
           INITIALIZE                  SPA-P02Y-AREA
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP02Y"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-P02SCRAREA
           MOVE    SPA-WORK            TO  SPA-P02KYOUTU
      *
      *
           IF      SPA-P02Y-MAX        =   ZERO
               MOVE    "SRYCD"             TO  MCP-WIDGET
           ELSE
               MOVE    "SELNUM"            TO  MCP-WIDGET
           END-IF
           MOVE    "P02"               TO  SPA-MOTOPG
           MOVE    "P02Y"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "P02Y"              TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                   TO  FLG-END
           .
       49021-P02Y-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    再発行処理
      *****************************************************************
       450-SAINUM-SEC           SECTION.
      *
      *    患者ＩＤがあり、新規の時
           IF     (SPA-GMN-PTNUM   NOT =   SPACE)  AND
                  (SPA-P02-SYORI       =   1    )
      *        自由構成の時、登録
               MOVE    1                   TO  FLG-HAKKOU
               PERFORM 41001-AUTONUM-SEC
           ELSE
               IF     (SPA-GMN-PTNUM   NOT =   SPACE ) AND
                      (SPA-GMN-PTID    NOT =   ZERO  ) AND
                      (SPA-P02-SYORI       =   2     )
                   PERFORM 4501-P015-SYORI-SEC
               ELSE
                   MOVE    "1004"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       450-SAINUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号再設定画面処理
      *****************************************************************
       4501-P015-SYORI-SEC            SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRCD
      *    他から患者選択での遷移時はできない
           EVALUATE    SPA-HZN-MOTOPG 
               WHEN    "K02"
               WHEN    "K02N"
               WHEN    "U02"
               WHEN    "Q02"
               WHEN    "I01"
                   IF     (SPA-HZN-PTNUM   NOT =   SPACE  )  AND
                          (SPA-HZN-PTNUM       =   SPA-GMN-PTNUM )
                       MOVE    "1012"              TO  SPA-ERRCD
                   END-IF
           END-EVALUATE
           IF      SPA-ERRCD           =   SPACE
               PERFORM 45011-P015-SET-SEC
           END-IF
      *
           .
       4501-P015-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    患者番号再設定画面処理
      *****************************************************************
       45011-P015-SET-SEC            SECTION.
      *
           INITIALIZE                  SPA-P015-AREA
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP015"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-P02SCRAREA
           MOVE    SPA-WORK            TO  SPA-P02KYOUTU
      *
           MOVE    "P02"               TO  SPA-MOTOPG
           MOVE    "P015"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "P015"              TO  MCP-WINDOW
      *    カーソル移動
           MOVE    "PTNUM2"            TO  MCP-WIDGET
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       45011-P015-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *----(01.00.03) LINE ADD START ----------------------------------
           IF     (SPA-UPDFLG          =   "1" )
              AND (SPA-LOCK            =   ZERO)
               MOVE    "0101"          TO  SPA-PIDCD
               GO      TO              210-BACK-EXT
           END-IF
      *----(01.00.03) LINE ADD END   ----------------------------------
      *
      *    ＱＲ解除
           PERFORM 9901-QRCDHKN-OUT-SEC
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
      *    パラメタファイル削除
           PERFORM 992-PARA-DEL-SEC
      *
           IF      SPA-HZN-MOTOPG(1:3) =   "U02"  OR  "K02" OR "Q02"
                                                  OR  "I01"
               IF      SPA-HZN-PTNUM       =   SPACE
                   MOVE    SPACE               TO  SPA-PTNUM
                   MOVE    ZERO                TO  SPA-PTID
               END-IF
               MOVE    "P02"               TO  SPA-MOTOPG
               MOVE    SPACE               TO  SPA-MOTOPG2
               MOVE    SPA-HZN-MOTOPG      TO  SPA-SAKIPG
      *
               MOVE    SPACE               TO  LINKAREA
               MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
           ELSE
               MOVE    "P02"               TO  SPA-MOTOPG
               MOVE    SPACE               TO  SPA-MOTOPG2
               MOVE    "M01"               TO  SPA-SAKIPG
      *
               MOVE    SPACE               TO  LINKAREA
               INITIALIZE                      SPA-KYOTUKEY
               MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
           END-IF
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  SPA-FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK-SYORI-SEC               SECTION.
      *
      *    ＱＲ解除
           PERFORM 9901-QRCDHKN-OUT-SEC
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
      *    パラメタファイル削除
           PERFORM 992-PARA-DEL-SEC
      *
           IF      SPA-HZN-MOTOPG(1:3) =   "U02"  OR  "K02" OR "Q02"
                                                  OR  "I01"
      *********MOVE    SPA-MACHIPG         TO  SPA-SAKIPG
               MOVE    SPA-HZN-MOTOPG      TO  SPA-SAKIPG
               MOVE    "P02"               TO  SPA-MOTOPG
               MOVE    SPACE               TO  SPA-MOTOPG2
               MOVE    SPACE               TO  SPA-ERRMSG
      *
               MOVE    SPACE               TO  LINKAREA
               MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
           ELSE
               MOVE    "P02"               TO  SPA-MOTOPG
               MOVE    SPACE               TO  SPA-MOTOPG2
      *
               MOVE    "M01"               TO  SPA-SAKIPG
               MOVE    SPACE               TO  LINKAREA
               MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
           END-IF
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  SPA-FLG-END
      *
           .
       210-BACK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約登録へ
      *****************************************************************
       460-YOYAKU-SEC             SECTION.
      *
      *    戻り時、画面を元へ戻す
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-P02SCRAREA      TO  LNK-SCRSPA
      *
           INITIALIZE                  SPA-KYOTUKEY
      *    画面移行用のパラメタ変更
           MOVE    "P02"               TO  SPA-MOTOPG
           MOVE    "Y01"               TO  SPA-SAKIPG
           MOVE    "P02"               TO  SPA-MOTOPG3
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "Y01    "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       460-YOYAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付一覧へ
      *****************************************************************
       490-UKEICHI-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-P02SCRAREA      TO  LNK-SCRSPA
      *    画面移行用のパラメタ変更
           MOVE    "P02"               TO  SPA-MOTOPG
           MOVE    "U01"               TO  SPA-SAKIPG
      *
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "U01    "            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-UKEICHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    氏名検索へ
      *****************************************************************
       470-SIMEIKEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-P02SCRAREA      TO  LNK-SCRSPA
      *    画面移行用のパラメタ変更
           MOVE    "P02"               TO  SPA-MOTOPG
           MOVE    "P97"               TO  SPA-SAKIPG
      *
      *    氏名をクリアする
           MOVE    SPACE               TO  SPA-NAME
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "P97"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       470-SIMEIKEN-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       499-DELETE-SEC             SECTION.
      *
           IF     (SPA-P02-SYORI       =   2      )   AND
                  (SPA-GMN-PTID    NOT =   ZERO   )
               MOVE    "0103"              TO  SPA-PIDCD
           END-IF
      *
           .
       499-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       4990-DELETE-CHEK-SEC             SECTION.
      *
      *    テスト患者
           IF      SPA-GMN-TSTPTNUMKBN     =   "1"
      *        削除処理
               PERFORM 4991-DELETE-SYORI-SEC
               GO      TO      4990-DELETE-CHEK-EXT
           END-IF
      *
      *    受診履歴チェック
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               MOVE    SPACE               TO  JYURRK-REC
               INITIALIZE                  JYURRK-REC
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-JYURRK          =   ZERO
      *        再確認メッセージ
               MOVE    "0113"              TO  SPA-PIDCD
           ELSE
      *        削除処理
               PERFORM 4991-DELETE-SYORI-SEC
           END-IF
      *
           .
       4990-DELETE-CHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       4991-DELETE-SYORI-SEC             SECTION.
      *
      *    ファイル削除
           PERFORM 49911-DB-DELETE-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4991-DELETE-SYORI-EXT
           END-IF
      *
      *H29.7
      *PUSH通信
           IF      SPA-ERRCD           =   SPACE
               MOVE    3                   TO  FLG-PUSHSYORI
               PERFORM 600-PUSH-SYORI-SEC
           END-IF
      *
      *    ユーザプログラム起動
      *v4.8
      *    ginbee対応
      *    IF      SPA-MW              =   SPA-GINBEE
      *        CONTINUE
      ***  ELSE
               MOVE    2                   TO  FLG-USERPG
               PERFORM 4907-USERPG-SEC
      **** END-IF
           IF      FLG-END             =   1
               GO      TO      4991-DELETE-SYORI-EXT
           END-IF
      *
      *****IF      SPA-MACHIPG         =   "U02"  OR  "K02" OR "Q02"
           IF      SPA-HZN-MOTOPG(1:3) =   "U02"  OR  "K02" OR "Q02"
                                                  OR  "I01"
               IF      SPA-HZN-PTNUM       =   SPACE
                   CONTINUE
               ELSE
      *            更新後、元の画面へ
                   INITIALIZE                  SPA-KYOTUKEY
                   PERFORM 210-BACK-SYORI-SEC
                   GO      TO      4991-DELETE-SYORI-EXT
               END-IF
           END-IF
      *
      *    各画面を初期編集する
           MOVE    SPA-HZN-PTNUM       TO  WRK-HZN-PTNUM
           MOVE    SPA-HZN-MOTOPG      TO  WRK-HZN-MOTOPG
           INITIALIZE                  SPA-P02SCRAREA
           INITIALIZE                  SPA-P02KYOUTU
           MOVE    WRK-HZN-PTNUM       TO  SPA-HZN-PTNUM
           MOVE    WRK-HZN-MOTOPG      TO  SPA-HZN-MOTOPG
           MOVE    "P02"               TO  SPA-P02-GAMEN
           MOVE    ZERO                TO  SPA-GMN-CUR
           PERFORM 310-SPA-SET-SEC
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           .
      *
       4991-DELETE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       49911-DB-DELETE-SEC             SECTION.
      *
      *    患者番号削除処理
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
      *R01.10
           MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
           MOVE    SPA-GMN-NAME        TO  SPA-NAME
           CALL    "ORCSP05DL"         USING
                                       SPA-AREA
      *
      *
           .
       49911-DB-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       490-TOUROKU-SEC             SECTION.
      *
      **   画面の内容を保存＆更新前チェック
           PERFORM 4901-KOUSINMAE-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-PIDCD       NOT =   SPACE )  OR
                  (SPA-FLG-END     NOT =   ZERO  )
               GO  TO                  490-TOUROKU-EXT
           END-IF
      *
      *    保険組合せの変更による履歴チェック
           IF      SPA-P02-SYORI   NOT =   1
               IF      SPA-KEI2-FLG        =   ZERO
                   MOVE    SPA-P02KYOUTU   TO  SPA-WORK
                   MOVE    SPA-P02SCRAREA  TO  SPA-FREE
                   MOVE    SPA-AREA        TO  SPA-COMMON
      *            更新サブへ
                   CALL    "ORCGP029"      USING   MCPAREA
                                                   SPAAREA
                   MOVE    SPA-COMMON      TO  SPA-AREA
                   MOVE    SPA-WORK        TO  SPA-P02KYOUTU
                   MOVE    SPA-FREE        TO  SPA-P02SCRAREA
               END-IF
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )   AND
                  (SPA-PIDCD           =   SPACE )
               PERFORM 4900-TOROKU-SYORI2-SEC
           END-IF
           .
       490-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新処理
      *****************************************************************
       4900-TOROKU-SYORI2-SEC           SECTION.
      *
      *    ＣＬＡＩＭ接続チェック処理
           MOVE    ZERO                TO  FLG-CLAIM
      *v4.8
      *    ginbee対応
           IF      SPA-MW              =   SPA-GINBEE
               INITIALIZE                  SPA-P02X-AREA
           ELSE
               PERFORM 4905-CLAIM-CHK-SEC
           END-IF
      *
           IF      FLG-CLAIM           =   2
               CONTINUE
           ELSE
      *    更新処理
               MOVE    ZERO                TO  FLG-CLAIM-OK
               PERFORM 4902-KOUSIN-SYORI-SEC
           END-IF
           .
       4900-TOROKU-SYORI2-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新処理
      *****************************************************************
       4902-KOUSIN-SYORI-SEC           SECTION.
      *
           MOVE    SPA-P02KYOUTU   TO  SPA-WORK
           MOVE    SPA-P02SCRAREA  TO  SPA-FREE
           MOVE    SPA-AREA        TO  SPA-COMMON
      *    更新サブへ
           CALL    "ORCGP029"      USING   MCPAREA
                                           SPAAREA
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
      *
           IF     (SPA-ERRCD           =   SPACE )   AND
                  (SPA-PIDCD           =   SPACE )
               IF      SPA-KEI-ERRCD   NOT =   SPACE
      ****         MOVE    SPA-KEI-ERRCD       TO  SPA-PIDCD
                   MOVE    SPA-KEI-ERRCD       TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-KEI-FLG
               END-IF
           END-IF
      *!!!
      *    カルテ／処方箋発行処理（クライアント印刷の為、移動）
      **** IF     (SPA-ERRCD           =   SPACE )
           IF     (SPA-ERRCD      NOT  =   SPACE )
             AND  (SPA-KEI-FLG         =   ZERO  )
               CONTINUE
           ELSE
               PERFORM 49023-HC01-SYORI-SEC
           END-IF
      *!!!
      *    ＣＬＡＩＭ送信
           MOVE    FLG-CLAIM           TO  SPA-FLG-CLAIM
      *
      *    登録後、処理
           IF     (SPA-ERRCD           =   SPACE )   AND
                  (SPA-PIDCD           =   SPACE )   AND
                  (FLG-END             =   ZERO  )
               PERFORM  49021-KOUSIN-AFTER-SEC
           END-IF
           .
       4902-KOUSIN-SYORI-EXT.
           EXIT.
      *
      *ver.4.7
      *****************************************************************
      *    カルテ／処方箋発行処理
      *****************************************************************
       49023-HC01-SYORI-SEC         SECTION.
      *
      *    帳票印刷有無判定
           MOVE    ZERO                TO  FLG-PRT
           IF      SPA-GMN-HAKKO       =   "1"  OR  "2"
                                       OR  "3"
               MOVE    1               TO  FLG-PRT
           END-IF
           PERFORM   VARYING   IDX2    FROM    1   BY  1
                     UNTIL   ( IDX2            >   10 )
                     OR      ( FLG-PRT         =   1  )
                     OR      ( SPA-GMN20-YBANGOU(IDX2)
                                               =   ZERO  OR  SPACE )
               IF   (SPA-GMN20-TKARUTE (IDX2)      =   "0"
                                                   OR  SPACE  )  AND
                    (SPA-GMN20-TSYOHOU (IDX2)      =   "0"
                                                   OR  SPACE  )
                   CONTINUE
               ELSE
                   MOVE    1               TO  FLG-PRT
               END-IF
           END-PERFORM
           IF      FLG-PRT             =   ZERO
               GO      TO      49023-HC01-SYORI-EXT
           END-IF
      *
      *    ＳＰＡに共通項目を設定
           MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
           MOVE    "2"                 TO  SPA-NYUGAIKBN
      *
      *    帳票プログラム取得
           PERFORM 21000-PRINT-PG-SEC
      *
      *    印刷制御情報取得サブ
           CALL    "ORCSONPRTSET"      USING   SPA-AREA
      *    再印刷指示なし
           MOVE    2                   TO  SPA-CLIENT-TEMP-FLG
      *
           IF     (SPA-CLIENT-PRT-FLG  =   2 )
             OR   (SPA-PRTCONF         =   "1" )
      *        クライアント印刷制御情報取得処理
               PERFORM 4901-CLIENT-PRT-SEC
           END-IF
      *
      *    基本画面のカルテ出力
           IF      SPA-GMN-HAKKO       =   "1"  OR  "2"
                                       OR  "3"
               PERFORM 21003-HC01-CALL2-SEC
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM   VARYING   IDX2    FROM    1   BY  1
                     UNTIL   ( IDX2            >   10 )
                     OR      ( SPA-GMN20-YBANGOU(IDX2)
                                               =   ZERO  OR  SPACE )
      *        カルテ出力
               IF   (SPA-GMN20-TKARUTE (IDX2)      =   "1"  OR  "2"
                                                   OR  "3"  )
                   PERFORM 21001-HC01-CALL-SEC
               END-IF
      *        処方箋出力
               IF    SPA-GMN20-TSYOHOU (IDX2)      =   "1"
                   PERFORM 21001-HC02-CALL-SEC
               END-IF
           END-PERFORM
      *
           IF      (SPA-CLIENT-PRT-FLG =   1  )
      *        印刷ダミー処理
               MOVE    "99"                TO  SPA-PRT-FLG
               CALL    "ORCHCDUMMY"    USING   SPA-AREA
           END-IF
           MOVE    SPACE               TO  SPA-PRT-FLG
           MOVE    ZERO                TO  SPA-CLIENT-TEMP-FLG
      *
           .
       49023-HC01-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票プログラム取得処理
      *****************************************************************
       21000-PRINT-PG-SEC              SECTION.
      *
      *    カルテ
           MOVE    SPACE               TO  WRK-KARUTE-PG
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000001"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1032-REC
           MOVE    ORCSPRTNM-PRTPG     TO  WRK-KARUTE-PG
      *
      *    処方せん
           MOVE    SPACE               TO  WRK-SYOHOU-PG
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000002"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1032-REC
           MOVE    ORCSPRTNM-PRTPG     TO  WRK-SYOHOU-PG
      *
           IF      WRK-KARUTE-PG       =   SPACE
               MOVE    "ORCHC01"           TO  WRK-KARUTE-PG
           END-IF
      *
           IF      WRK-SYOHOU-PG       =   SPACE
               MOVE    "ORCHCM19"          TO  WRK-SYOHOU-PG
           END-IF
      *
           .
      *
       21000-PRINT-PG-EXT.
           EXIT.
      *****************************************************************
      *    クライアント印刷前　処理
      *****************************************************************
       4901-CLIENT-PRT-SEC         SECTION.
      *
      *    INITIALIZE                  PDMY01
      *    MOVE    SPACE               TO  PDMY01-PANDAPRINT1
      *
      *    クライアント印刷制御情報取得サブ
           INITIALIZE                  ORCSPRTCTRLAREA
      *    MOVE    "カルテ"            TO  ORCSPRTCTRL-TITLE-I(01)
      *    MOVE    "処方箋"            TO  ORCSPRTCTRL-TITLE-I(02)
           CALL    "ORCSPRTCTRL"       USING   SPA-AREA
                                               ORCSPRTCTRLAREA
                                               MCPAREA
      *    MOVE    ORCSPRTCTRL-OUT     TO  PDMY01-PANDAPRINT1
      *
           .
       4901-CLIENT-PRT-EXT.
           EXIT.
      *
      *****************************************************************
      *    クライアント印刷画面処理
      *****************************************************************
      *600-CLIENT-DMY-SEC              SECTION.
      *
      *    MOVE    MCP-WINDOW          TO  SPA-SAKIPG
      *    MOVE    MCP-PUTTYPE         TO  SPA-PUTTYPE
      *    MOVE    "_PDMY01"           TO  MCP-WINDOW
      *    MOVE    "NEW"               TO  MCP-PUTTYPE
      *
      *    PERFORM 900-PUT-WINDOW
      *
      *    .
      *600-CLIENT-DMY-EXT.
      *    EXIT.
      *
      *****************************************************************
      *    基本画面のカルテ出力処理
      *****************************************************************
       21003-HC01-CALL2-SEC           SECTION.
      *
      *
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
           MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
      *
           MOVE    SPA-GMN-SELNUM      TO  SPA-HKNCOMBINUM
           MOVE    SPA-GMN-SRYKA       TO  SPA-SRYKA
           MOVE    SPA-GMN-SRYKAMEI    TO  SPA-SRYKAMEI
           MOVE    SPA-GMN-HAKKO       TO  ORCHC01-SYORIKBN
      *    病名なしは、区分＝４へ
           IF      SPA-GMN-HAKKO       =   "3"
               MOVE    "4"             TO  ORCHC01-SYORIKBN
           END-IF
      *    クライアント印刷
      *    IF      FLG-CLIENT-OK       =   1
               MOVE    "01"                TO  SPA-PRT-FLG
      *    END-IF
      *
           CALL    WRK-KARUTE-PG     USING  SPA-AREA
                                            ORCHC01AREA
      *
           .
       21003-HC01-CALL2-EXT.
           EXIT.
      *
      *****************************************************************
      *    カルテ　出力へ
      *****************************************************************
       21001-HC01-CALL-SEC           SECTION.
      *
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
           MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
           IF      SPA-GMN20-THKNCOMBI(IDX2)   =   SPACE
               MOVE    ZERO            TO  SPA-HKNCOMBINUM
           ELSE
               MOVE    SPA-GMN20-THKNCOMBI(IDX2)
                                       TO  SPA-HKNCOMBINUM
           END-IF
           MOVE    SPA-NAI20-SRYKA   (IDX2)
                                       TO  SPA-SRYKA
           MOVE    SPA-GMN20-TSRYKA  (IDX2)
                                       TO  SPA-SRYKAMEI
           MOVE    SPA-GMN20-TKARUTE (IDX2)
                                       TO  ORCHC01-SYORIKBN
      *    病名なしは、区分＝４へ
           IF      SPA-GMN20-TKARUTE (IDX2)    =   "3"
               MOVE    "4"             TO  ORCHC01-SYORIKBN
           END-IF
      *    クライアント印刷
      *    IF      FLG-CLIENT-OK       =   1
               MOVE    "01"                TO  SPA-PRT-FLG
      *        再印刷指示なし
               MOVE    2                   TO  SPA-CLIENT-TEMP-FLG
      *    END-IF
      *
           CALL    WRK-KARUTE-PG       USING  SPA-AREA
                                              ORCHC01AREA
      *
           .
       21001-HC01-CALL-EXT.
           EXIT.
      *****************************************************************
      *    処方箋　出力へ
      *****************************************************************
       21001-HC02-CALL-SEC           SECTION.
      *
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
           MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
           MOVE    SPA-GMN-NAME        TO  SPA-NAME
           MOVE    SPA-GMN-SEXH        TO  SPA-SEX
           MOVE    SPA-NAI-BIRTHDAY    TO  SPA-BIRTHDAY
      *
           MOVE    SPA-NAI20-SRYKA   (IDX2)       TO  SPA-SRYKA
           MOVE    SPA-GMN20-TSRYKA  (IDX2)       TO  SPA-SRYKAMEI
           MOVE    "1"                            TO  SPA-DRCD (1:1)
           MOVE    SPA-NAI20-DRCD    (IDX2)       TO  SPA-DRCD (2:)
           MOVE    SPA-GMN20-THKNCOMBI(IDX2)       TO  SPA-HKNCOMBINUM
      *    クライアント印刷
      *    IF      FLG-CLIENT-OK       =   1
               MOVE    "02"                TO  SPA-PRT-FLG
      *        再印刷指示なし
               MOVE    2                   TO  SPA-CLIENT-TEMP-FLG
      *    END-IF
      *    印刷
           INITIALIZE                  ORCHC19AREA
           MOVE    ZERO                TO  ORCHC19-RENNUM
           MOVE    "1"                 TO  ORCHC19-KBN
           CALL    WRK-SYOHOU-PG       USING   SPA-AREA
                                               ORCHC19AREA
      *
           .
       21001-HC02-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録後、処理
      *****************************************************************
       49021-KOUSIN-AFTER-SEC          SECTION.
      *
           MOVE    SPACE               TO  SPA-KEI-ERRCD
      *
      *    更新済
           MOVE    SPACE               TO  SPA-UPDFLG
      *    前回患者へ
           MOVE    SPA-GMN-PTNUM       TO  SPA-LAST-PTNUM
           MOVE    SPA-GMN-PTID        TO  SPA-LAST-PTID
      *
      *    ＣＬＡＩＭ送信
           IF      SPA-FLG-CLAIM       =   1
               PERFORM 4906-CLAIM-SOUSIN-SEC
               PERFORM 49061-API-CLMAI-SEC
           END-IF
      *!!!!
      *H28.6
      *    ginbee対応
           IF     (SPA-MW              =   SPA-GINBEE)
             AND  (SPA-ERRCD           =   SPACE     )
             AND  (SPA-GMN-PTNUM   NOT =   SPACE     )
      *        AIP CLAIM送信処理
               PERFORM 49061-API-CLMAI-SEC
           END-IF
      *
      *
      *H29.7
      *PUSH通信
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-P02-SYORI   =   1
                   MOVE    1               TO  FLG-PUSHSYORI
               ELSE
                   MOVE    2               TO  FLG-PUSHSYORI
               END-IF
               PERFORM 600-PUSH-SYORI-SEC
           END-IF
      *
      **** IF      SPA-MACHIPG         =   "U02"  OR  "K02" OR "Q02"
           MOVE    0                   TO  FLG-BACK
           IF      SPA-HZN-MOTOPG(1:3) =   "U02"
               IF       SPA-HZN-PTNUM       =   SPACE
                   IF     (SPA-P02-UKEID    NOT =  ZERO )  AND
                          (SPA-P02-SYORI        =   1   )
                       MOVE    1               TO  FLG-BACK
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-BACK
               END-IF
           END-IF
           IF      SPA-HZN-MOTOPG(1:3) =   "K02" OR "Q02"
                                                 OR  "I01"
               IF     (SPA-HZN-PTNUM       =   SPACE  )
                   MOVE    0               TO  FLG-BACK
               ELSE
                   MOVE    1               TO  FLG-BACK
               END-IF
           END-IF
      *
      *    ユーザプログラム起動
      *v4.8
      *    ginbee対応
      **   IF      SPA-MW              =   SPA-GINBEE
      *        CONTINUE
      **** ELSE
               MOVE    1                   TO  FLG-USERPG
               PERFORM 4907-USERPG-SEC
      ***  END-IF
      *
           IF      FLG-END             =   1
               MOVE    ZERO                TO  SPA-KEI-FLG
               GO      TO      49021-KOUSIN-AFTER-EXT
           END-IF
      *
      *****IF      SPA-HZN-MOTOPG(1:3) =   "U02"  OR  "K02" OR "Q02"
      *                                           OR  "I01"
      *        IF     (SPA-HZN-PTNUM       =   SPACE  )
      *!!!     AND    (SPA-P02-UKEID       =   ZERO   )
           IF      FLG-BACK            =   ZERO
                   MOVE    SPACE               TO  SPA-PTNUM
                   MOVE    ZERO                TO  SPA-PTID
               ELSE
      *
                   MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
                   MOVE    SPA-GMN-PTID        TO  SPA-PTID
                   MOVE    SPACE               TO  SPA-ERRMSG
      *            更新後、元の画面へ
                   IF     (FLG-CLAIM-OK        =   ZERO )  AND
                          (SPA-KEI-FLG         =   ZERO )
                       PERFORM 210-BACK
                   ELSE
                       PERFORM 210-BACK-SYORI-SEC
                   END-IF
                   MOVE    ZERO                TO  SPA-KEI-FLG
                   GO      TO      49021-KOUSIN-AFTER-EXT
      *********END-IF
           END-IF
           MOVE    ZERO                TO  SPA-KEI-FLG
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
      *    各画面を初期編集する
           MOVE    SPA-HZN-PTNUM       TO  WRK-HZN-PTNUM
           MOVE    SPA-HZN-MOTOPG      TO  WRK-HZN-MOTOPG
           INITIALIZE                  SPA-P02SCRAREA
           INITIALIZE                  SPA-P02KYOUTU
           MOVE    WRK-HZN-PTNUM       TO  SPA-HZN-PTNUM
           MOVE    WRK-HZN-MOTOPG      TO  SPA-HZN-MOTOPG
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    "P02"               TO  SPA-P02-GAMEN
           MOVE    ZERO                TO  SPA-GMN-CUR
           PERFORM 310-SPA-SET-SEC
      *
           .
       49021-KOUSIN-AFTER-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       4901-KOUSINMAE-SEC             SECTION.
      *
           IF    ( SPA-GMN-PTNUM       =   SPACE   )  OR
                 ((SPA-P02-SYORI   NOT =   1     ) AND
                  (SPA-GMN-PTID        =   ZERO  ))
               MOVE    ZERO                TO  SPA-GMN-CUR
               MOVE    "0009"              TO  SPA-ERRCD
               GO  TO                  4901-KOUSINMAE-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-PIDCD
      *    -------------------------
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           EVALUATE    SPA-P02-GAMEN
      *各画面表示中
      *    受付情報
           WHEN    "P020"
               MOVE    "CLICKED"           TO  MCP-EVENT
               MOVE    "B12"               TO  MCP-WIDGET
               MOVE    "PUTG"              TO  MCP-STATUS
               CALL    "ORCGP020"      USING   MCPAREA
                                               SPAAREA
                                               LINKAREA
                                               SCRAREA
               MOVE    SPA-COMMON          TO  SPA-AREA
               MOVE    SPA-FREE            TO  SPA-P02SCRAREA
               MOVE    SPA-WORK            TO  SPA-P02KYOUTU
      *    基本情報
           WHEN    "P02"
               MOVE    "CLICKED"           TO  MCP-EVENT
               MOVE    "B12"               TO  MCP-WIDGET
               MOVE    "PUTG"              TO  MCP-STATUS
               CALL    "ORCGP02W1"     USING   MCPAREA
                                               SPAAREA
                                               LINKAREA
                                               SCRAREA
               MOVE    SPA-COMMON          TO  SPA-AREA
               MOVE    SPA-FREE            TO  SPA-P02SCRAREA
               MOVE    SPA-WORK            TO  SPA-P02KYOUTU
      *    所得
           WHEN    "P024"
               MOVE    "CLICKED"           TO  MCP-EVENT
               MOVE    "B12"               TO  MCP-WIDGET
               MOVE    "PUTG"              TO  MCP-STATUS
               CALL    "ORCGP024"      USING   MCPAREA
                                               SPAAREA
                                               LINKAREA
                                               SCRAREA
               MOVE    SPA-COMMON          TO  SPA-AREA
               MOVE    SPA-FREE            TO  SPA-P02SCRAREA
               MOVE    SPA-WORK            TO  SPA-P02KYOUTU
      *    住所
           WHEN    "P021"
               MOVE    "CLICKED" TO  MCP-EVENT
               MOVE    "B12"     TO  MCP-WIDGET
               MOVE    "PUTG"    TO  MCP-STATUS
               CALL    "ORCGP021"       USING   MCPAREA
                                                SPAAREA
                                                LINKAREA
                                                SCRAREA
               MOVE    SPA-COMMON          TO  SPA-AREA
               MOVE    SPA-FREE            TO  SPA-P02SCRAREA
               MOVE    SPA-WORK            TO  SPA-P02KYOUTU
      *
      *    入力履歴
           WHEN    "P025"
               MOVE    "CLICKED" TO  MCP-EVENT
               MOVE    "B12"     TO  MCP-WIDGET
               MOVE    "PUTG"    TO  MCP-STATUS
               CALL    "ORCGP025"       USING   MCPAREA
                                                SPAAREA
                                                LINKAREA
                                                SCRAREA
               MOVE    SPA-COMMON          TO  SPA-AREA
               MOVE    SPA-FREE            TO  SPA-P02SCRAREA
               MOVE    SPA-WORK            TO  SPA-P02KYOUTU
      *
      *H24.12
      *        特記事項情報・レセプト分割
           WHEN    "P026"
               MOVE    "CLICKED" TO  MCP-EVENT
               MOVE    "B12"     TO  MCP-WIDGET
               MOVE    "PUTG"    TO  MCP-STATUS
               CALL    "ORCGP026"       USING   MCPAREA
                                                SPAAREA
                                                LINKAREA
                                                SCRAREA
               MOVE    SPA-COMMON          TO  SPA-AREA
               MOVE    SPA-FREE            TO  SPA-P02SCRAREA
               MOVE    SPA-WORK            TO  SPA-P02KYOUTU
      *
      *VER.4.7
      *        個別設定
           WHEN    "P028"
               MOVE    "CLICKED" TO  MCP-EVENT
               MOVE    "B12"     TO  MCP-WIDGET
               MOVE    "PUTG"    TO  MCP-STATUS
               CALL    "ORCGP028"       USING   MCPAREA
                                                SPAAREA
                                                LINKAREA
                                                SCRAREA
               MOVE    SPA-COMMON          TO  SPA-AREA
               MOVE    SPA-FREE            TO  SPA-P02SCRAREA
               MOVE    SPA-WORK            TO  SPA-P02KYOUTU
      *
           END-EVALUATE
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    MCP-WIDGET          TO  SPA-P02-WIDGET
               GO  TO                  4901-KOUSINMAE-EXT
           END-IF
           IF     (SPA-PIDCD           NOT =   SPACE )
               IF      (SPA-PIDCD(1:1)         =   "K"   )
                   MOVE    SPA-PIDCD           TO  WRK-PIDCD
                   MOVE    SPACE               TO  SPA-PIDCD
               ELSE
                   MOVE    MCP-WIDGET          TO  SPA-P02-WIDGET
                   GO      TO                  4901-KOUSINMAE-EXT
               END-IF
           END-IF
           IF     (SPA-FLG-END     NOT =   ZERO  )
               GO      TO                  4901-KOUSINMAE-EXT
           END-IF
      *
      *    表示画面以外
           IF      SPA-P02-GAMEN   NOT =   "P020"
      *        受付情報
               MOVE    "CLICKED"           TO  MCP-EVENT
               MOVE    "B12"               TO  MCP-WIDGET
               MOVE    "PUTG"              TO  MCP-STATUS
               CALL    "ORCGP020"      USING   MCPAREA
                                               SPAAREA
                                               LINKAREA
                                               SCRAREA
               MOVE    SPA-COMMON          TO  SPA-AREA
               MOVE    SPA-FREE            TO  SPA-P02SCRAREA
               MOVE    SPA-WORK            TO  SPA-P02KYOUTU
               IF      SPA-ERRCD       NOT =   SPACE
                   MOVE    "P020"              TO  SPA-P02-GAMEN
                   MOVE    MCP-WIDGET          TO  SPA-P02-WIDGET
                   GO  TO                  4901-KOUSINMAE-EXT
               END-IF
           END-IF
           IF      SPA-P02-GAMEN   NOT =   "P024"
      *        所得情報チェック
               MOVE    "CLICKED"           TO  MCP-EVENT
               MOVE    "B12"               TO  MCP-WIDGET
               MOVE    "PUTG"              TO  MCP-STATUS
               CALL    "ORCGP024"     USING   MCPAREA
                                               SPAAREA
                                               LINKAREA
                                               SCRAREA
               MOVE    SPA-COMMON          TO  SPA-AREA
               MOVE    SPA-FREE            TO  SPA-P02SCRAREA
               MOVE    SPA-WORK            TO  SPA-P02KYOUTU
               IF      SPA-ERRCD       NOT =   SPACE
                   MOVE    "P024"              TO  SPA-P02-GAMEN
                   MOVE    MCP-WIDGET          TO  SPA-P02-WIDGET
                   GO  TO                  4901-KOUSINMAE-EXT
               END-IF
           END-IF
      *
           IF      SPA-P02-GAMEN   NOT =   "P026"
      *        特記事項情報・レセプト分割
               MOVE    "CLICKED"           TO  MCP-EVENT
               MOVE    "B12"               TO  MCP-WIDGET
               MOVE    "PUTG"              TO  MCP-STATUS
               CALL    "ORCGP026"     USING   MCPAREA
                                               SPAAREA
                                               LINKAREA
                                               SCRAREA
               MOVE    SPA-COMMON          TO  SPA-AREA
               MOVE    SPA-FREE            TO  SPA-P02SCRAREA
               MOVE    SPA-WORK            TO  SPA-P02KYOUTU
               IF      SPA-ERRCD       NOT =   SPACE
                   MOVE    "P026"              TO  SPA-P02-GAMEN
                   MOVE    MCP-WIDGET          TO  SPA-P02-WIDGET
                   GO  TO                  4901-KOUSINMAE-EXT
               END-IF
           END-IF
      *
           IF      SPA-P02-GAMEN   NOT =   "P02"
      *        基本情報以外
      *        基本情報チェック
               MOVE    "CLICKED"           TO  MCP-EVENT
               MOVE    "B12"               TO  MCP-WIDGET
               MOVE    "PUTG"              TO  MCP-STATUS
               CALL    "ORCGP02W1"     USING   MCPAREA
                                               SPAAREA
                                               LINKAREA
                                               SCRAREA
               MOVE    SPA-COMMON          TO  SPA-AREA
               MOVE    SPA-FREE            TO  SPA-P02SCRAREA
               MOVE    SPA-WORK            TO  SPA-P02KYOUTU
               IF     (SPA-ERRCD           NOT =   SPACE )
                   MOVE    "P02"               TO  SPA-P02-GAMEN
                   MOVE    MCP-WIDGET          TO  SPA-P02-WIDGET
                   GO      TO                  4901-KOUSINMAE-EXT
               END-IF
               IF     (SPA-PIDCD           NOT =   SPACE )
                   IF      (SPA-PIDCD(1:1)          =   "K"   )
                       MOVE    SPA-PIDCD           TO  WRK-PIDCD
                       MOVE    SPACE               TO  SPA-PIDCD
                   ELSE
                       MOVE    "P02"               TO  SPA-P02-GAMEN
                       MOVE    MCP-WIDGET          TO  SPA-P02-WIDGET
                       GO      TO                  4901-KOUSINMAE-EXT
                   END-IF
               END-IF
               IF     (SPA-FLG-END     NOT =   ZERO  )
                   GO      TO                  4901-KOUSINMAE-EXT
               END-IF
           END-IF
      *
      *    確認メッセージ
           IF     (SPA-PIDCD           =   SPACE )  AND
                  (WRK-PIDCD       NOT =   SPACE )
               MOVE    WRK-PIDCD           TO  SPA-PIDCD
               MOVE    "P02"               TO  SPA-P02-GAMEN
               GO      TO                  4901-KOUSINMAE-EXT
           END-IF
      *
      *    患者番号存在チェック（新規の時、存在しないこと）
           IF      SPA-P02-SYORI       =   1
               PERFORM 491010-PTNUM-DBCHECK-SEC
           END-IF
      *
           .
       4901-KOUSINMAE-EXT.
           EXIT.
      *****************************************************************
      *    患者番号変換マスタ　変更チェック
      *****************************************************************
       491010-PTNUM-DBCHECK-SEC          SECTION.
      *
      *    患者番号変換（管理）マスタ読み込み
           INITIALIZE                      PTNUM-REC
           MOVE    SPA-HOSPNUM         TO  PTNUM-HOSPNUM
           MOVE    SPA-GMN-PTNUM       TO  PTNUM-PTNUM
      *R01.12 ２０桁で検索する
      *!   IF      SPA-GMN-PTNUM(20:1) =   SPACE
      *        STRING  SPA-GMN-PTNUM       DELIMITED   BY  SPACE
      *                "%"                 DELIMITED   BY  SIZE
      *                INTO                    PTNUM-PTNUM
      *        END-STRING
      *!   END-IF
      *
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
      *R01.12
      *  ２０桁で検索する
      ***  MOVE    "key2"              TO  MCP-PATHNAME
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptnum"         TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC     TO  PTNUM-REC
                   MOVE    ZERO            TO  FLG-PTNUM
               ELSE
                   MOVE    1               TO  FLG-PTNUM
                   INITIALIZE                  PTNUM-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTNUM
               INITIALIZE                  PTNUM-REC
           END-IF
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PTNUM           =   ZERO
               MOVE    "1011"              TO  SPA-ERRCD
               MOVE    ZERO                TO  SPA-GMN-CUR
           ELSE
      *
      *    患者マスター存在チェック
               IF      SPA-GMN-PTID    NOT =   ZERO 
                   MOVE    SPA-HOSPNUM        TO  PTINF-HOSPNUM
                   MOVE    SPA-GMN-PTID       TO  PTINF-PTID
                   PERFORM 900-PTINF-READ-SEC
                   IF      FLG-PTINF          =   ZERO 
                       MOVE    "1011"              TO  SPA-ERRCD
                       MOVE    ZERO                TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
           .
       491010-PTNUM-DBCHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    複写処理
      *****************************************************************
       499-COPY-SEC             SECTION.
      *
           IF    ( SPA-GMN-PTNUM       =   SPACE   )  OR
                 ((SPA-P02-SYORI   NOT =   1     ) AND
                  (SPA-GMN-PTID        =   ZERO  ))
               MOVE    "1021"              TO  SPA-ERRCD
               MOVE    ZERO                TO  SPA-GMN-CUR
               GO      TO      499-COPY-EXT
           END-IF
      *    新規の時のみ、複写できることとする
           IF      SPA-P02-SYORI   NOT =   1
               MOVE    "1022"              TO  SPA-ERRCD
               MOVE    ZERO                TO  SPA-GMN-CUR
               GO      TO      499-COPY-EXT
           END-IF
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP02E"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-P02SCRAREA
           MOVE    SPA-WORK            TO  SPA-P02KYOUTU
      *
           MOVE    "P02"               TO  SPA-MOTOPG
           MOVE    "P02E"              TO  SPA-SAKIPG
      *    カーソル移動
           MOVE    "PTNUM"             TO  MCP-WIDGET
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "P02E"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       499-COPY-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＬＡＩＭ接続情報取得処理
      *****************************************************************
       4905-CLAIM-CHK-SEC               SECTION.
      *
           INITIALIZE                  SPA-P02X-AREA
      *
           MOVE    ZERO                TO  FLG-CLAIM
           MOVE    ZERO                TO  FLG-CLAIM-MULTIHOST
      *    ＣＬＡＩＭ接続判定
           MOVE    SPACE               TO  SYS-9000-REC
           INITIALIZE                  SYS-9000-REC
           MOVE    "9000"              TO  SYS-9000-KANRICD
           MOVE    "*"                 TO  SYS-9000-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-9000-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-9000-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-9000-HOSPNUM
           MOVE    SYS-9000-REC        TO  SYSKANRI-REC
           PERFORM 9009-SYSKANRI-KEY-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-9000-REC
           END-IF
      *
           MOVE    ZERO                TO  FLG-CLAIM-MULTIHOST
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYS-9000-CODE       TO  SPA-GMN-SCODE
               MOVE    1                   TO  SPA-P02X-GMN-SEL
      *        マルチホストフラグ
               IF      SYS-9000-MULTIHOST-FLG  =   "1"
                    MOVE    1                   TO  FLG-CLAIM-MULTIHOST
               END-IF
               IF     (SYS-9000-SETUZOKU   =   ZERO )  OR
                      (SYS-9000-PT-FLG NOT =   "1"  )
                   CONTINUE
               ELSE
                   MOVE    SYS-9000-CODE       TO  SPA-P02X-CODE
                   IF      SYS-9000-KANBUTTON (02) =   "T"
                       MOVE    2               TO  FLG-CLAIM
                   ELSE
                       MOVE    1               TO  FLG-CLAIM
                   END-IF
               END-IF
           END-IF
      *
           IF      FLG-CLAIM           =   2
               CONTINUE
           ELSE
               GO      TO      4905-CLAIM-CHK-EXT
           END-IF
      *
      *    ＣＬＡＩＭ接続情報
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP02X"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-P02SCRAREA
           MOVE    SPA-WORK            TO  SPA-P02KYOUTU
     *
           MOVE    "P02"               TO  SPA-MOTOPG
           MOVE    "P02X"              TO  SPA-SAKIPG
      *    カーソル移動
           MOVE    "SELNUM"            TO  MCP-WIDGET
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "P02X"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4905-CLAIM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＬＡＩＭ接続情報取得処理
      *****************************************************************
       4906-CLAIM-SOUSIN-SEC               SECTION.
      *
      *    受付時間取得
           MOVE    SPACE               TO  WRK-UKETIME
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           MOVE    SMCNDATE-HMS        TO  WRK-UKETIME
      *
           MOVE    SPACE               TO  SYS-9001-REC
           INITIALIZE                  SYS-9001-REC
           MOVE    "9001"              TO  SYS-9001-KANRICD
           MOVE    SPA-SYSYMD          TO  SYS-9001-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-9001-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-9001-HOSPNUM
           MOVE    SYS-9001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-9001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  WRK-RENNUM
           PERFORM 
                   UNTIL       (FLG-SYSKANRI   =   1    )
               MOVE    MCPDATA-REC         TO  SYS-9001-REC
               MOVE    ZERO                TO  FLG-SOUSIN
               IF      FLG-CLAIM-MULTIHOST =   1
      *            複数送信処理
                   MOVE    1                   TO  FLG-SOUSIN
               ELSE
                   MOVE    SYS-9001-KBNCD (1:2)  TO  WRK-GMN-SELNUM
                   IF      WRK-GMN-SELNUM        =   SPA-P02X-GMN-SEL
                       MOVE    1                   TO  FLG-SOUSIN
                       MOVE    1                   TO  FLG-SYSKANRI
                   END-IF
               END-IF
               IF      FLG-SOUSIN          =   1
                   PERFORM 49061-CLAIM-SOUSINSYOSI-SEC
               END-IF
      *
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
               END-IF
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       4906-CLAIM-SOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＬＡＩＭ接続情報取得処理
      *****************************************************************
       49061-CLAIM-SOUSINSYOSI-SEC               SECTION.
      *
      *    フィアルデイレクトリ位置指定
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    CLAIM-FILE          TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
      *
           MOVE    MKPASS-OT-01        TO  SHELLTBL-NAME
      *
      **** MOVE    "1"                 TO  SHELLTBL-ARG1
           MOVE    SPA-GMN-SCODE       TO  SHELLTBL-ARG1
           MOVE    SPA-GMN-PTID        TO  WRK-PTID
           MOVE    WRK-PTID            TO  SHELLTBL-ARG2
           MOVE    "XX"                TO  SHELLTBL-ARG3
           MOVE    WRK-UKETIME         TO  SHELLTBL-ARG4
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG5
      *
           MOVE    SYS-9001-ADDRESS    TO  SHELLTBL-ARG6
           MOVE    SYS-9001-PORT1      TO  SHELLTBL-ARG7
      *    ドクター
           MOVE    "@@@@@"             TO  SHELLTBL-ARG8
      *    保険組合せ
           MOVE    "@@@@"              TO  SHELLTBL-ARG9
      *    診療内容
           MOVE    "XX"                TO  SHELLTBL-ARG10
      *    連番
           ADD     1                   TO  WRK-RENNUM
           MOVE    WRK-RENNUM          TO  SHELLTBL-ARG11 (1:2)
      *    システム日付
           MOVE    SPA-SYSYMD          TO  SHELLTBL-ARG12
      *
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "shell"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       49061-CLAIM-SOUSINSYOSI-EXT.
           EXIT.
      *H28.6
      *****************************************************************
      *    API ＣＬＡＩＭ送信処理
      *****************************************************************
       49061-API-CLMAI-SEC  SECTION.
      *
           MOVE    ZERO                TO  FLG-CLAIM
           MOVE    ZERO                TO  WRK-RENNUM
      *    ＣＬＡＩＭ接続判定
           MOVE    SPACE               TO  SYS-9000-REC
           INITIALIZE                  SYS-9000-REC
           MOVE    "9000"              TO  SYS-9000-KANRICD
           MOVE    "*"                 TO  SYS-9000-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-9000-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-9000-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-9000-HOSPNUM
           MOVE    SYS-9000-REC        TO  SYSKANRI-REC
           PERFORM 9009-SYSKANRI-KEY-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-9000-REC
           END-IF
      *
           IF      FLG-SYSKANRI        =   1
               CONTINUE
           ELSE
               MOVE    SYS-9000-CODE       TO  SPA-GMN-SCODE
               IF     (SYS-9000-SETUZOKU   =   ZERO )  OR
                      (SYS-9000-PT-FLG NOT =   "1"  )
                   CONTINUE
               ELSE
      *            ＣＬＡＩＭ送信あり
                   MOVE    1               TO  FLG-CLAIM
               END-IF
           END-IF
      *
           IF      FLG-CLAIM           =   1
      *        受付時間取得
               MOVE    SPACE               TO  WRK-UKETIME
               INITIALIZE                  ORCSMCNDATEAREA
               CALL    "ORCSMCNDATE"       USING
                                           ORCSMCNDATEAREA
               MOVE    SMCNDATE-HMS        TO  WRK-UKETIME
      *
      *        フィアルデイレクトリ位置指定
               INITIALIZE                  ORCSMKPASSAREA
               MOVE    CLAIM-FILE          TO  MKPASS-IN-01
               CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
      *
               MOVE    "HL04.sh"           TO  SHELLTBL-NAME
      *
               MOVE    SPA-GMN-SCODE       TO  SHELLTBL-ARG1
               MOVE    SPA-GMN-PTID        TO  WRK-PTID
               MOVE    WRK-PTID            TO  SHELLTBL-ARG2
               MOVE    "XX"                TO  SHELLTBL-ARG3
               MOVE    WRK-UKETIME         TO  SHELLTBL-ARG4
               MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG5
      *
      *****    MOVE    SYS-9001-ADDRESS    TO  SHELLTBL-ARG6
      *****    MOVE    SYS-9001-PORT1      TO  SHELLTBL-ARG7
      *        ドクター
               MOVE    "@@@@@"             TO  SHELLTBL-ARG8
      *        保険組合せ
               MOVE    "@@@@"              TO  SHELLTBL-ARG9
      *        診療内容
               MOVE    "XX"                TO  SHELLTBL-ARG10
      *        連番
               ADD     1                   TO  WRK-RENNUM
               MOVE    WRK-RENNUM          TO  SHELLTBL-ARG11 (1:2)
      *        システム日付
               MOVE    SPA-SYSYMD          TO  SHELLTBL-ARG12
      *        患者番号
               MOVE    SPA-GMN-PTNUM       TO  SHELLTBL-ARG14
      *
               MOVE    SHELLTBL            TO  MCPDATA-REC
               MOVE    "SHELL"             TO  MCP-FUNC
               MOVE    "shell"             TO  MCP-TABLE
               MOVE    "allways"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           END-IF
      *
           .
       49061-API-CLMAI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ユーザプログラム起動処理
      *****************************************************************
       4907-USERPG-SEC  SECTION.
      *
           EVALUATE    FLG-USERPG
               WHEN    1
      *        追加
                   IF      SPA-P02-SYORI   =   1
                       MOVE    1               TO  SPA-SYORI-FLG
                   ELSE
      *        更新
                       MOVE    2               TO  SPA-SYORI-FLG
                   END-IF
               WHEN    2
      *        削除
                   MOVE    3               TO  SPA-SYORI-FLG
           END-EVALUATE
           IF      SPA-GMN-USERPG      =   "1"
      *        ユーザプログラム実行画面表示へ
               PERFORM 49071-XD01-SYORI-SEC
           ELSE
               IF      SPA-NAI-GYOUMU-ARI  =   "1"
      *            起動するユーザプログラムある時、実行
                   INITIALIZE                      ORCSUSERCHKAREA
                   MOVE    "P02"               TO  ORCSUSERCHK-GMNPG
                   MOVE    "2"                 TO  ORCSUSERCHK-SYORI
                   MOVE    SPA-SYSYMD          TO  ORCSUSERCHK-SYSYMD
                   MOVE    SPA-SYSYMD          TO  ORCSUSERCHK-SRYYMD
                   MOVE    SPA-GMN-PTID        TO  ORCSUSERCHK-PTID
                   MOVE    SPA-GMN-PTNUM       TO  ORCSUSERCHK-PTNUM
                   MOVE    SPACE               TO  ORCSUSERCHK-SRYKA
                   MOVE    SPACE               TO  ORCSUSERCHK-HKNCOMBI
                   MOVE    SPACE               TO  ORCSUSERCHK-DRCD
                   MOVE    SPA-SYORI-FLG       TO  ORCSUSERCHK-SYORI-FLG
                   CALL    "ORCSUSERCHK"       USING
                                               ORCSUSERCHKAREA
                                               SPA-AREA
               END-IF
           END-IF
      *
           .
       4907-USERPG-EXT.
           EXIT.
      *
      *****************************************************************
      *    ユーザプログラム実行画面表示処理
      *****************************************************************
       49071-XD01-SYORI-SEC              SECTION.
      *
      *    パラメタファイル削除
           PERFORM 992-PARA-DEL-SEC
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           IF      FLG-BACK            =   ZERO
               MOVE    "P02"               TO  SPA-SAKIPG
               MOVE    "P02"               TO  SPA-MOTOPG
           ELSE
               MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
               MOVE    SPA-GMN-PTID        TO  SPA-PTID
               MOVE    SPACE               TO  SPA-ERRMSG
      *        更新後、元の画面へ
               IF      SPA-HZN-MOTOPG(1:3) =   "U02"  OR  "K02" OR "Q02"
                                               OR  "I01"
                   MOVE    SPA-HZN-MOTOPG      TO  SPA-SAKIPG
                   MOVE    "P02"               TO  SPA-MOTOPG
                   MOVE    SPACE               TO  SPA-MOTOPG2
               ELSE
                   MOVE    "P02"               TO  SPA-SAKIPG
                   MOVE    "P02"               TO  SPA-MOTOPG
               END-IF
           END-IF
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-P02SCRAREA      TO  LNK-SCRSPA
      *
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
           MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
           MOVE    SPA-GMN-NAMEH       TO  SPA-NAME
           MOVE    SPA-GMN-SEXH        TO  SPA-SEX
           MOVE    SPA-GMN-BIRTHDAYH   TO  SPA-BIRTHDAYW
           MOVE    SPACE               TO  SPA-SRYKA
           MOVE    SPACE               TO  SPA-HKNCOMBINUM
           MOVE    SPACE               TO  SPA-DRCD
           MOVE    SPACE               TO  SPA-SRYYMD
      *
      *    画面移行用のパラメタ変更
           MOVE    "P02"               TO  SPA-MOTOPG
           MOVE    "XD01"              TO  SPA-SAKIPG
      *****INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           IF     (FLG-CLAIM-OK        =   ZERO )  AND
                  (SPA-KEI-FLG         =   ZERO )  AND
                  (FLG-USERPG      NOT =   2    )
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
           ELSE
               MOVE    "JOIN"              TO  MCP-PUTTYPE
           END-IF
           MOVE   "XD01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       49071-XD01-SYORI-EXT.
           EXIT.
      *
      *H29.7
      *PUSH通信
      *****************************************************************
      *    PUSH通信処理
      *****************************************************************
       600-PUSH-SYORI-SEC             SECTION.
      *
           INITIALIZE                      PUSHPT01-REC
           MOVE    "patient_infomation"    TO  PUSHPT01-EVENT
           EVALUATE    FLG-PUSHSYORI
           WHEN    1
      *        追加
               MOVE    "add"               TO  PUSHPT01-PMODE
           WHEN    2
      *        更新
               MOVE    "modify"            TO  PUSHPT01-PMODE
           WHEN    3
      *        削除
               MOVE    "delete"            TO  PUSHPT01-PMODE
           END-EVALUATE
      *    処理時間取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  PUSHPT01-SYSYMD
           MOVE    WRK-HEN-TIME        TO  PUSHPT01-SYSTIME
           MOVE    SPA-GMN-PTNUM       TO  PUSHPT01-PTNUM
      *
           MOVE    PUSHPT01-REC        TO  MCPDATA-REC
      *
           MOVE    "PUSHEVENT"         TO  MCP-FUNC
           MOVE    "push_patient01"    TO  MCP-TABLE
           MOVE    "push_patient01"    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       600-PUSH-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
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
           MOVE    WRK-THHH            TO  WRK-HEN-THH
           MOVE    WRK-THMM            TO  WRK-HEN-TMM
           MOVE    WRK-THSS            TO  WRK-HEN-TSS
           MOVE    ":"                 TO  WRK-HEN-T1
           MOVE    ":"                 TO  WRK-HEN-T2
           .
       801-DAYHEN01-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    西暦→和暦FORMAT日付編集
      *****************************************************************
       410112-SE-WAFORMAT-YMDEDIT-SEC  SECTION.
      *
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"    USING   STS-AREA-DAY  LNK-DAY2-AREA
           IF  STS-DAY-RC1             =   ZERO
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMD
           ELSE
               MOVE    SPACE               TO  WRK-HENYMD
           END-IF
           .
       410112-SE-WAFORMAT-YMDEDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN                  SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *    排他中
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-LOCK        =   2
                   MOVE    "9998"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-LOCK
               END-IF
           END-IF
      *    保険組合せの最大番号と最大連番が違う時
           IF      SPA-ERRCD           =   SPACE
                                       OR  "9106"
      *        ＳＰＡを追加できないので代用
               IF      SPA-QRFLG       =   1
                   MOVE    "9106"              TO  SPA-ERRCD
                   MOVE    0                   TO  SPA-QRFLG
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO              500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-PIDCD       NOT =   SPACE
               PERFORM 520-JIDSET-SEC
               GO      TO              500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "P02"               TO  MCP-WINDOW.
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
           MOVE    "GMN"               TO  ORCSP02-SYORI
           CALL    "ORCSP02"           USING   ORCSP02AREA
                                               SPA-AREA
                                               SPA-P02KYOUTU
                                               SPA-P02SCRAREA
                                               P02
      *
           PERFORM 5001-MAPCUR-SEC
      *H26.4
           IF      SPA-ERRCD           =   "9108"
               MOVE    "B12"           TO   MCP-WIDGET
           END-IF
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
           IF      SPA-GMN-CUR         =   ZERO
               MOVE    "PTNUM"             TO   MCP-WIDGET
               GO      TO      5001-MAPCUR-EXT
           END-IF
      *
           IF      SPA-P02-WIDGET      =   SPACE
               EVALUATE    P02-PAGENO
                   WHEN    ZERO
                       MOVE  "P020_SELNUM"     TO   MCP-WIDGET
                   WHEN    1
                       MOVE  "P02_KANANAME"    TO   MCP-WIDGET
                   WHEN    2
                       MOVE  "P027_SELNUM"     TO   MCP-WIDGET
                   WHEN    3
                       MOVE  "P021_FAX"        TO   MCP-WIDGET
      ***********  WHEN    4
      ***********      MOVE  "P023_SELNUM"     TO   MCP-WIDGET
                   WHEN    4
                       MOVE  "P024_TSRSELNUM"  TO   MCP-WIDGET
                   WHEN    5
                       MOVE  "P025_SELNUM1"    TO   MCP-WIDGET
                   WHEN    6
                       MOVE  "P026_SELNUM"     TO   MCP-WIDGET
                   WHEN    7
                       MOVE  "P028_PATIENTID1" TO   MCP-WIDGET
               END-EVALUATE
           ELSE
               MOVE    SPA-P02-WIDGET          TO  MCP-WIDGET
           END-IF
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *    エラーメッセージ編集（ＡＰＩと共通）
           CALL    "ORCSP90"           USING   SPA-AREA
                                               SPA-P02KYOUTU
                                               SPA-P02SCRAREA
      *
           EVALUATE    SPA-ERRCD
               WHEN    "9999"
                   MOVE    "他端末で使用中です。"
                                               TO  SPA-ERRMSG
                   MOVE    SLOCK-MSG           TO  SPA-ERRMSG(21:)
               WHEN    "9998"
                   STRING  "他端末で使用中です。" 
                           "更新はできません。"    DELIMITED  BY  SIZE
                           SLOCK-MSG               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
           END-EVALUATE
      *
           MOVE    MCP-WIDGET          TO  SPA-WIDGET
      *
           MOVE    SPACE               TO  PERR
           MOVE    SPA-ERRCD           TO  PERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  PERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "P02"               TO  SPA-MOTOPG
           MOVE    "PERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "PERR"              TO  MCP-WINDOW
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
      *    エラーメッセージ表示理
      *****************************************************************
       520-JIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-PIDCD
               WHEN    "0101"
                   STRING  "内容が修正されています。"
                           "ＯＫで修正分は反映されずに終了します"
                                           DELIMITED  BY  SIZE
                                           INTO    WRK-PIDMSG
                   END-STRING
               WHEN    "0103"
                   STRING  "該当患者のデータをすべて削除します。"
                            "復元はできません。"
                            "よろしいですか？"
                                           DELIMITED  BY  SIZE
                                           INTO    WRK-PIDMSG
                   END-STRING
      *ver.4.7
               WHEN    "0113"
                   STRING  "受診履歴のある患者です。"
                           "削除後に復元はできません。"
                           "削除してよろしいですか？"
                                           DELIMITED  BY  SIZE
                                           INTO    WRK-PIDMSG
                   END-STRING
      *
               WHEN    "0104"
                   STRING    SPA-PIDMSG    DELIMITED  BY  SPACE
                             "を老人負担割合へ追加変更します。"
                             "よろしいですか？"
                                           DELIMITED  BY  SIZE
                                           INTO    WRK-PIDMSG
                   END-STRING
               WHEN    "0105"
      **********   MOVE
      *            "保険の期間が重複します。前の保険を終了させます。"
      *                                TO  WRK-PIDMSG
      *            MOVE
      *                 "よろしいですか？"
      *********                        TO  WRK-PIDMSG(47:)
                   STRING     "保険の期間が重複します。前の"
                                           DELIMITED  BY  SIZE
                              SPA-PIDMSG   DELIMITED  BY  SPACE
                              "を終了させます。よろしいですか？"
                                           DELIMITED  BY  SIZE
                                       INTO    WRK-PIDMSG
                   END-STRING
               WHEN    "0106"
                   STRING  "公費の期間が重複します。"
                           "前の公費を終了させます。よろしいですか？"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-PIDMSG
                   END-STRING
               WHEN    "0107"
                   MOVE    SPA-PIDMSG      TO  WRK-PIDMSG
      *        Ｈ１５．４改正から
               WHEN    "0201"
                   STRING  "保険が終了しています。終了日を変更します。"
                           "よろしいですか？"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-PIDMSG
                   END-STRING
               WHEN    "0202"
                   MOVE    "保険を切り替えます。よろしいですか？"
                                       TO  WRK-PIDMSG
               WHEN    "0203"
                   STRING  "継続は平成１５年３月末で終了しています。"
                           "終了日を変更します。よろしいですか？"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-PIDMSG
                   END-STRING
               WHEN    "0204"
                   STRING  "任継を切り替えます。"
                           "切り替え後の終了日を確認して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-PIDMSG
                   END-STRING
      *ver.4.7
               WHEN    "0250"
                   STRING  "漢字氏名が変更されました。"
                           "ＯＫで旧姓履歴に追加します。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-PIDMSG
                   END-STRING
      *ver.4.7
      *        Ｈ１８．１０改正から
               WHEN    "0110"
                   STRING  SPA-PIDMSG    DELIMITED  BY  SPACE
                           "の前期高齢者２割を平成１８年１０月から"
                           "３割に変更します。"
                           "よろしいですか？"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-PIDMSG
                   END-STRING
               WHEN    "0111"
                   STRING  "老人公費の２割を平成１８年１０月から"
                           "３割に変更します。"
                           "よろしいですか？"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-PIDMSG
                   END-STRING
      *
               WHEN    "K910"
                   STRING  "警告！"
                           "保険組合せ更新で期間外の診療が"
                           "発生します。"
                           "よろしいですか？"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-PIDMSG
                   END-STRING
               WHEN    "K911"
                   STRING  "警告！"
                           "保険組合せ更新で期間外の病名が"
                           "発生します。"
                           "よろしいですか？"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-PIDMSG
                   END-STRING
               WHEN    "K912"
                   STRING  "警告！"
                           "保険組合せ更新で期間外の診療と病名が"
                           "発生します。"
                           "よろしいですか？"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-PIDMSG
                   END-STRING
               WHEN    "1098"
                   STRING  "入力した住所が郵便番号の住所と違います。"
                           "変更しますか？"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-PIDMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPA-PIDCD
                                       TO  WRK-PIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-PID1-FLG
      *
           MOVE    SPACE               TO  PID1
           INITIALIZE                      PID1
           MOVE    SPA-PIDCD           TO  PID1-ID1CODE
           MOVE    WRK-PIDMSG          TO  PID1-ID1MSG
           MOVE    "戻る"              TO  PID1-B01
           MOVE    "ＯＫ"              TO  PID1-B12
           EVALUATE    SPA-PIDCD
               WHEN    "1098"
               WHEN    "0250"
                   MOVE    "ＮＯ"              TO  PID1-B01
           END-EVALUATE
      *
           EVALUATE    SPA-PIDCD
               WHEN    "0103"
               WHEN    "0113"
                   MOVE    "B01"               TO  MCP-WIDGET
               WHEN    OTHER
                   MOVE    "B12"               TO  MCP-WIDGET
           END-EVALUATE
      *
           MOVE    "P02"               TO  SPA-MOTOPG
           MOVE    "PID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "PID1"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-JIDSET-EXT.
           EXIT.
      *----(01.00.03) LINE ADD END   ----------------------------------
      *****************************************************************
      *    QRコード(被保険者証)読込
      *****************************************************************
       900-QRCDHKN-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  QRCDHKN-REC
               MOVE    ZERO            TO  FLG-QRCDHKN
           ELSE
               INITIALIZE                  QRCDHKN-REC
               MOVE    1               TO  FLG-QRCDHKN
           END-IF
           .
       900-QRCDHKN-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約マスター読込処理
      *****************************************************************
      *900-YOYAKU-READ-SEC         SECTION.
      *
      *    MOVE    "tbl_yyk"           TO  MCP-TABLE
      *
      *    PERFORM 920-DBFETCH-SEC
      *    IF      MCP-RC              =   ZERO
      *        MOVE    ZERO                TO  FLG-YYK
      *        MOVE    MCPDATA-REC         TO  YYK-REC
      *    ELSE
      *        MOVE    1                   TO  FLG-YYK
      *    END-IF
      *
      *    .
      *900-YOYAKU-READ-EXT.
      *    EXIT.
      *
      *****************************************************************
      *    同一患者識別マスター読込処理
      *****************************************************************
      *900-PTSAME-READ-SEC         SECTION.
      *
      *    PERFORM 920-DBFETCH-SEC
      *    IF      MCP-RC              =   ZERO
      *        MOVE    ZERO                TO  FLG-PTSAME
      *        MOVE    MCPDATA-REC         TO  PTSAME-REC
      *    ELSE
      *        MOVE    1                   TO  FLG-PTSAME
      *    END-IF
      *
      *    .
      *900-PTSAME-READ-EXT.
      *    EXIT.
      *
      *****************************************************************
      *    患者マスター読込（主キー）
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
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴マスター読込
      *****************************************************************
       970-JYURRK-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       970-JYURRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理主キー検索
      *****************************************************************
       9009-SYSKANRI-KEY-READ-SEC         SECTION.
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC     TO  SYSKANRI-REC
               ELSE
                   INITIALIZE                  SYSKANRI-REC
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
       9009-SYSKANRI-KEY-READ-EXT.
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
      *
           IF      SPA-LOCK            =   1
               MOVE    SPACE               TO  SPA-ERRCD
               MOVE    2                   TO  SPA-LOCK
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
      *    パラメタ削除処理
      *****************************************************************
       992-PARA-DEL-SEC          SECTION.
      *
      *    パラメタ削除
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       992-PARA-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＱＲ解除処理
      *****************************************************************
       9901-QRCDHKN-OUT-SEC         SECTION.
      *
           IF     (SPA-QR-REGISTYMD    NOT =   SPACE )  AND
                  (SPA-QR-REGISTID     NOT =   ZERO  )
      *
           INITIALIZE                      QRCDHKN-REC
           MOVE    SPA-HOSPNUM          TO  QRCDHKN-HOSPNUM
           MOVE    SPA-QR-REGISTYMD    TO  QRCDHKN-REGISTYMD
           MOVE    SPA-QR-REGISTID     TO  QRCDHKN-REGISTID
           MOVE    QRCDHKN-REC         TO  MCPDATA-REC
           MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-QRCDHKN-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-QRCDHKN
               INITIALIZE                      QRCDHKN-REC
           END-IF
           MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           IF      FLG-QRCDHKN         =   ZERO
               MOVE    ZERO                TO  QRCDHKN-MOD-FLG
               MOVE    SMCNDATE-YMD        TO  QRCDHKN-UPYMD
               MOVE    SMCNDATE-HMS        TO  QRCDHKN-UPHMS
      *
               MOVE    QRCDHKN-REC         TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO 
                   MOVE    "9000"              TO  SPA-ERRCD
                   DISPLAY "P029 QRCDHKN KEY UPD ERR:"  MCP-RC
                       ",KEY:" QRCDHKN-KEY
               END-IF
           END-IF
      *
           END-IF
           INITIALIZE                  SPA-QRDATA-AREA
      *
           .
       9901-QRCDHKN-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
