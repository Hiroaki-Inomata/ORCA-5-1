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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCSC10200203.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 診療行為チェック、算定（診療）
      *                      Ｈ１４．４改正まで
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     MCC−多々納  01/04/19  時間外加算の乳幼児加算の修正等
      * 02.00.00     MCC−多々納  01/08/09  全体の修正
      * 02.00.01     MCC−多々納  02/03/01  在宅薬剤、在宅器材の回数を１
      *                                     回のみへ
      * 02.00.02     NACL-多々納  05/11/24  MONFUNC 対応
      * 03.05.00     NACL-多々納  07/04/XX  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
      *
           03  FLG-ARI             PIC 9(01).
           03  FLG-ARIOK           PIC 9(01).
           03  FLG-SP              PIC 9(01).
      *
           03  FLG-KYUJITU         PIC 9(01).
           03  FLG-GAIKAN          PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *    電話再診有無
           03  FLG-TELSAI          PIC 9(01).
      *    特定薬剤治療管理初回
           03  FLG-SYOKAI          PIC 9(01).
           03  FLG-SYOKAI2         PIC 9(01).
      *
           03  FLG-ADD             PIC 9(01).
      *    特定薬剤当月算定あり
           03  FLG-SYOKAI-TOGTU    PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDM                 PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-Z2              PIC 9(04).
           03  IDX-Z3              PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-T               PIC 9(04).
      *
           03  IDX-MAX             PIC 9(04).
      *
           03  IDX-ZAI             PIC 9(04).
           03  IDX-ZAI1            PIC 9(04).
           03  IDX-ZAI2            PIC 9(04).
           03  IDX-SYU             PIC 9(04).
           03  IDX-SYU2            PIC 9(04).
           03  IDX-GAIKAN          PIC 9(04).
           03  IDX-Y2              PIC 9(02).
           03  IDX-2               PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *    パス名
           03  WRK-DBPATH          PIC X(64).
      *
           03  WRK-IDX             PIC 9(03).
      *
           03  WRK-MOJI            PIC X(01). 
      *    加算エラー分
           03  WRK-KASAN-NASI      PIC X(01). 
      *
           03  WRK-ERRCD           PIC X(04). 
      *
      *
           03  WRK-TENSUKEI        PIC 9(08)V99.
           03  WRK-TENSUKEI-9      PIC 9(08).
           03  WRK-TENSUKEI-G      PIC 9(08).
      *    往診時、所定点数加算分
           03  WRK-TENSU-O1        PIC 9(08).
      *    往診時、所定点数加算外
           03  WRK-TENSU-O2        PIC 9(08).
      *
           03  WRK-TENSU1          PIC 9(08).
           03  WRK-KAISU           PIC 9(04).
      *
           03  WRK-SRYCD           PIC X(09).
      *
           03  WRK-KEIYMD.
               05  WRK-KEI-YY          PIC 9(04).
               05  WRK-KEI-MM          PIC 9(02).
               05  WRK-KEI-DD          PIC 9(02).
      *----(01.00.01) LINE ADD START ----------------------------------
      *    乳幼児の自動加算コード
           03  WRK-NYU-SRYCD           PIC X(09).
      *    入力時表示用
           03  IDX-INCNT               PIC 9(04).
           03  WRK-HEN-INPUTCD         PIC X(22).
      *
           03  WRK-SYOKAI-YMD          PIC X(08).
           03  WRK-SANTEI-CNT          PIC 9(04).
      *    特定薬剤初回算定日
           03  WRK-SANTEI-YMD          PIC X(08).
           03  WRK-WYM.
               05  WRK-WYY             PIC X(02).
               05  WRK-WMM             PIC X(02).
           03  WRK-SYOKAI              PIC 9(04).
      *
      *    保存用ＳＰＡ
       01  HZN-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //HZN-//.
      *
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
      *    特定薬剤管理加算料テーブル
       01  TBL-TOKUYAKU-AREA.
           03  FILLER              PIC X(09)   VALUE   "113000410".
           03  FILLER              PIC X(09)   VALUE   "180000110".
           03  FILLER              PIC X(09)   VALUE   "180000210".
           03  FILLER              PIC X(09)   VALUE   "113000510".
       01  TBL-TOKUYAKU-AREA-R     REDEFINES   TBL-TOKUYAKU-AREA.
           03  TBL-TOKUYAKU-OCC    OCCURS   4  INDEXED  TBL-TOK.
               05  TBL-TOKSRYCD    PIC X(09).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ−
      *01  TENSU-REC.
      *    COPY    "CPTENSU.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    チェックマスタ−
       01  CHK-REC.
           COPY    "CPCHK.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    診療行為算定編集パラメタ
           COPY    "CPORCSC90.INC".
      *
      *    診療行為算定歴チェックパラメタ
           COPY    "CPORCSC91.INC".
      *
      *    診療行為点数編集・基本チェック
           COPY    "CPORCSC92.INC".
      *
      *    画面編集
           COPY    "CPORCSC94.INC".
      *
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
      *    診療行為算定用共通パラメタ
           COPY    "CPORCSC00.INC".
      *
       01  WRK-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-//.
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           ORCSC00AREA
           WRK-SCR-REC
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
      *
           EVALUATE    LNK-ORCSC-KBN
      *        剤別　チェック、編集、自動算定
               WHEN    "1"
                   PERFORM 100-SEL-SYORI-SEC
      *        全体のチェック
               WHEN    "2"
                   PERFORM 200-ALL-SYORI-SEC
      *        自動算定
               WHEN    "3"
                   PERFORM 300-AUTO-SYORI-SEC
               WHEN    OTHER
                   MOVE    "1001"          TO  SPA-ERRCD
           END-EVALUATE
      *
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    剤別のチェック、編集、自動算定処理
      *****************************************************************
       100-SEL-SYORI-SEC                SECTION.
      *
      *    チェック、自動算定
           PERFORM 1001-NAI-CHK-SEC
           IF     (WRK-GMN-INPUTCD(IDX-MAX)(1:1)
                                       =   "."   )  OR
                  (WRK-GMN-INPUTCD(IDX-MAX)
                                       =   SPACE )  OR
                  (SPA-ERRCD       NOT =   SPACE )
               GO      TO      100-SEL-SYORI-EXT
           END-IF
      *
      *    点数計算
           PERFORM 1002-TENSU-KEI-SEC
      *
           .
       100-SEL-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細別内容チェック、自動算定編集処理
      *****************************************************************
       1001-NAI-CHK-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDX-MAX
           MOVE    ZERO                TO  IDX-SYU
           INITIALIZE                  WRK-AREA
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE )   OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *
               ADD     1                   TO  IDX-MAX
      *
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1)     =   "."
                   MOVE    WRK-GMN-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
               ELSE
                   MOVE    WRK-COM-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
                   IF      WRK-COM-INPUTCD(IDX-Z)(1:3) =   "059"
                       MOVE    "7"                     TO  WRK-MOJI
                   END-IF
               END-IF
      *
               EVALUATE    WRK-MOJI
                   WHEN    "0"
      *************WHEN    "8"
                   WHEN    "."
                       CONTINUE
      *
                   WHEN    "1"
      *                手技料
                       PERFORM 10011-SYUGISYORI-SEC
      *
                   WHEN    "8"
      *                コメントのチェック
                       PERFORM 10018-COMMENT-SEC
      *
                   WHEN    "7"
                       IF      WRK-COM-DATAKBN (IDX-Z) =   "3"
      *                    フィルムの時エラー
                           MOVE    "0047"      TO  WRK-ERRCD
                           MOVE    "1"         TO  WRK-COM-CUR (IDX-Z)
                       END-IF
      *
      *            酸素・窒素はエラー
                       IF      WRK-COM-TOKUKIZAISBT1 (IDX-Z) =   2
                           MOVE    "0039"      TO  WRK-ERRCD
                           MOVE    "1"         TO  WRK-COM-CUR  (IDX-Z)
                       END-IF
               END-EVALUATE
      *
      *        初診、再診、指導、在宅の時、回数は１のみ
               IF     (WRK-COM-SRYKBN (IDX-Z)  =   "11"  OR
                                                   "12"  OR
                                                   "13" ) OR
                      (WRK-COM-SRYKBN (IDX-Z)  =   "14" )
      *??????     AND  WRK-COM-SRYSYUKBN(IDX-Z)  = "140" )
                  IF      (WRK-COM-KAIFLG (IDX-Z)  =   1  )  AND
                         (WRK-COM-KAISU(IDX-Z)    >   1  )
                       MOVE    "0008"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE    WRK-ERRCD           TO  SPA-ERRCD
      *
           .
       1001-NAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細別内容チェック、自動算定編集処理
      *****************************************************************
       10011-SYUGISYORI-SEC                  SECTION.
      *
      *    在宅診察料が有る時、再診料を削除する
           IF      LNK-ORCSC-HOUMON-ARI    =   1
               IF      WRK-COM-SRYKBN (IDX-Z)  =   "12"
                   INITIALIZE              WRK-GMN-TBLREC(IDX-Z)
                   INITIALIZE              WRK-COM-TBLREC(IDX-Z)
                   GO    TO      10011-SYUGISYORI-EXT
               END-IF
           END-IF
      *    在宅診察料が有る時、初診料が算定済みであること
           IF      LNK-ORCSC-HOUMON-ARI    =   1
      *********IF      LNK-ORCSC-LSTYMD    =   SPACE
               IF      LNK-ORCSC-SYOYMD    =   SPACE
                   MOVE    "1020"          TO  WRK-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   GO      TO      10011-SYUGISYORI-EXT
               END-IF
           END-IF
      *
      *    老人慢性疾患外来診療料が有る時、外来管理加算を削除する
      *         診察回数が３回目以外の時
           IF     (LNK-ORCSC-ROUMANSEI-ARI     NOT =   ZERO )  OR
                  (LNK-ORCSC-ROUMANSEI-ARI2    NOT =   ZERO  AND
                   LNK-ORCSC-SINSATU-CNT       NOT =   2    )
               IF      WRK-COM-INPUTCD(IDX-Z)  =   "112005610"  OR
                                                   "112701410"  OR
                                                   "112701510"
                   INITIALIZE              WRK-GMN-TBLREC(IDX-Z)
                   INITIALIZE              WRK-COM-TBLREC(IDX-Z)
                   GO    TO      10011-SYUGISYORI-EXT
               END-IF
           END-IF
      *
      *    電話再診が有る時、外来管理加算・継続管理加算を削除する
           IF      LNK-ORCSC-TELSAI-ARI    =   1
               IF      WRK-COM-INPUTCD(IDX-Z)  =   "112005610"  OR
                                                   "112701410"  OR
                                                   "112701510"  OR
                                                   "112007170"  OR
                                                   "112702270"
                   INITIALIZE              WRK-GMN-TBLREC(IDX-Z)
                   INITIALIZE              WRK-COM-TBLREC(IDX-Z)
                   GO    TO      10011-SYUGISYORI-EXT
               END-IF
           END-IF
      *
      *    外来管理加算・継続管理加算の剤は、１明細のみとする
           IF      WRK-KASAN-NASI      =   "1"
               IF      WRK-COM-INPUTCD(IDX-Z)(1:1) =   "1"
                   MOVE    "0059"          TO  WRK-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   GO      TO      10011-SYUGISYORI-EXT
               END-IF
           END-IF
           IF      WRK-COM-INPUTCD(IDX-Z)  =   "112005610"  OR
                                               "112701410"  OR
                                               "112701510"  OR
                                               "112007170"  OR
                                               "112702270"
               MOVE    "1"                 TO  WRK-KASAN-NASI
           END-IF
      *
      *    剤内に手技料は１つのみ
           IF      WRK-COM-DATAKBN (IDX-Z) =   "1"
               IF      IDX-SYU             >   ZERO
                   MOVE    "0019"          TO  WRK-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   GO      TO      10011-SYUGISYORI-EXT
               ELSE
                   MOVE    IDX-Z           TO  IDX-SYU
               END-IF
           END-IF
      *
      *    加算チェック
           IF      WRK-COM-DATAKBN (IDX-Z) =   "2"
               PERFORM 1005-KASAN-CHK-SEC
           END-IF
      *
      *    年齢加算算定チェック
           MOVE    SPACE               TO  WRK-NYU-SRYCD
           PERFORM 1007-NENREI-SYORI-SEC
      *
      *    時間加算チェック
           IF     WRK-COM-TIMEFLG (IDX-Z)  NOT =   ZERO
               PERFORM 1002-KASAN-SET-SEC
           END-IF
      *
      *    時間加算を入力された時、自動振替をする
           IF     (WRK-COM-DATAKBN (IDX-Z)     =   "2"  )  AND
                  (WRK-COM-TIMEKSNKBN(IDX-Z)   =   1  OR
                                                   2  OR
                                                   3  OR
                                                   4     ) AND
                  (WRK-COM-AUTOKBN (IDX-Z)     =   SPACE )
      *        年齢加算自動振り替え処理
               IF      WRK-NYU-SRYCD   NOT =   SPACE
                   PERFORM 1008-NENREI-CHENGE-SEC
               END-IF
           END-IF
      *
      *----(01.00.02) LINE ADD START ----------------------------------
      *    寝たきり老人在宅総合診療科を算定時、加算を追加
           IF      WRK-COM-INPUTCD (IDX-Z)     =   "114701210"  OR
                                                   "114701710"
               PERFORM 1009-NETAKIRI-HEN-SEC
           END-IF
      *----(01.00.02) LINE ADD END   ----------------------------------
      *
      *----(01.00.03) LINE ADD START ----------------------------------
      *    診療情報提供料を入力時、算定日のコメントを自動加算
           IF      WRK-COM-INPUTCD (IDX-Z)     =   "180016110"  OR
                                                   "180016210"  OR
                                                   "180016310"  OR
                                                   "113004410"
               PERFORM 10010-COMMT-SEC
           END-IF
      *
      *    特定薬剤治療加算料チェック
           IF      WRK-COM-INPUTCD (IDX-Z)     =   "113000410"  OR
                                                   "180000110"  OR
                                                   "180000210"  OR
                                                   "113000510"  OR
                                                   "113000770"  OR
                                                   "113000670"
               PERFORM 10011-TOKUYAKU-SEC
           END-IF
      *
           .
       10011-SYUGISYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    加算料チェック処理
      *****************************************************************
       1005-KASAN-CHK-SEC                  SECTION.
      *
      *    加算データの時、前に手技料があること
           MOVE    ZERO                TO  FLG-ARI
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL       IDX1    >=  IDX-Z
               IF     (WRK-COM-INPUTCD(IDX1)(1:1)  =   "1" )  AND
                      (WRK-COM-DATAKBN(IDX1)   NOT =   "2" )
                   IF      WRK-COM-CHUKSNCD (IDX1)     =
                                               WRK-COM-CHUKSNCD(IDX-Z)
                       MOVE    1               TO  FLG-ARIOK
                   END-IF
                   MOVE    1               TO  FLG-ARI
                   MOVE    IDX-Z           TO  IDX1
               END-IF
      *
           END-PERFORM
      *
           IF      FLG-ARI             =   ZERO
               MOVE    "0013"          TO  WRK-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
           END-IF
      *
           .
       1005-KASAN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    時間加算追加処理
      *****************************************************************
       1002-KASAN-SET-SEC              SECTION.
      *
      *    診察の時、他の時間加算もする
           MOVE    WRK-COM-TIMEFLG(IDX-Z)
                                       TO  LNK-ORCSC-TIMEFLG
      *
           MOVE    SPACE               TO  WRK-SRYCD
      *
      *    時間加算
           EVALUATE    WRK-COM-SRYKBN(IDX-Z)
              WHEN     "11"
      *----(01.00.01) LINE UPD START ----------------------------------
      *           初診料加算
                  EVALUATE    WRK-COM-TIMEFLG(IDX-Z)  ALSO
                                                   LNK-ORCSC-ROUJIN
      *            /* 時間外 */
                       WHEN    1       ALSO     0
                           MOVE    "111000570"     TO  WRK-SRYCD
      *            /* 休日 */
                       WHEN    2       ALSO     0
                           MOVE    "111000670"     TO  WRK-SRYCD
      *            /* 深夜 */
                       WHEN    3       ALSO     0
                           MOVE    "111000770"     TO  WRK-SRYCD
      *            /* 時間外 　老人
                       WHEN    1       ALSO     1
                           MOVE    "111700570"     TO  WRK-SRYCD
      *            /* 休日 　老人*/
                       WHEN    2       ALSO     1
                           MOVE    "111700670"     TO  WRK-SRYCD
      *            /* 深夜 　老人*/
                       WHEN    3       ALSO     1
                           MOVE    "111700770"     TO  WRK-SRYCD
                   END-EVALUATE
      *----(01.00.01) LINE UPD END   ----------------------------------
              WHEN     "12"
      *           再診料加算
                  EVALUATE    WRK-COM-TIMEFLG(IDX-Z)  ALSO
                                                      LNK-ORCSC-ROUJIN
      *            /* 時間外 */
                       WHEN    1       ALSO     0
                           MOVE    "112001110"     TO  WRK-SRYCD
      *            /* 休日 */
                       WHEN    2       ALSO     0
                           MOVE    "112001210"     TO  WRK-SRYCD
      *            /* 深夜 */
                       WHEN    3       ALSO     0
                           MOVE    "112001310"     TO  WRK-SRYCD
      *            /* 時間外 　老人
                       WHEN    1       ALSO     1
                           MOVE    "112701010"     TO  WRK-SRYCD
      *            /* 休日 　老人*/
                       WHEN    2       ALSO     1
                           MOVE    "112701110"     TO  WRK-SRYCD
      *            /* 深夜 　老人*/
                       WHEN    3       ALSO     1
                           MOVE    "112701210"     TO  WRK-SRYCD
                   END-EVALUATE
           END-EVALUATE
      *
      *    小児科外来診療科の加算
           IF      WRK-COM-TIMEFLG(IDX-Z)  NOT =   ZERO
               EVALUATE    WRK-COM-INPUTCD (IDX-Z)
      *            初診
                   WHEN    "113003510"
                   WHEN    "113003710"
                       EVALUATE    WRK-COM-TIMEFLG(IDX-Z)
                           WHEN    1
      *                        初診時時間外加算
                               MOVE    "111010470"     TO  WRK-SRYCD
                           WHEN    2
      *                        初診時休日加算
                               MOVE    "111010570"     TO  WRK-SRYCD
                           WHEN    3
      *                        初診時深夜加算
                               MOVE    "111010670"     TO  WRK-SRYCD
                       END-EVALUATE
      *            再診
                   WHEN    "113003610"
                   WHEN    "113003810"
                       EVALUATE    WRK-COM-TIMEFLG(IDX-Z)
                           WHEN    1
      *                        再診時時間外加算
                               MOVE    "112005770"     TO  WRK-SRYCD
                           WHEN    2
      *                        再診時休日加算
                               MOVE    "112005870"     TO  WRK-SRYCD
                           WHEN    3
      *                        再診時深夜加算
                               MOVE    "112005970"     TO  WRK-SRYCD
                       END-EVALUATE
               END-EVALUATE
           END-IF
      *
           IF      WRK-SRYCD            =    SPACE
               GO      TO      1002-KASAN-SET-EXT
           END-IF
      *
      *    時間自動加算　存在チェック
           MOVE    ZERO                TO  FLG-ARI
           MOVE    ZERO                TO  IDX-T
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE   )   OR
                              (FLG-ARI                 =   1    ) OR
                              (WRK-GMN-INPUTCD (IDX1)  =   SPACE )
               IF     (WRK-COM-DATAKBN    (IDX1)       =   "2"  )  AND
                      (WRK-COM-TIMEKSNKBN (IDX1)   NOT =   ZERO )
                    MOVE    1              TO  FLG-ARI
                    MOVE    IDX1           TO  IDX-T
               END-IF
           END-PERFORM
      *    自動追加
           MOVE    IDX-Z               TO  IDX-S
           PERFORM 1100-KASAN-SET-SEC
      *
           IF      FLG-ADD             =   1
      *        年齢加算自動振り替え処理
               IF      WRK-NYU-SRYCD   NOT =   SPACE
                   PERFORM 1008-NENREI-CHENGE-SEC
               END-IF
           END-IF
      *
           .
       1002-KASAN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動加算　存在チェック加処理
      *****************************************************************
       1110-KASAN-CHK-SEC        SECTION.
      *
      *    時間自動加算　存在チェック
           MOVE    ZERO                TO  FLG-ARI
           MOVE    ZERO                TO  IDX-T
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE     ) OR
                              (FLG-ARI                 =   1    ) OR
                              (WRK-GMN-INPUTCD (IDX1)  =   SPACE )
               IF       WRK-COM-INPUTCD (IDX1) =   WRK-SRYCD
                    MOVE    IDX1           TO  IDX-T
                    MOVE    1              TO  FLG-ARI
               END-IF
           END-PERFORM
           .
       1110-KASAN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動加算１行追加処理
      *****************************************************************
       1100-KASAN-SET-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-ADD
      *
           IF      FLG-ARI             =   ZERO
               ADD     1               TO  IDX-S
           ELSE
               MOVE    IDX-T           TO  IDX-S
           END-IF
      *
      *    点数マスタ編集・基本チェック
           MOVE    WRK-SCR-REC         TO  HZN-SCR-REC
           INITIALIZE                  ORCSC92AREA
           IF      FLG-ARI             =   ZERO
               MOVE    "1"                 TO  LNK-SC92-KBN
           ELSE
               MOVE    SPACE               TO  WRK-COM-CHKFLG (IDX-S)
               MOVE    SPACE               TO  WRK-COM-GMNFLG (IDX-S)
           END-IF
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC92-SYORIFLG
           MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
           MOVE    IDX-S               TO  LNK-SC92-GMN-IDX
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    LNK-ORCSC-NENREI    TO  LNK-SC92-NENREI
           MOVE    LNK-ORCSC-ROUJIN    TO  LNK-SC92-ROUJIN
           MOVE    LNK-ORCSC-LSTSRYKA  TO  LNK-SC92-LSTSRYKA
           MOVE    LNK-ORCSC-LSTSRYKAMEI   TO  LNK-SC92-LSTSRYKAMEI
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       WRK-SCR-REC
                                       SPA-AREA
      *
      *    エラーの時、追加なし
           IF     (SPA-ERRCD           NOT =   SPACE )  OR
                  (LNK-SC92-ERRAREA    NOT =   SPACE )
               MOVE    HZN-SCR-REC         TO  WRK-SCR-REC
               MOVE    SPACE               TO  SPA-ERRCD
               IF      FLG-ARI             =   ZERO
                   COMPUTE IDX-S           =   IDX-S
                                           -   1
               END-IF
               GO      TO      1100-KASAN-SET-EXT
           END-IF
      *
           IF      IDX-S               >   1
               MOVE    WRK-COM-SRYKBN (IDX-S - 1)
                                           TO  WRK-COM-SRYKBN(IDX-S)
               MOVE    SPACE               TO  WRK-GMN-SRYKBN(IDX-S)
               MOVE    WRK-COM-SRYSYUKBN (IDX-S - 1)
                                           TO  WRK-COM-SRYSYUKBN(IDX-S)
           END-IF
      *
      *    自動追加分（入力時に追加）
           MOVE    "2"                 TO  WRK-COM-AUTOKBN (IDX-S)
      *
           MOVE    1                   TO  FLG-ADD
      *
           .
       1100-KASAN-SET-EXT.
           EXIT.
           .
      *****************************************************************
      *    年齢加算追加処理
      *****************************************************************
       1007-NENREI-SYORI-SEC           SECTION.
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2        >   4       )   OR
                              (WRK-COM-AGEKSNSRYCD(IDX-Z IDX2)
                                          =   SPACE  OR  ZERO )
      *
               MOVE    WRK-COM-AGEKSNSRYCD (IDX-Z IDX2)  TO  WRK-SRYCD
      *        加算チェック
               PERFORM 1110-KASAN-CHK-SEC
               MOVE    IDX-Z               TO  IDX-S
      *        加算追加
               PERFORM 1100-KASAN-SET-SEC
               IF      FLG-ADD             =   1
                   MOVE    4                   TO  IDX2
                   MOVE    WRK-SRYCD           TO  WRK-NYU-SRYCD
               END-IF
           END-PERFORM
      *
           .
       1007-NYUYOJI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    年齢加算時間外自動振替処理
      *****************************************************************
       1008-NENREI-CHENGE-SEC           SECTION.
      *
      *
           MOVE    WRK-NYU-SRYCD       TO  WRK-SRYCD
      *    加算存在チェック
           PERFORM 1110-KASAN-CHK-SEC
           IF      FLG-ARI             =   ZERO
               GO      TO      1008-NENREI-CHENGE-EXT
           END-IF
      *
           MOVE    IDX-SYU               TO  IDX-S
           EVALUATE    WRK-COM-SRYKBN(IDX-SYU)
               WHEN    "11"
      *            初診
                   MOVE    "111011470"         TO  WRK-SRYCD
                   PERFORM 1100-KASAN-SET-SEC
               WHEN    "12"
      *            再診（乳幼児）（時間加算等）
                   MOVE    "112006970"         TO  WRK-SRYCD
                   PERFORM 1100-KASAN-SET-SEC
                   IF      FLG-ADD             =   ZERO
      *                再診（幼児）（時間加算等）
                       MOVE    "112007070"         TO  WRK-SRYCD
                       PERFORM 1100-KASAN-SET-SEC
                   END-IF
           END-EVALUATE
      *
           .
       1008-NENREI-CHENGE-EXT.
           EXIT.
      *****************************************************************
      *    寝たきり老人在宅総合診療科に加算追加処理
      *****************************************************************
       1009-NETAKIRI-HEN-SEC           SECTION.
      *
      *    既に入力のある時、自動加算しない
      *     ２４時間連携体制加算（１)
           MOVE    "114701870"         TO  WRK-SRYCD
           PERFORM 1110-KASAN-CHK-SEC
           IF      FLG-ARI             =   1
               GO      TO      1009-NETAKIRI-HEN-EXT
           END-IF
      *     ２４時間連携体制加算（２)
           MOVE    "114703970"         TO  WRK-SRYCD
           PERFORM 1110-KASAN-CHK-SEC
           IF      FLG-ARI             =   1
               GO      TO      1009-NETAKIRI-HEN-EXT
           END-IF
      *     ２４時間連携体制加算（３)
           MOVE    "114703770"         TO  WRK-SRYCD
           PERFORM 1110-KASAN-CHK-SEC
           IF      FLG-ARI             =   1
               GO      TO      1009-NETAKIRI-HEN-EXT
           END-IF
      *
      *     ２４時間連携体制加算（１)
           MOVE    "114701870"         TO  WRK-SRYCD
           MOVE    IDX-Z               TO  IDX-S
           PERFORM 1100-KASAN-SET-SEC
           IF      FLG-ADD             =   1
               GO      TO      1009-NETAKIRI-HEN-EXT
           END-IF
      *
      *     ２４時間連携体制加算（２)
           MOVE    "114703970"         TO  WRK-SRYCD
           MOVE    IDX-Z               TO  IDX-S
           PERFORM 1100-KASAN-SET-SEC
           IF      FLG-ADD             =   1
               GO      TO      1009-NETAKIRI-HEN-EXT
           END-IF
      *
      *     ２４時間連携体制加算（３)
           MOVE    "114703770"         TO  WRK-SRYCD
           MOVE    IDX-Z               TO  IDX-S
           PERFORM 1100-KASAN-SET-SEC
           IF      FLG-ADD             =   1
               GO      TO      1009-NETAKIRI-HEN-EXT
           END-IF
      *
           .
       1009-NETAKIRI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療情報提供算定日追加処理
      *****************************************************************
       10010-COMMT-SEC           SECTION.
      *    診療情報提供料算定日
           MOVE    "840000052"         TO  WRK-SRYCD
           PERFORM 1110-KASAN-CHK-SEC
           IF      FLG-ARI             =   1
               GO      TO      10010-COMMT-EXT
           END-IF
      *
           MOVE    IDX-Z               TO  IDX-S
           PERFORM 1100-KASAN-SET-SEC
           IF      FLG-ADD             =   ZERO
               GO      TO      10010-COMMT-EXT
           END-IF
      *    診療日を編集
           MOVE    SPA-SRYYMD(7:2)     TO  WRK-COM-ATAI1 (IDX-S)
      *
      *    画面再編集
           INITIALIZE                  ORCSC94AREA
           MOVE    "1"                 TO  LNK-SC94-KBN
           MOVE    LNK-ORCSC-PG        TO  LNK-SC94-PG
           MOVE    IDX-S               TO  LNK-SC94-IDX
           CALL    "ORCSC94"           USING
                                       ORCSC94AREA
                                       WRK-SCR-REC
                                       SPA-AREA
      *
      *    診療日を編集
           INITIALIZE                  ORCSC92AREA
           MOVE    SPACE               TO  LNK-SC92-KBN
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC92-SYORIFLG
           MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
           MOVE    IDX-S               TO  LNK-SC92-GMN-IDX
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    LNK-ORCSC-NENREI    TO  LNK-SC92-NENREI
           MOVE    LNK-ORCSC-ROUJIN    TO  LNK-SC92-ROUJIN
           MOVE    LNK-ORCSC-LSTSRYKA  TO  LNK-SC92-LSTSRYKA
           MOVE    LNK-ORCSC-LSTSRYKAMEI   TO  LNK-SC92-LSTSRYKAMEI
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       WRK-SCR-REC
                                       SPA-AREA
      *
           .
       10010-COMMT-EXT.
           EXIT.
      *****************************************************************
      *    特定薬剤治療加算料チェック処理
      *****************************************************************
       10011-TOKUYAKU-SEC              SECTION.
      *
           MOVE    ZERO                 TO  IDX-S
      *
      *    初回算定日の編集
           MOVE    SPACE               TO  WRK-SYOKAI-YMD
           MOVE    ZERO                TO  WRK-SYOKAI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   4
      *        算定履歴検索（診療日降順）
               SET     TBL-TOK             TO  IDX
               MOVE    TBL-TOKSRYCD  (TBL-TOK) TO  WRK-SRYCD
               MOVE    "key3"              TO  WRK-DBPATH
               PERFORM 100111-SANTEI-KEN-SEC
               PERFORM
                   UNTIL   FLG-SANTEI          =   1
                   IF     (SANTEI-MSANTEICNT   >   ZERO      ) AND
                          (SANTEI-FSANTEIYMD   <=  SPA-SRYYMD)
                       IF      SANTEI-FSANTEIYMD   >=  WRK-SYOKAI-YMD
                           MOVE    SANTEI-FSANTEIYMD
                                                  TO  WRK-SYOKAI-YMD
                       END-IF
      *                当月算定あり
                       IF      SANTEI-SRYYM        =   SPA-SRYYMD(1:6)
                           MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
                           MOVE    SANTEI-MSANTEICNT   TO  WRK-SYOKAI
                           IF     (LNK-ORCSC-SYORIFLG  =   1  ) AND
                                  (SANTEI-DAY (IDX-Y2) >   ZERO)
                               COMPUTE WRK-SYOKAI      =   WRK-SYOKAI
                                                       -   SANTEI-DAY
                                                              (IDX-Y2)
                           END-IF
                       END-IF
                   END-IF
                   MOVE    "tbl_santei"        TO  MCP-TABLE
                   MOVE    WRK-DBPATH          TO  MCP-PATHNAME
                   PERFORM 920-SANTEI-READ-SEC
               END-PERFORM
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    WRK-DBPATH          TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-PERFORM
      *
      *    訂正の時、初回算定日と診療日が同じ時、初回分とする
           IF     (LNK-ORCSC-SYORIFLG   =   1  )  AND
                  (WRK-SYOKAI-YMD       =   SPA-SRYYMD)
               MOVE    SPACE            TO  WRK-SYOKAI-YMD
           END-IF
      *    すでに初回算定日がある時、初回算定日に入れる
           IF      LNK-ORCSC-TOKTEI-YMD    NOT =   SPACE
               MOVE    LNK-ORCSC-TOKTEI-YMD    TO  WRK-SYOKAI-YMD
           END-IF
      *
      *    特定薬剤治療管理加算がある時、初回とする
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE     )  OR
                              (WRK-GMN-INPUTCD (IDX1)  =   SPACE ) OR
                              (WRK-ERRCD       NOT =   SPACE )
               EVALUATE    WRK-COM-INPUTCD (IDX1)
                   WHEN    "113000770"
      *            特定薬剤治療管理料が当月存在しないこと
                       MOVE    "113000410"         TO  WRK-SRYCD
                       MOVE    "key"               TO  WRK-DBPATH
                       PERFORM 100111-SANTEI-KEN-SEC
                       MOVE    "tbl_santei"        TO  MCP-TABLE
                       MOVE    WRK-DBPATH          TO  MCP-PATHNAME
                       PERFORM 990-DBCLOSE-SEC
                       IF     (FLG-SANTEI          =   1  )  OR
                              (LNK-ORCSC-SYORIFLG  =   1  AND
                               SPA-SRYYMD(7:2)     =   SANTEI-MSANTEID)
                           MOVE    SPACE           TO  WRK-SYOKAI-YMD
                       ELSE
                           MOVE    "0132"      TO  WRK-ERRCD
                           MOVE    "1"         TO  WRK-COM-CUR (IDX-Z)
                       END-IF
               END-EVALUATE
           END-PERFORM
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      10011-TOKUYAKU-EXT
           END-IF
      *
      *    初回時
           IF      WRK-SYOKAI-YMD      =   SPACE
      *********MOVE    SPA-SYSYMD          TO  WRK-SANTEI-YMD
               MOVE    SPA-SRYYMD          TO  WRK-SANTEI-YMD
               EVALUATE    WRK-COM-INPUTCD (IDX-Z)
      *            特定薬剤治療管理料
                   WHEN    "113000410"
      *                特定薬剤治療管理加算１回目自動発生
                       MOVE    "113000770"     TO  WRK-SRYCD
                       PERFORM 1110-KASAN-CHK-SEC
                       IF      FLG-ARI         =   ZERO
                           MOVE    IDX-Z               TO  IDX-S
                           PERFORM 1100-KASAN-SET-SEC
                       ELSE
                           MOVE    IDX-T               TO  IDX-S
                       END-IF
      *                特定薬剤治療管理料初回算定追加
                       MOVE    "840000038"         TO  WRK-SRYCD
                       PERFORM 100112-COMMT1-SEC
      *            特定薬剤治療管理料（ジギタリス製剤等）
                   WHEN    "180000110"
                   WHEN    "180000210"
      *                特定薬剤治療管理加算有無
                       MOVE    "113000770"     TO  WRK-SRYCD
                       PERFORM 1110-KASAN-CHK-SEC
                       IF      FLG-ARI         =   ZERO
                           MOVE    IDX-Z               TO  IDX-S
                       ELSE
                           MOVE    IDX-T               TO  IDX-S
                       END-IF
      *                特定薬剤治療管理料初回算定追加
                       MOVE    "840000038"         TO  WRK-SRYCD
                       PERFORM 100112-COMMT1-SEC
      *            特定薬剤治療管理料（４月目以上）はエラー
                   WHEN    "113000510"
                       MOVE    "0130"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                       GO      TO      10011-TOKUYAKU-EXT
               END-EVALUATE
      *
           END-IF
      *        ２回目以降
      *        Ｋ０２のみ
           IF     (WRK-SYOKAI-YMD  NOT =   SPACE  )  AND
                  (LNK-ORCSC-PG        =   "K02"  )
      *        算定回数計算
               PERFORM 10013-SANTEI-CNT-SEC
      *
               EVALUATE    WRK-COM-INPUTCD (IDX-Z)
      *            特定薬剤治療管理料
                   WHEN    "113000410"
                       IF      WRK-SANTEI-CNT      >=  3
      ***H14.03.08 現在 振替えなしで、警告表示
      *                   ４月目以降へ振り替え
      *                   MOVE    "113000510"          TO  WRK-SRYCD
      *                   MOVE    IDX-Z                TO  IDX-T
      *                   MOVE    1                    TO  FLG-ARI
      *                   PERFORM 1100-KASAN-SET-SEC
      *                   MOVE    SPACE            TO  WRK-COM-AUTOKBN
      *****                                                (IDX-Z)
                          IF      WRK-COM-KZMFLG (IDX-Z)   =   SPACE
                              MOVE    "K007"           TO  WRK-ERRCD
                              MOVE    "1"              TO  WRK-COM-CUR
                                                          (IDX-Z)
                              MOVE    "1"          TO  WRK-COM-KZMFLG
                                                          (IDX-Z)
                           END-IF
                       END-IF
      *            特定薬剤治療管理料（４月目以上）はエラー
                   WHEN    "113000510"
                       IF      WRK-SANTEI-CNT      >=  3
                           CONTINUE
                       ELSE
                           MOVE    "0130"          TO  WRK-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                              (IDX-Z)
                           GO      TO      10011-TOKUYAKU-EXT
                       END-IF
               END-EVALUATE
      *
      *        特定薬剤治療管理料初回算定追加
               MOVE    WRK-SYOKAI-YMD      TO  WRK-SANTEI-YMD
               EVALUATE    WRK-COM-INPUTCD (IDX-Z)
      *            特定薬剤治療管理料
                   WHEN    "113000410"
      *            特定薬剤治療管理料（ジギタリス製剤等）
                   WHEN    "180000110"
                   WHEN    "180000210"
      *            特定薬剤治療管理料（４月目以上）はエラー
                   WHEN    "113000510"
                       IF      WRK-SYOKAI          =   ZERO
                           MOVE    IDX-Z               TO  IDX-S
                           MOVE    "840000038"         TO  WRK-SRYCD
                           PERFORM 100112-COMMT1-SEC
                       END-IF
               END-EVALUATE
           END-IF
      *
      *    測定薬剤名コメント自動発生
           IF     (WRK-COM-INPUTCD (IDX-Z + 1) =   SPACE )  OR
                  (IDX-S                       >    ZERO  AND
                   WRK-COM-INPUTCD (IDX-S + 1) =   SPACE )
               MOVE    "830000014"         TO  WRK-SRYCD
               IF      IDX-S               =   ZERO
                   MOVE    IDX-Z           TO  IDX-S
               END-IF
               PERFORM 100112-COMMT1-SEC
           END-IF
      *    初回算定日をセットする
           MOVE    WRK-SYOKAI-YMD      TO  LNK-ORCSC-TOKTEI-YMD
           .
       10011-TOKUYAKU-EXT.
           EXIT.
      *****************************************************************
      *    特定薬剤治療管理料のコメント追加処理
      *****************************************************************
       100112-COMMT1-SEC           SECTION.
      *
           PERFORM 1110-KASAN-CHK-SEC
           IF      FLG-ARI             =   1
               GO      TO      100112-COMMT1-EXT
           END-IF
      *
      *****MOVE    IDX-Z               TO  IDX-S
           PERFORM 1100-KASAN-SET-SEC
           IF      FLG-ADD             =   ZERO
               GO      TO      100112-COMMT1-EXT
           END-IF
      *    測定薬剤名コメントの追加
           IF      WRK-SRYCD           =   "830000014"
      *        自動追加分を止める
               MOVE    SPACE               TO  WRK-COM-AUTOKBN (IDX-S)
               MOVE    "1"                 TO  WRK-COM-CUR     (IDX-S)
               GO      TO      100112-COMMT1-EXT
           END-IF
      *
      *    特定薬剤治療管理料初回算定コメント
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY1-AREA
           MOVE    "11"                TO  LNK-DAY1-IRAI
           MOVE    WRK-SANTEI-YMD      TO  LNK-DAY1-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY1-AREA
           MOVE    LNK-DAY1-WYMD(2:2)  TO  WRK-COM-ATAI1 (IDX-S)
           MOVE    LNK-DAY1-WYMD(4:2)  TO  WRK-COM-ATAI2 (IDX-S)
      *
      *    画面再編集
           INITIALIZE                  ORCSC94AREA
           MOVE    "1"                 TO  LNK-SC94-KBN
           MOVE    LNK-ORCSC-PG        TO  LNK-SC94-PG
           MOVE    IDX-S               TO  LNK-SC94-IDX
           CALL    "ORCSC94"           USING
                                       ORCSC94AREA
                                       WRK-SCR-REC
                                       SPA-AREA
      *
      *    診療日を編集
           INITIALIZE                  ORCSC92AREA
           MOVE    SPACE               TO  LNK-SC92-KBN
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC92-SYORIFLG
           MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
           MOVE    IDX-S               TO  LNK-SC92-GMN-IDX
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    LNK-ORCSC-NENREI    TO  LNK-SC92-NENREI
           MOVE    LNK-ORCSC-ROUJIN    TO  LNK-SC92-ROUJIN
           MOVE    LNK-ORCSC-LSTSRYKA  TO  LNK-SC92-LSTSRYKA
           MOVE    LNK-ORCSC-LSTSRYKAMEI   TO  LNK-SC92-LSTSRYKAMEI
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       WRK-SCR-REC
                                       SPA-AREA
      *
           .
       100112-COMMT1-EXT.
           EXIT.
      *****************************************************************
      *    特定薬剤治療管理料算定回数計算処理
      *****************************************************************
       10013-SANTEI-CNT-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-SANTEI-CNT
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   4
               SET     TBL-TOK             TO  IDX
      *        算定履歴検索
               INITIALIZE                  SANTEI-REC
               MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
               MOVE    SPA-PTID            TO  SANTEI-PTID
               MOVE    TBL-TOKSRYCD (TBL-TOK)
                                           TO  SANTEI-SRYCD
               MOVE    WRK-SYOKAI-YMD      TO  SANTEI-FSANTEIYMD
      *        算定履歴区分は’１’で保険毎でないこととする
               MOVE    ZERO                TO  SANTEI-NYUGAIKBN
               MOVE    ZERO                TO  SANTEI-SRYKA
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
      *
               MOVE    SANTEI-REC          TO  MCPDATA-REC
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_santei"        TO  MCP-TABLE
                   MOVE    "key4"              TO  MCP-PATHNAME
                   PERFORM 920-SANTEI-READ-SEC
               ELSE
                   MOVE    1                   TO  FLG-SANTEI
               END-IF
               PERFORM
                       UNTIL       FLG-SANTEI      =   1
                   IF      SANTEI-MSANTEICNT   >   ZERO
                       ADD     SANTEI-MSANTEICNT   TO  WRK-SANTEI-CNT
                   END-IF
      *            訂正の時、訂正分は対象外
                   IF     (LNK-ORCSC-SYORIFLG   =   1  )  AND
                          (SANTEI-SRYYM         =   SPA-SRYYMD(1:6))
                     MOVE  ZERO                 TO  IDX-Y2
                     IF    TBL-TOKSRYCD (TBL-TOK)
                                                =   WRK-COM-INPUTCD
                                                         (IDX-Z)
                       MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
                       IF      SANTEI-DAY (IDX-Y2) >   ZERO
                           COMPUTE WRK-SANTEI-CNT  =   WRK-SANTEI-CNT
                                                   -   SANTEI-DAY
                                                             (IDX-Y2)
                       END-IF
                     END-IF
                     ADD     1                 TO  IDX-Y2
                     PERFORM VARYING     IDX-2 FROM   IDX-Y2 BY  1
                             UNTIL       IDX-2 >   31
                           COMPUTE WRK-SANTEI-CNT  =   WRK-SANTEI-CNT
                                                   -   SANTEI-DAY
                                                             (IDX-2)
                     END-PERFORM
                   END-IF
      *            訂正の時、訂正日以降は対象外
                   IF     (LNK-ORCSC-SYORIFLG  =   1  )  AND
                          (SANTEI-SRYYM        >   SPA-SRYYMD(1:6))
                       COMPUTE WRK-SANTEI-CNT  =   WRK-SANTEI-CNT
                                               -   SANTEI-MSANTEICNT
                   END-IF
      *
                   MOVE    "tbl_santei"        TO  MCP-TABLE
                   MOVE    "key4"              TO  MCP-PATHNAME
                   PERFORM 920-SANTEI-READ-SEC
               END-PERFORM
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-PERFORM
           .
       10013-SANTEI-CNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴検索処理
      *****************************************************************
       100111-SANTEI-KEN-SEC                  SECTION.
      *
      *    算定履歴検索
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           IF      WRK-DBPATH          =   "key"
               MOVE    SPA-SRYYMD(1:6)     TO  SANTEI-SRYYM
           END-IF
      *    算定履歴区分は’１’で、保険毎でないこととする
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    WRK-DBPATH          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    WRK-DBPATH          TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           .
       100111-SANTEI-KEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   コメントのチェック処理
      *****************************************************************
       10018-COMMENT-SEC                  SECTION.
      *
      *    特定薬剤治療管理初回算定コメントがあれば初回とする
           IF     (WRK-COM-INPUTCD (IDX-Z) =   "840000038" ) AND
                  (LNK-ORCSC-TOKTEI-YMD    NOT =   SPACE   )
      *        特定開始日和暦変換
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY1-AREA
               MOVE    "11"                    TO  LNK-DAY1-IRAI
               MOVE    LNK-ORCSC-TOKTEI-YMD    TO  LNK-DAY1-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY1-AREA
      *
               MOVE    WRK-COM-ATAI1 (IDX-Z)(1:2)  TO  WRK-WYY
               MOVE    WRK-COM-ATAI2 (IDX-Z)(1:2)  TO  WRK-WMM
      *        初回算定年月が前回初回と違う時システム日を初回算定とする
               IF      WRK-WYM             =   LNK-DAY1-WYMD(2:4)
                   CONTINUE
               ELSE
      *            診療年月和暦変換
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY1-AREA
                   MOVE    "11"                    TO  LNK-DAY1-IRAI
                   MOVE    SPA-SRYYMD              TO  LNK-DAY1-YMD
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY1-AREA
                   IF      WRK-WYM         =   LNK-DAY1-WYMD(2:4)
                       MOVE    SPA-SRYYMD      TO  LNK-ORCSC-TOKTEI-YMD
                   ELSE
                       MOVE    "0133"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   END-IF
               END-IF
           END-IF
      *
      *    特定薬剤治療管理初回算定コメントがあれば初回とする
           IF     (WRK-COM-INPUTCD (IDX-Z) =   "840000038" ) AND
                  (LNK-ORCSC-TOKTEI-YMD    =   SPACE   )
      *
               MOVE    WRK-COM-ATAI1 (IDX-Z)(1:2)  TO  WRK-WYY
               MOVE    WRK-COM-ATAI2 (IDX-Z)(1:2)  TO  WRK-WMM
      *        入力年月を西暦変換
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY1-AREA
               MOVE    "12"                TO  LNK-DAY1-IRAI
               IF      WRK-WYY                 >   "60"
                   MOVE    "3"                 TO  LNK-DAY1-WYMD(1:1)
               ELSE
                   MOVE    "4"                 TO  LNK-DAY1-WYMD(1:1)
               END-IF
               MOVE    WRK-WYM             TO  LNK-DAY1-WYMD(2:4)
               MOVE    "01"                TO  LNK-DAY1-WYMD(6:2)
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY1-AREA
               MOVE    LNK-DAY1-YMD        TO  WRK-SYMD
               IF      SPA-SRYYMD(1:6)     >   WRK-SYMD(1:6)
                   MOVE    WRK-SYMD            TO  LNK-ORCSC-TOKTEI-YMD
               ELSE
                   IF      SPA-SRYYMD(1:6)     <   WRK-SYMD(1:6)
                       MOVE    "0135"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   END-IF
               END-IF
           END-IF
      *
      *
           .
       10018-COMMENT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    点数計算編集処理
      *****************************************************************
       1002-TENSU-KEI-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDX-MAX
           MOVE    ZERO                TO  IDX-SYU
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  ORCSC90AREA
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE  )   OR
                              (SPA-ERRCD       NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *
               ADD     1                   TO  IDX-MAX
      *
               MOVE    SPACE               TO  WRK-GMN-SURYO (IDX-Z)
               MOVE    SPACE               TO  WRK-GMN-TENSU (IDX-Z)
               MOVE    SPACE               TO  WRK-GMN-KAISU (IDX-Z)
               MOVE    ZERO                TO  WRK-GMN-KEI   (IDX-Z)
      *
      *
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1)     =   "."
                   MOVE    WRK-GMN-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
               ELSE
                   MOVE    WRK-COM-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
                   IF      WRK-COM-INPUTCD(IDX-Z)(1:3) =   "059"
                       MOVE    "7"                     TO  WRK-MOJI
                   END-IF
               END-IF
      *
               EVALUATE    WRK-MOJI
                   WHEN    "0"
                   WHEN    "8"
                   WHEN    "."
                       CONTINUE
      *
                   WHEN    OTHER
      *                編集チェックへ
                       PERFORM 10021-KEISAN-SEC
               END-EVALUATE
      *
           END-PERFORM
      *
      *    最終行
      *    薬剤点数計算
           MOVE    "2"                 TO  LNK-SC90-KBN
           MOVE    IDX-MAX             TO  LNK-SC90-IDX
           CALL    "ORCSC90"           USING
                                       ORCSC90AREA
                                       WRK-SCR-REC
      *
      *    往診料編集
           IF      WRK-COM-SRYKBN (IDX-MAX)    =   "14"
               MOVE    WRK-TENSU-O1        TO  LNK-SC90-TENSUKEI
           END-IF
      *    割合加算
           PERFORM 1006-KASAN-SET-SEC
      *
           COMPUTE LNK-SC90-TENSUKEI   =   LNK-SC90-TENSUKEI
                                       +   LNK-SC90-TENGAKEI
                                       +   WRK-TENSU-O2
      *    各点数編集
           MOVE    LNK-SC90-TENSUKEI   TO  WRK-COM-SYUTEN1 (IDX-MAX)
           COMPUTE WRK-COM-YKZTEN (IDX-MAX)
                                       =   LNK-SC90-ZAITEN
                                       +   LNK-SC90-GAITEN
      *    器材点数
           MOVE    LNK-SC90-KIZTEN     TO  WRK-COM-KIZAITEN (IDX-MAX)
      *
      *    点数計　計算
           COMPUTE WRK-COM-TENSU (IDX-MAX) =   LNK-SC90-TENSUKEI
                                           +   LNK-SC90-ZAITEN
                                           +   LNK-SC90-GAITEN
                                           +   LNK-SC90-KIZTEN
      *    計
           COMPUTE WRK-COM-KEI (IDX-MAX)   =   WRK-COM-TENSU  (IDX-MAX)
                                           *   LNK-ORCSC-KAISU
      *
           .
       1002-TENSU-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数計算処理
      *****************************************************************
       10021-KEISAN-SEC                  SECTION.
      *
           MOVE    LNK-ORCSC-KAISU     TO  LNK-SC90-KAISU
           MOVE    "1"                 TO  LNK-SC90-KBN
           MOVE    IDX-Z               TO  LNK-SC90-IDX
           CALL    "ORCSC90"           USING
                                       ORCSC90AREA
                                       WRK-SCR-REC
      *
      *    往診料の時、往診区分により、点数集計を変更する
           IF     (WRK-COM-INPUTCD (IDX-Z)(1:1)    =   "1"  ) AND
                  (WRK-COM-SRYKBN      (IDX-Z)     =   "14" )  AND
                  (WRK-COM-TENSIKIBETU (IDX-Z) NOT =   5    )  AND
                  (WRK-COM-TUSOKUGAIKBN(IDX-Z) NOT =   "1"  )
               EVALUATE    WRK-COM-OSINKBN (IDX-Z)
                   WHEN    1
                   WHEN    2
                   WHEN    6
                       ADD     WRK-COM-KISOTEN (IDX-Z) TO  WRK-TENSU-O1
                   WHEN    OTHER
                       ADD     WRK-COM-KISOTEN (IDX-Z) TO  WRK-TENSU-O2
               END-EVALUATE
           END-IF
      *
      *    手技料判定
           IF      (WRK-MOJI                   =   "1"  )  AND
                   (WRK-COM-DATAKBN (IDX-Z)    =   "1"  )
               MOVE    IDX-Z               TO  IDX-SYU
           END-IF
      *
           .
       10021-KEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    割合加算計算処理
      *****************************************************************
       1006-KASAN-SET-SEC              SECTION.
      *
           MOVE    ZERO                TO  WRK-TENSUKEI-G
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE )   OR
                              (WRK-GMN-INPUTCD (IDX)   =   SPACE )
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   5   )
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   COMPUTE WRK-TENSUKEI        =   LNK-SC90-TENSUKEI
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.5
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-G
               END-IF
           END-PERFORM
      *
           COMPUTE LNK-SC90-TENSUKEI   =   LNK-SC90-TENSUKEI
                                       +   WRK-TENSUKEI-G
      *
           .
       1006-KASAN-SET-EXT.
           EXIT.
      *****************************************************************
      *    全診療行為チェック処理
      *****************************************************************
       200-ALL-SYORI-SEC                SECTION.
      *
      *    重複チェック
           PERFORM 2000-JYUFUKU-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      200-ALL-SYORI-EXT
           END-IF
      *
           MOVE    ZERO                TO  IDX-ZAI
           MOVE    ZERO                TO  IDX-ZAI1
           MOVE    ZERO                TO  IDX-ZAI2
           MOVE    ZERO                TO  IDX-SYU
           MOVE    ZERO                TO  IDX-SYU2
           MOVE    ZERO                TO  FLG-GAIKAN
           MOVE    ZERO                TO  IDX-GAIKAN
           MOVE    ZERO                TO  LNK-ORCSC-CHK-IDX
      *
           MOVE    ZERO                TO  FLG-SYOKAI
           MOVE    ZERO                TO  FLG-SYOKAI2
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE   )   OR
                              (SPA-ERRCD           NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      ******** IF     (WRK-COM-ZAIEND(IDX-Z)   =    "1"  ) AND
      *               (WRK-COM-SRYKBN(IDX-Z)   =    "11"  OR  "12")
      *            ADD     1               TO  IDX-ZAI
      ******** END-IF
               IF    ( WRK-COM-ZAIEND(IDX-Z)   NOT =    "1"   )  OR
      *              外来管理加算、継続加算以外のチェック
                     ( WRK-COM-INPUTCD(IDX-Z)  =    "112005610"   OR
                                                    "112701410"   OR
                                                    "112701510"   OR
                                                    "112007170"   OR
                                                    "112702270" )
                   CONTINUE
               ELSE
                   IF      WRK-COM-DATAKBN(IDX-Z)  =    "1"
                       IF      WRK-COM-SRYKBN(IDX-Z)   =    "11"
                           ADD     1               TO  IDX-ZAI1
                           MOVE   IDX-Z            TO  IDX-SYU2
                       END-IF
                       IF      WRK-COM-SRYKBN(IDX-Z)   =    "12"
                           ADD     1               TO  IDX-ZAI2
                           MOVE   IDX-Z            TO  IDX-SYU2
                       END-IF
                   END-IF
               END-IF
      *
               IF     (WRK-COM-SRYKBN (IDX-Z)  =   "11"  OR "12" ) AND
                      (WRK-COM-DATAKBN(IDX-Z)  =   "1"           )
      *            同日再診の判定
                   IF      WRK-COM-INPUTCD(IDX-Z)  =    "112700550" OR
                                                        "112700850" OR
                                                        "112003250" OR
                                                        "112003350" OR
      *                外来管理加算、継続加算を削除
                                                        "112005610" OR
                                                        "112701410" OR
                                                        "112701510" OR
                                                        "112007170" OR
                                                        "112702270"
      *                同日電話再診
                                                    OR  "112003050"
                                                    OR  "112003150"
                                                    OR  "112700650"
                                                    OR  "112700950"
                       CONTINUE
                   ELSE
                       IF      LNK-ORCSC-CHK-IDX   >   ZERO
                           MOVE    "0017"          TO  SPA-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                              (IDX-Z)
                       ELSE
                           MOVE    IDX-Z       TO  LNK-ORCSC-CHK-IDX
                       END-IF
                   END-IF
               END-IF
      *        併用算定チェック
      *?0608   IF      WRK-COM-SDOKNRYO (IDX-Z)    NOT =   ZERO
      *************PERFORM 220-SDOKNRYO-CHK-SEC
      *???     END-IF
      *
      *        外来管理加算算定チェック
               IF     (WRK-COM-SRYKBN (IDX-Z)  =    "12" )  AND
                      (WRK-COM-DATAKBN(IDX-Z)  =    "1"  )
                   MOVE    IDX-Z               TO  IDX-SYU
               END-IF
      *
      *         外来管理加算算定
                IF      WRK-COM-INPUTCD(IDX-Z) =   "112005610"  OR
                                                   "112701410"  OR
                                                   "112701510"
      *            外来管理加算は１つのみ
                   IF      IDX-GAIKAN      >   ZERO
                       MOVE    "0058"          TO  SPA-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   ELSE
                       MOVE    IDX-Z           TO  IDX-GAIKAN
                    END-IF
                END-IF
                IF     (WRK-COM-SRYKBN     (IDX-Z)
                                           NOT =   "95" AND "96") AND
                       (WRK-COM-GAIKANRIKBN(IDX-Z)
                                               =   1   )
                   MOVE    1               TO  FLG-GAIKAN
                END-IF
      * 
      *         指導料・初診日チェック
                IF      WRK-COM-INPUTCD(IDX-Z) =   "113001810"  OR
                                                   "113001910"  OR
                                                   "113002010"  OR
                                                   "113700610"  OR
                                                   "113700710"  OR
                                                   "113700810" 
                   PERFORM 230-SIDOU-SYOSIN-SEC
               END-IF
      * 
      *         老人慢性疾病外来総合診療科チェック
                IF      WRK-COM-INPUTCD(IDX-Z) =   "113701010"  OR
                                                   "113701110"  OR
                                                   "113701510" 
                   PERFORM 230-ROUMANSEI-SYOSIN-SEC
               END-IF
      *
      *        小児科外来診療科チェック
      *            初診
               IF      WRK-COM-INPUTCD(IDX-Z) =   "113003510"  OR
                                                  "113003710"
                   ADD     1               TO  IDX-ZAI1
               END-IF
      *            再診
               IF      WRK-COM-INPUTCD(IDX-Z) =   "113003610"  OR
                                                  "113003810"
                   ADD     1               TO  IDX-ZAI2
               END-IF
      *
      *        特定薬剤治療管理初回算定コメントチェック
               IF      WRK-COM-INPUTCD (IDX-Z) =   "840000038"
      *            特定開始日和暦変換
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY1-AREA
                   MOVE    "11"                TO  LNK-DAY1-IRAI
                   MOVE    SPA-SRYYMD          TO  LNK-DAY1-YMD
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY1-AREA
                   MOVE    WRK-COM-ATAI1 (IDX-Z)(1:2)  TO  WRK-WYY
                   MOVE    WRK-COM-ATAI2 (IDX-Z)(1:2)  TO  WRK-WMM
                   IF      WRK-WYM             =   LNK-DAY1-WYMD(2:4)
                       MOVE    1                   TO  FLG-SYOKAI
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      200-ALL-SYORI-EXT
           END-IF
      *    初診料・再診料の重複チェック
           IF     (IDX-ZAI1            >   ZERO) AND
                  (IDX-ZAI2            >   ZERO)
               MOVE    "0017"          TO  SPA-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR (IDX-SYU2)
               GO      TO      200-ALL-SYORI-EXT
           END-IF
           IF     (IDX-ZAI1            >   1   ) OR
                  (IDX-ZAI2            >   1   )
               MOVE    "0017"          TO  SPA-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR (IDX-SYU2)
               GO      TO      200-ALL-SYORI-EXT
           END-IF
      *    新規患者入力の時、必ず初診料であること
           IF     (LNK-ORCSC-PG        =   "K02"  )  AND
                  (LNK-ORCSC-SYOSIN    =   1      )
               IF     (IDX-ZAI1            =   ZERO) AND
                      (IDX-ZAI2            =   1   )
                   MOVE    "1017"          TO  SPA-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (IDX-SYU2)
                   GO      TO      200-ALL-SYORI-EXT
               END-IF
           END-IF
      *
      *    初診・再診料の算定チェック
      *    訂正の時、チェックなし
           IF      LNK-ORCSC-SYORIFLG  =   ZERO
               IF     (LNK-ORCSC-CHK-IDX   >   ZERO       )  AND
                      (LNK-ORCSC-LSTYMD    =   SPA-SRYYMD )
                   MOVE    "0101"              TO  LNK-ORCSC-KIDCD
                   GO      TO      200-ALL-SYORI-EXT
               END-IF
           END-IF
      *
      *    外来管理加算の算定チェック
           IF      FLG-GAIKAN          =   1
               IF      IDX-GAIKAN          >   ZERO
                   MOVE    "0102"              TO  LNK-ORCSC-KIDCD
                   MOVE    IDX-GAIKAN          TO  LNK-ORCSC-CHK-IDX
               END-IF
           ELSE
               IF     (IDX-SYU             >   ZERO )  AND
                      (IDX-GAIKAN          =   ZERO )  AND
                      (LNK-ORCSC-GAIFLG    =   ZERO )
                   IF     (LNK-ORCSC-ROUMANSEI-ARI     NOT =   ZERO ) OR
                          (LNK-ORCSC-ROUMANSEI-ARI2    NOT =   ZERO  AND
                           LNK-ORCSC-SINSATU-CNT       NOT =   3    )
                       CONTINUE
                   ELSE
                       MOVE    IDX-SYU         TO  IDX-Z
      *                外来管理加算コード追加処理
                       PERFORM 2001-GAIKASAN-SEC
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        特定薬剤治療管理料の算定チェック
               PERFORM 20010-TOKUTEI-ALLCHK-SEC
           END-IF
           .
       200-ALL-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    重複チェック処理
      *****************************************************************
       2000-JYUFUKU-CHK-SEC               SECTION.
      *
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE   )   OR
                              (SPA-ERRCD           NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
               IF      WRK-COM-SRYKBN(IDX-Z)(1:1)  =   "1"
      *
                   COMPUTE IDX-Z3          =   IDX-Z   +   1
                   PERFORM VARYING     IDX-Z2  FROM    IDX-Z3 BY  1
                           UNTIL      (IDX-Z2      >   SPA-MAX-LINE) OR
                                      (SPA-ERRCD   NOT =   SPACE )  OR
                                      (WRK-GMN-INPUTCD (IDX-Z2)
                                                       =   SPACE )
                       IF     (WRK-COM-SRYKBN  (IDX-Z)
                                           =   WRK-COM-SRYKBN(IDX-Z2))
                         AND  (WRK-COM-INPUTCD (IDX-Z)
                                           =   WRK-COM-INPUTCD(IDX-Z2))
                           MOVE    "0134"          TO  SPA-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                              (IDX-Z2)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           .
       2000-JYUFUKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    外来管理加算コード追加処理
      *****************************************************************
       2001-GAIKASAN-SEC                  SECTION.
      *
      *    外来管理加算コード追加
           EVALUATE    LNK-ORCSC-ROUJIN   ALSO   SPA-HOSPSBT
      *        老人　病院
               WHEN    1       ALSO          1
                   MOVE    "112701410"         TO  WRK-SRYCD
      *        老人、診療所
               WHEN    1       ALSO          2
                   MOVE    "112701510"         TO  WRK-SRYCD
      *        老人外
               WHEN    ZERO    ALSO          ANY
                   MOVE    "112005610"         TO  WRK-SRYCD
           END-EVALUATE
      *
           COMPUTE IDX-S               =   IDX-Z   +  1
      *
      *    点数マスタ編集・基本チェック
           INITIALIZE                  ORCSC92AREA
           MOVE    "1"                 TO  LNK-SC92-KBN
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC92-SYORIFLG
           MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
           MOVE    IDX-S               TO  LNK-SC92-GMN-IDX
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    LNK-ORCSC-NENREI    TO  LNK-SC92-NENREI
           MOVE    LNK-ORCSC-ROUJIN    TO  LNK-SC92-ROUJIN
           MOVE    LNK-ORCSC-LSTSRYKA  TO  LNK-SC92-LSTSRYKA
           MOVE    LNK-ORCSC-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       WRK-SCR-REC
                                       SPA-AREA
      *
      *    エラーの時、追加なし
           IF     (SPA-ERRCD           NOT =   SPACE )  OR
                  (LNK-SC92-ERRAREA    NOT =   SPACE )
               MOVE    SPACE           TO  SPA-ERRCD
               INITIALIZE              WRK-GMN-TBLREC (IDX-S)
               INITIALIZE              WRK-COM-TBLREC (IDX-S)
               COMPUTE IDX-S           =   IDX-S
                                       -   1
               GO      TO      2001-GAIKASAN-EXT
           END-IF
      *
           MOVE    WRK-COM-SRYKBN (IDX-S - 1)
                                       TO  WRK-COM-SRYKBN(IDX-S)
           MOVE    SPACE               TO  WRK-GMN-SRYKBN(IDX-S)
           MOVE    WRK-COM-SRYSYUKBN (IDX-S - 1)
                                       TO  WRK-COM-SRYSYUKBN(IDX-S)
      *
      *    自動追加分（入力時に追加）
           MOVE    "1"                 TO  WRK-COM-AUTOKBN (IDX-S)
      *
           ADD     1                   TO  WRK-GMN-MAX
           .
       2001-GAIKASAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    指導料初診日チェック処理
      *****************************************************************
       230-SIDOU-SYOSIN-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-KYUJITU
           IF      LNK-ORCSC-SYOYMD    =   SPACE
               MOVE    SPA-SRYYMD          TO  WRK-KEIYMD
           ELSE
      *        初診料算定日＋１月後
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    LNK-ORCSC-SYOYMD    TO  LNK-DAY6-KIJUN
               MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    1                   TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KOYOMI     TO  WRK-KEIYMD
               IF      LNK-DAY6-KOYOMI NOT =   LNK-DAY6-KOYOMIZEN
                   MOVE    1                   TO  FLG-KYUJITU
               END-IF
           END-IF
      *
           EVALUATE    TRUE
               WHEN    WRK-KEIYMD          <   SPA-SRYYMD
                   CONTINUE
               WHEN    WRK-KEIYMD          >   SPA-SRYYMD
                   MOVE    "0034"          TO  SPA-ERRCD
               WHEN    WRK-KEIYMD          =   SPA-SRYYMD
                   IF      FLG-KYUJITU     =   ZERO
                       MOVE    "0034"          TO  SPA-ERRCD
                   ELSE
                       IF      WRK-COM-KZMFLG (IDX-Z)  =   SPACE
                           MOVE    "1"             TO  WRK-COM-KZMFLG
                                                                (IDX-Z)
                           MOVE    "K002"          TO  SPA-ERRCD
                       END-IF
                   END-IF
           END-EVALUATE
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "1"             TO  WRK-COM-CUR  (IDX-Z)
           END-IF
           .
       230-SIDOU-SYOSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人慢性疾患外来総合診察料との初診日チェック
      *****************************************************************
       230-ROUMANSEI-SYOSIN-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-KYUJITU
           IF      LNK-ORCSC-SYOYMD    =   SPACE
               MOVE    SPA-SRYYMD          TO  WRK-KEIYMD
           ELSE
      *        初診料算定日＋１月後
               MOVE    LNK-ORCSC-SYOYMD    TO  WRK-KEIYMD
               COMPUTE WRK-KEI-MM          =   WRK-KEI-MM
                                           +   1
               IF      WRK-KEI-MM          >   12
                   MOVE    1                   TO  WRK-KEI-MM
                   COMPUTE WRK-KEI-YY          =   WRK-KEI-YY
                                               +   1
               END-IF
           END-IF
           IF      LNK-ORCSC-SYOYMD(7:2)   =   "01"
      *        初診料算定日が１日の時、翌月から
               IF      WRK-KEIYMD(1:6)     <=  SPA-SRYYMD(1:6)
                   CONTINUE
               ELSE
                   MOVE    "0034"              TO  SPA-ERRCD
               END-IF
           ELSE
      *        初診料算定日が１日以外の時、翌々月から
               IF      WRK-KEIYMD(1:6)     <   SPA-SRYYMD(1:6)
                   CONTINUE
               ELSE
                   MOVE    "0065"            TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "1"             TO  WRK-COM-CUR  (IDX-Z)
           END-IF
           .
       230-ROUMANSEI-SYOSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    特定薬剤治療管理料の算定チェック処理
      *****************************************************************
       20010-TOKUTEI-ALLCHK-SEC                SECTION.
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE )  OR
                              (SPA-ERRCD   NOT =   SPACE  )  OR
                              (WRK-GMN-INPUTCD(IDX-Z)  =   SPACE  AND
                               WRK-COM-INPUTCD(IDX-Z)  =   SPACE  )
      *        特定薬剤治療加算料チェック
               IF      WRK-COM-INPUTCD (IDX-Z)     =   "180000110"  OR
                                                       "180000210"
                   MOVE    ZERO                TO  FLG-SYOKAI2
                   IF      LNK-ORCSC-TOKTEI-YMD    NOT  =  SPACE
      *                算定履歴検索
                       MOVE    WRK-COM-INPUTCD (IDX-Z) TO  WRK-SRYCD
                       MOVE    "key3"              TO  WRK-DBPATH
                       PERFORM 100111-SANTEI-KEN-SEC
                       MOVE    "tbl_santei"        TO  MCP-TABLE
                       MOVE    WRK-DBPATH          TO  MCP-PATHNAME
                       PERFORM 990-DBCLOSE-SEC
                       IF      FLG-SANTEI          =   ZERO
      *                    訂正の時、当日分はチェックしない
                           IF     (LNK-ORCSC-SYORIFLG   =   1  )  AND
                                  (SANTEI-SRYYM         =   SPA-SRYYMD
                                                            (1:6))
                               MOVE    SPA-SRYYMD(7:2) TO  IDX-Y2
                               IF      SANTEI-DAY (IDX-Y2) >   ZERO
                                   MOVE    1           TO  FLG-SYOKAI2
                               END-IF
                           END-IF
      *                    今回の初回算定日より前の算定日の時はＯＫ
                           IF     (LNK-ORCSC-TOKTEI-YMD
                                                 >   SANTEI-FSANTEIYMD)
                               OR (FLG-SYOKAI    =   1     )
                               OR (FLG-SYOKAI2   =   1     )
                               CONTINUE
                           ELSE
                               MOVE    "0131"          TO  SPA-ERRCD
                               MOVE    "1"             TO  WRK-COM-CUR
                                                            (IDX-Z)
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       20010-TOKUTEI-ALLCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動算定処理
      *****************************************************************
       300-AUTO-SYORI-SEC                SECTION.
      *
           .
       300-AUTO-SYORI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    算定歴マスタ読み込み
      *****************************************************************
       920-SANTEI-READ-SEC         SECTION.
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SANTEI-REC
               MOVE    ZERO                TO  FLG-SANTEI
           ELSE
               INITIALIZE                      SANTEI-REC
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           .
       920-SANTEI-READ-EXT.
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
      *
      *
