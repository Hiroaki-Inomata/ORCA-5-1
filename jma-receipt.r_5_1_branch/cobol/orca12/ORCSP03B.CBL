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
       PROGRAM-ID.             ORCSP03B.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 公費入力チェックサブ（Ｐ０２Ｗ１のサブ）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/06/27    MCC-多々納    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-多々納  02/09/09  平成１４年１０月改正対応
      * 01.00.02     NACL-多々納  02/10/03  負担者番号からの公費種類設定修正
      * 01.00.03     NACL-多々納  02/10/29  負担者番号の全角対応
      * 01.00.04     NACL-多々納  03/02/14  法別番号チェック区分追加
      * 01.00.05     NACL-多々納  03/04/03  公費変更チェック修正
      * 01.00.06     NACL-多々納  03/07/08  保険者異動日チェック追加
      * 01.00.07     NACL-多々納  03/08/07  公費負担者番号チェック修正
      * 01.00.08     NACL-多々納  03/09/02  公費自動切替え追加
      * 01.00.09     NACL-多々納  03/12/16  ”５１”チェックに”７０１”追加
      * 01.00.10     NACL-多々納  05/01/13  有効開始日の初期設定追加
      * 01.00.11     NACL-多々納  05/02/10  特定の重複登録可能
      * 01.00.12     NACL-多々納  05/11/15  MONFUNC 対応
      * 01.00.13     NACL-多々納  06/03/10  '015更正','016育成'重複可
      * 03.00.01     NACL-多々納  06/08/25  平成１８年１０月改正対応
      *                                     （老人割合変更）
      * 03.04.00     NACL-多々納  06/10/30  受給者検証番号警告追加
      * 03.05.00     NACL-多々納  07/05/XX  グループ診療対応
      * 04.02.00     NACL-多々納  08/03/XX  平成２０年４月改正対応
      * 04.07.00     NACL-多々納  11/07/01  公費一覧表示対応
      * 04.07.00     NACL-多々納  12/09/25  生活保護重複対応
      * 04.07.00     NACL-多々納  12/11/16  長期高額療養重複不可
      * 04.07.00     NACL-多々納  13/11/13  生活保護重複警告対応
      * 04.07.00     NACL-多々納  14/12/XX  難病等対応、公費名称編集修正
      * 04.08.00     NACL-多々納  16/02/XX  京都府　京都子重複可対応
      * 05.00.00     NACL-多々納  18/11/XX  092 肝がん治療対応
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
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC X(03).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-KENSYO          PIC 9(01).
           03  FLG-RJN             PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-CHK-ERR         PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-OK2             PIC 9(01).
           03  FLG-KAKUTEI         PIC 9(01).
      *
           03  FLG-AUTO            PIC 9(01).
      *
           03  FLG-HKNNUM-CHK      PIC 9(01).
      *    新規区分
           03  FLG-KOHINS          PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX-K               PIC 9(04).
           03  IDX-K1              PIC 9(04).
           03  IDX-K2              PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDK2                PIC 9(02).
           03  IDX2                PIC 9(02).
           03  IDX3                PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
      *     エラー位置
           03  WRK-ERR-CUR         PIC 9(02).
      *
           03  WRK-ERRMSG          PIC X(40).
           03  WRK-GMN-LINE        PIC 9(04).
           03  WRK-EDANUM          PIC 9(04).
      *
           03  WRK-N-IDX           PIC 9(04).
      *
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-BASEYMD.
               05  WRK-BYY         PIC 9(04).
               05  WRK-BMM         PIC 9(02).
               05  WRK-BDD         PIC 9(02).
           03  WRK-YMD65.
               05  WRK-B65YY       PIC 9(04).
               05  WRK-B65MM       PIC 9(02).
               05  WRK-B65DD       PIC 9(02).
           03  WRK-YMD70.
               05  WRK-B70YY       PIC 9(04).
               05  WRK-B70MM       PIC 9(02).
               05  WRK-B70DD       PIC 9(02).
      *
           03  WRK-KENSAKU-AREA.
               05  WRK-KEN-HKNNUM    PIC X(03).
               05  WRK-KEN-TEKSTYMD  PIC X(08).
               05  WRK-KEN-TEKEDYMD  PIC X(08).
               05  WRK-KEN-ERFLG     PIC X(01).
      *    検証番号チェック
           03  WRK-MOJ-AREA.
               05  WRK-MOJ-MAX     PIC 9(02).
               05  WRK-MOJ-NXT     PIC 9(02).
      *    年齢算出
           03  WRK-NENREI.
               05  WRK-NENREI-YY   PIC 9(03).
           03  WRK-YMDS1.
               05  WRK-YYS1        PIC 9(04).
               05  WRK-MMS1        PIC 9(02).
               05  WRK-DDS1        PIC 9(02).
           03  WRK-YMDS2.
               05  WRK-YYS2        PIC 9(04).
               05  WRK-MMS2        PIC 9(02).
               05  WRK-DDS2        PIC 9(02).
           03  WRK-STYEAR          PIC 9(03).
           03  WRK-EDYEAR          PIC 9(03).
      *
           03  WRK-IDX-AREA.
               05  WRK-IDX         PIC 9(04)  OCCURS   3.
               05  WRK-IDX1        PIC 9(04).
               05  WRK-IDX2        PIC 9(04).
               05  WRK-IDX-SIN1    PIC 9(04).
               05  WRK-IDX-SIN2    PIC 9(04).
      *
           03  WRK-MAE-KOHNUM      PIC X(03).
           03  WRK-MAE-KOHKBN      PIC X(01).
      *
           03  WRK-HBTNUM-1         PIC X(02).
           03  WRK-HBTNUM-2         PIC X(02).
      *    保険適用期間チェック
           03  WRK-HKN-TEKSTYMD        PIC X(08).
           03  WRK-HKN-TEKEDYMD        PIC X(08).
      *
           03  WRK-HKN2-TEKSTYMD        PIC X(08).
           03  WRK-HKN2-TEKEDYMD        PIC X(08).
      *
           03  WRK-CUR                 PIC 9(02).
      *    法別番号チェックフラグ
           03  WRK-KOHHBTNUMKBN        PIC X(01).
      *    Ｈ１８．１０改正
           03  WRK-TEKEDYMD            PIC X(08).
           03  WRK-TEKEDYMD-W          PIC X(10).
      *ver.4.7
      *    法別チェック
           03  WRK-HBTNUM              PIC X(02).
           03  WRK-CHK-KOHNUM          PIC X(03).
           03  WRK-HBTCNT              PIC 9(02).
           03  WRK-MAE-HBTNUM          PIC X(02).
      *H26.12
      *    公費種別編集
           03  WRK-HKNNUM              PIC X(03).
           03  WRK-PAYKBN              PIC X(02).
           03  WRK-HKNKOHSBTKBN        PIC X(01).
           03  WRK-TANSEIDONAME        PIC X(20).
           03  WRK-TEKSTYMD            PIC X(08).
           03  WRK-HZN-STYMD           PIC X(08).
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
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    保険者号マスタ
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
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
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   チェックデジット
           COPY    "CPORCCHKDGT.INC".
      *
      *   画面日付変換サブ
          COPY    "CPORCSGDAY.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    保険年齢・負担割合算定サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
      *
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
      *    患者登録スパ領域
           COPY    "P02COMMON-SPA".
      *    患者画面スパ領域
           COPY    "P02SCR-SPA".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           SPA-P02SCRAREA
           SPA-P02KYOUTU
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
      *    老人公費追加（Ｈ１４．１０改正分）
           IF      SPA-KOH-ADD         =   1
               MOVE    1                   TO  IDX-K
               PERFORM 3101-RJN-KOHADD-SEC
           END-IF
      *
           MOVE    ZERO                TO  SPA-KOH-ADD
           EVALUATE    SPA-PIDCD
      *        公費重複期間処理
               WHEN    "0106"
                   PERFORM 300-KOHKIKAN-SYORI-SEC
                   MOVE    SPACE           TO  SPA-PIDCD
      *        公費終了期間処理
               WHEN    "0107"
                   PERFORM 310-KOHEDYMD-SYORI-SEC
                   MOVE    SPACE           TO  SPA-PIDCD
      *        老人公費切替え処理
               WHEN    "0111"
                   PERFORM 320-RJNKOH-SYORI-SEC
                   MOVE    SPACE           TO  SPA-PIDCD
           END-EVALUATE
           IF      SPA-KOH-ADD         =   1
               GO      TO      000-PROC-EXT
           END-IF
      *
           PERFORM VARYING     IDX-K   FROM    1   BY  1
                   UNTIL      (IDX-K       >   3   )  OR
                              (SPA-ERRCD   NOT =   SPACE )  OR
                              (SPA-PIDCD   NOT =   SPACE )
                           OR (SPA-FLG-END         =   1     )
      *        第三者行為なし
               IF      SPA-GMN-KOHNUM(IDX-K) =   "970"
                   CONTINUE
               ELSE
      *
      *        基本チェック
               PERFORM 100-KIHON-CHK-SEC
               IF     (SPA-ERRCD       NOT =   SPACE )  OR
                      (SPA-PIDCD       NOT =   SPACE )
                   COMPUTE SPA-GMN-CUR     =   SPA-GMN-CUR
                                           +  ((IDX-K  -   1  )  *  6 )
                   MOVE    IDX-K           TO  SPA-KOH-ERRIDX
               END-IF
      *
               END-IF
           END-PERFORM
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-PIDCD       NOT =   SPACE )
               GO      TO      000-PROC-EXT
           END-IF
           IF     (SPA-FLG-END         =   1     )
               GO      TO      000-PROC-EXT
           END-IF
      *
      *    確定処理の時、関連チェックを行う
           IF     (FLG-AUTO            =   ZERO )
      *
           PERFORM VARYING     IDX-K   FROM    1   BY  1
                   UNTIL      (IDX-K       >   3   )  OR
                              (SPA-ERRCD   NOT =   SPACE )  OR
                              (SPA-PIDCD   NOT =   SPACE )
               PERFORM 200-KANREN-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   COMPUTE SPA-GMN-CUR     =   SPA-GMN-CUR
                                           +  ((IDX-K  -   1  )  *  6 )
               END-IF
               IF     (SPA-ERRCD       NOT =   SPACE )  OR
                      (SPA-PIDCD       NOT =   SPACE )
                   MOVE    IDX-K           TO  SPA-KOH-ERRIDX
               END-IF
           END-PERFORM
      *
           END-IF
      *    自動振替えの時、警告は表示しない
           IF     (FLG-AUTO            =   1     )  AND
                  (SPA-ERRCD(1:1)      =   "K"   )
               MOVE    SPACE               TO  SPA-ERRCD
               MOVE    WRK-CUR             TO  SPA-GMN-CUR
           END-IF
      *
      *    すべてのエラーがない時、公費期間重複チェック
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-PIDCD           =   SPACE )
               IF      SPA-HKNKOH-FLG      =   1
                   PERFORM 300-KANREN-KOH-CHECK-SEC
               END-IF
           END-IF
      *
           .
       000-PROC-EXT.
           EXIT.
      *
           EXIT    PROGRAM
           .
      *
      ****************************************************************
      *    基本チェック処理
      ****************************************************************
       100-KIHON-CHK-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-KOHINS
           MOVE    SPA-NAI-PTKOH-EDANUM (IDX-K)    TO  IDX-K2
           IF     (IDX-K2              =   ZERO      ) OR
                  (SPA-PTKOH-SINKIKBN (IDX-K2)
                                       =    "1"      )
               MOVE    1                   TO  FLG-KOHINS
           END-IF
      *
      *    負担者番号
           IF      SPA-ERRCD           =   SPACE
               PERFORM 1001-FTNJANUM-CHK-SEC
           END-IF
           IF     (SPA-FLG-END         =   1     )
               GO      TO      100-KIHON-CHK-EXT
           END-IF
      *
      *    公費
           IF      SPA-ERRCD           =   SPACE
               PERFORM 1001-KOHNUM-CHK-SEC
           END-IF
      *    受給者番号
           IF      SPA-ERRCD           =   SPACE
               PERFORM 1001-JKYSNUM-CHK-SEC
           END-IF
      *    適用開始日
           IF      SPA-ERRCD           =   SPACE
              PERFORM 1001-KTEKSTYMD-CHK-SEC
           END-IF
      *    適用終了日
           IF      SPA-ERRCD           =   SPACE
               PERFORM 1001-KTEKEDYMD-CHK-SEC
           END-IF
      *    確認日
           IF      SPA-ERRCD           =   SPACE
               PERFORM 1001-KKAKUNINYMD-CHK-SEC
           END-IF
           .
       100-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    負担者番号入力処理
      *****************************************************************
       1001-FTNJANUM-CHK-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-MAE-KOHNUM
      *
      *    桁数チェック
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-FTNJANUM (IDX-K)
                                       TO  KANACHK-MAE-INPUT
           MOVE    8                   TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-FTNJANUM (IDX-K)
      *    全角の時、すべてのチェックをはずす
           IF     (KANACHK-RC          =   ZERO  )  AND
                  (KANACHK-SYUBETU     =   2     )
      *        老人公費の場合、全角はエラーとする
               IF      SPA-GMN-KOHNUM (IDX-K)  =   "027"
                   MOVE    "0095"              TO  SPA-ERRCD
                   MOVE    20                  TO  SPA-GMN-CUR
               END-IF
           ELSE
      *    半角で間に空白がないこと
               IF     (KANACHK-RC      NOT =   ZERO  )  OR
                      (KANACHK-RC2     NOT =   ZERO  )  OR
                      (KANACHK-SYUBETU     =   2     )
                   IF      SPA-ERRCD           =   SPACE
                       MOVE    "0071"              TO  SPA-ERRCD
                   END-IF
                   MOVE    20                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF     (SPA-GMN-FTNJANUM (IDX-K)  NOT =   SPACE ) AND
                 ((KANACHK-MAX         <   1   )  OR
                  (KANACHK-MAX         >   8   ))
               IF      SPA-ERRCD           =   SPACE
                   MOVE    "0071"              TO  SPA-ERRCD
               END-IF
               MOVE    20                  TO  SPA-GMN-CUR
           END-IF
      *    老人公費は８桁であること
           IF     (SPA-GMN-FTNJANUM (IDX-K)  NOT =   SPACE ) AND
                  (SPA-GMN-KOHNUM   (IDX-K)  =   "027" ) AND
                  (KANACHK-MAX         NOT =   8     )
               IF      SPA-ERRCD           =   SPACE
                   MOVE    "0071"              TO  SPA-ERRCD
               END-IF
               MOVE    20                  TO  SPA-GMN-CUR
           END-IF
      *
           IF     (SPA-GMN-FTNJANUM (IDX-K)  NOT =   SPACE ) AND
                  (SPA-ERRCD           =   SPACE )  AND
                  (SPA-NAI-PTKOH-KEIFLG(IDX-K)   =   SPACE  )
               PERFORM 10011-FTNJANUM-DB-CHK-SEC
           END-IF
      *
           .
       1001-FTNJANUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    法別番号チェック区分編集処理
      *****************************************************************
       10011-FTNJANUM-DB-CHK-SEC           SECTION.
      *
      *    保険者番号　検索チェック
           INITIALIZE                    HKNJAINF-REC
           MOVE    SPA-HOSPNUM           TO  HKNJA-HOSPNUM
           MOVE    SPA-GMN-FTNJANUM (IDX-K)
                                         TO  HKNJA-HKNJANUM
           MOVE    HKNJAINF-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknjainf"        TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknjainf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-HKNJAINF-READ-SEC
           ELSE
               INITIALIZE                  HKNJAINF-REC
               MOVE    1               TO  FLG-HKNJAINF
           END-IF
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-HKNJAINF        =   ZERO
      *        保険判定
               IF     (HKNJA-HKNNUM            =   "027"  )  OR
                      (SPA-MAE-KOHNUM (IDX-K)  =   HKNJA-HKNNUM)
                   CONTINUE
               ELSE
                   PERFORM 101100-HKNNUM-CHK-SEC
      *            保険の種類は保険者マスタなしとする
                   IF      FLG-HKNNUM-CHK      =   1
                       MOVE    1               TO  FLG-HKNJAINF
                   END-IF
               END-IF
           END-IF
      *    保険者番号あり
           IF      FLG-HKNJAINF        =   ZERO
               MOVE    HKNJA-HKNNUM        TO  WRK-MAE-KOHNUM
               MOVE    HKNJA-IDOYMD        TO  SPA-PTKOH-IDOYMD (IDX-K)
      *
      *ver.4.7
      *        新規で負担者番号再入力時
      *        IF     (FLG-KOHINS          =   1     )
      *           AND (SPA-GMN-KOHNUM (IDX-K) NOT =   SPACE )
      *           AND (SPA-GMN-KOHNUM (IDX-K) NOT =   WRK-MAE-KOHNUM)
      *            PERFORM 10110-KOHHBTNUMKBN-SEC
      *            IF     (WRK-KOHHBTNUMKBN    =   ZERO )
      *              OR   (WRK-MAE-HBTNUM      =   SPA-GMN-FTNJANUM
      *                                                  (IDX-K) (1:2))
      *                CONTINUE
      *            ELSE
      *                負担者番号の公費へ
      *                MOVE    SPACE           TO  SPA-GMN-KOHNUM(IDX-K)
      *            END-IF
      *****    END-IF
      *
           ELSE
               MOVE    "1"                 TO  SPA-NAI-PTKOH-KEIFLG
                                                               (IDX-K)
               PERFORM 10110-KOHHBTNUMKBN-SEC
               IF      WRK-KOHHBTNUMKBN    =   ZERO
      *            公費が入力済みで法別番号チェクなしの時
                   MOVE    SPA-MAE-KOHNUM (IDX-K)  TO  WRK-MAE-KOHNUM
               ELSE
      *
               EVALUATE    SPA-GMN-FTNJANUM(IDX-K)(1:2)
      *            老人
                   WHEN    "27"
                       MOVE    "027"              TO  WRK-MAE-KOHNUM
      *            特定
                   WHEN    "51"
                       EVALUATE    SPA-GMN-FTNJANUM(IDX-K)(5:3)
                           WHEN    "601"
                               MOVE    "091"      TO  WRK-MAE-KOHNUM
                           WHEN    "602"
                               MOVE    "051"      TO  WRK-MAE-KOHNUM
                           WHEN    "701"
                               MOVE    "091"      TO  WRK-MAE-KOHNUM
                           WHEN    "301"
      *                        要網等
                               MOVE    "091"      TO  WRK-MAE-KOHNUM
                           WHEN    "101"
      *                        要網等
                               MOVE    "091"      TO  WRK-MAE-KOHNUM
                           WHEN    OTHER
      *                        負担者番号警告
                               IF      SPA-NAI-FTNCHK (IDX-K)  =   ZERO
                                   MOVE    "K083"     TO  SPA-ERRCD
                                   MOVE    20         TO  SPA-GMN-CUR
                                   MOVE    1          TO  SPA-NAI-FTNCHK
                                                                (IDX-K)
      *ver.4.7                     警告後の一覧表示の為
                                   IF     (SPA-NAI-KOHHBTKBN-FLG
                                                       =   "1"   )
                                       MOVE    SPACE
                                               TO  SPA-NAI-PTKOH-KEIFLG
                                                               (IDX-K)
                                   END-IF
      *
                               ELSE
      *                            法別番号より公費種類を決定する
                                   PERFORM 10011-HKNNUM-SET-SEC
                               END-IF
                       END-EVALUATE
      *H30.11
      *            肝炎治療、肝がん治療
                   WHEN    "38"
                       EVALUATE    SPA-GMN-FTNJANUM(IDX-K)(5:3)
      *                    肝炎治療
                           WHEN    "601"
                               MOVE    "038"      TO  WRK-MAE-KOHNUM
                           WHEN    "602"
      *                    肝がん治療
                               MOVE    "092"      TO  WRK-MAE-KOHNUM
                           WHEN    OTHER
      *                        法別番号より公費種類を決定する
                               PERFORM 10011-HKNNUM-SET-SEC
                       END-EVALUATE
      *
      *            その他
                   WHEN    OTHER
      *                法別番号より公費種類を決定する
                       PERFORM 10011-HKNNUM-SET-SEC
               END-EVALUATE
      *
               END-IF
           END-IF
      *
           IF     (WRK-MAE-KOHNUM      =   SPACE )  OR
                  (SPA-ERRCD       NOT =   SPACE )
            OR    (SPA-FLG-END         =   1     )
               CONTINUE
           ELSE
      *        更新の時、保険区分変更時、エラーとする
      **       MOVE    SPA-NAI-PTKOH-EDANUM(IDX-K) TO  IDX-K2
      *        IF     (SPA-NAI-PTKOH-KOHID (IDX-K) NOT =   ZERO  )
      *         AND   (SPA-MAE-KOHNUM (IDX-K)
      ***                                  NOT =   SPACE )
               IF      FLG-KOHINS          =   ZERO
                   IF      WRK-MAE-KOHNUM  NOT =   SPA-MAE-KOHNUM(IDX-K)
                       PERFORM 10011-HKNNUM-HENCHK-SEC
                   END-IF
               ELSE
                   IF      SPA-GMN-KOHNUM(IDX-K)   =   SPACE
                       MOVE    WRK-MAE-KOHNUM  TO  SPA-GMN-KOHNUM(IDX-K)
                       MOVE    SPACE           TO  SPA-GMN-KOHKBN(IDX-K)
      *            Ｈ１４．１０以降、老人の初期値は０１とする
                       IF      (SPA-SYSYMD         >=  "20021001" ) AND
                               (WRK-MAE-KOHNUM     =   "027"      )
                           MOVE    "1"         TO  SPA-GMN-KOHKBN(IDX-K)
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
       10011-FTNJANUM-DB-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    法別番号チェック区分編集処理
      *****************************************************************
       101100-HKNNUM-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNNUM-CHK
           MOVE    SPACE               TO  HKNNUM-REC
           MOVE    HKNJA-HOSPNUM       TO  HKN-HOSPNUM
           MOVE    HKNJA-HKNNUM        TO  HKN-HKNNUM
           MOVE    ZERO                TO  HKN-PAYKBN
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 921-HKNMST-NEXT-SEC
           ELSE
               INITIALIZE              HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-HKNNUM          =   ZERO
      *        保険公費種別区分
               IF      HKN-HKNKOHSBTKBN    =   "1"  OR  "4"
                                           OR  "8"  OR  "9"
                   MOVE    1               TO  FLG-HKNNUM-CHK
               END-IF
           END-IF
      *
           .
       101100-HKNNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    法別番号チェック区分編集処理
      *****************************************************************
       10110-KOHHBTNUMKBN-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-KOHHBTNUMKBN
           MOVE    SPACE               TO  WRK-MAE-HBTNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-KOHMAX
               IF     (SPA-GMN-KOHNUM(IDX-K)
                                      =    SPA-GMN-KOHNUML (IDX)) AND
                      (SPA-GMN-KOHKBN(IDX-K)
                                      =    SPA-GMN-KOHKBNL (IDX))
      *            法別番号チェック区分
                   MOVE    SPA-NAI-KOHHBTNUMKBN (IDX)
                                           TO  WRK-KOHHBTNUMKBN
      *            法別番号
                   MOVE    SPA-NAI-HBTNUM (IDX)
                                           TO  WRK-MAE-HBTNUM
               END-IF
           END-PERFORM
      *
           .
       10110-KOHHBTNUMKBN-EXT.
           EXIT.
      *
      *****************************************************************
      *    法別番号より公費種類処理
      *****************************************************************
       10011-HKNNUM-SET-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-KOHHBTNUMKBN
           MOVE    SPACE               TO  WRK-MAE-HBTNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-KOHMAX
               IF     (SPA-MAE-KOHNUM(IDX-K)
                                      =    SPA-GMN-KOHNUML (IDX)) AND
                      (SPA-MAE-KOHKBN(IDX-K)
                                      =    SPA-GMN-KOHKBNL (IDX))
      *            法別番号チェック区分
                   MOVE    SPA-NAI-KOHHBTNUMKBN (IDX)
                                           TO  WRK-KOHHBTNUMKBN
                   MOVE    SPA-NAI-HBTNUM (IDX)
                                           TO  WRK-MAE-HBTNUM
               END-IF
           END-PERFORM
      *    法別番号チェック区分をしない
           IF      WRK-KOHHBTNUMKBN    =   ZERO
               MOVE    SPA-MAE-KOHNUM(IDX-K)   TO  WRK-MAE-KOHNUM
               GO      TO      10011-HKNNUM-SET-EXT
           END-IF
      *
      *ver.4.7
      *    負担者番号の法別番号から保険番号マスタの公費番号の判定
           MOVE    ZERO                TO  WRK-HBTCNT
           MOVE    SPACE               TO  WRK-CHK-KOHNUM
           MOVE    SPA-GMN-FTNJANUM(IDX-K) (1:2)
                                       TO  WRK-HBTNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-KOHMAX
               IF      WRK-HBTNUM      =   SPA-NAI-HBTNUM (IDX)
                   ADD     1               TO  WRK-HBTCNT
                   IF      WRK-HBTCNT      =   1
                       MOVE    SPA-GMN-KOHNUML (IDX)
                                               TO  WRK-CHK-KOHNUM
                   END-IF
               END-IF
           END-PERFORM
           IF      SPA-MAE-KOHNUM (IDX-K)  =   SPACE
               MOVE    WRK-CHK-KOHNUM      TO  WRK-MAE-KOHNUM
      *        公費法別一覧編集区分
               IF     (SPA-GMN-KOHNUM (IDX-K)  =   SPACE )
                 AND  (SPA-NAI-KOHHBTKBN-FLG   =   "1"   )
                   IF     (WRK-HBTCNT          >   1   )
      *                同じ制度が複数ある
                       PERFORM 10011-P02H-SYORI-SEC
                   END-IF
               END-IF
           ELSE
      *        公費種類の変更なし
               IF      WRK-CHK-KOHNUM      =   SPA-MAE-KOHNUM (IDX-K)
                   MOVE    SPACE           TO  WRK-MAE-KOHNUM
               ELSE
                   MOVE    WRK-CHK-KOHNUM  TO  WRK-MAE-KOHNUM
               END-IF
           END-IF
      *
      *!!!
           GO      TO    10011-HKNNUM-SET-EXT
      *
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    SPA-GMN-FTNJANUM(IDX-K) (1:2)
                                       TO  HKN-HBTNUM
           MOVE    SPA-SYSYMD          TO  HKN-TEKSTYMD
           MOVE    SPA-SYSYMD          TO  HKN-TEKEDYMD
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"       TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 921-HKNMST-NEXT-SEC
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1                   TO  FLG-HKNNUM
           END-IF
           MOVE    ZERO                TO  FLG-OK
           PERFORM UNTIL     FLG-HKNNUM    =   1
             IF      HKN-HKNKOHSBTKBN    =   "1"  OR  "4"
                                         OR  "8"  OR  "9"
                 CONTINUE
             ELSE
      *
               IF      SPA-MAE-KOHNUM (IDX-K)  NOT =   SPACE
                   IF      HKN-HKNNUM      =   SPA-MAE-KOHNUM (IDX-K)
                       MOVE    1               TO  FLG-OK
                       MOVE    1               TO  FLG-HKNNUM
                   ELSE
                        MOVE    HKN-HKNNUM      TO  WRK-MAE-KOHNUM
                   END-IF
               ELSE
                   MOVE    HKN-HKNNUM      TO  WRK-MAE-KOHNUM
                   MOVE    1               TO  FLG-HKNNUM
               END-IF
             END-IF
      *
               IF      FLG-HKNNUM          =   ZERO
                   MOVE    "tbl_hknnum"       TO  MCP-TABLE
                   MOVE    "key4"              TO  MCP-PATHNAME
                   PERFORM 921-HKNMST-NEXT-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_hknnum"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF     FLG-OK               =   1
               MOVE    SPACE           TO  WRK-MAE-KOHNUM
           END-IF
      *
           .
       10011-HKNNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費法別一覧編集画面処理
      *****************************************************************
       10011-P02H-SYORI-SEC           SECTION.
      *
           INITIALIZE                  SPA-P02H-AREA
           MOVE    WRK-HBTNUM          TO  SPA-P02H-HBTNUM
           MOVE    IDX-K               TO  SPA-P02H-KOHIDX
           MOVE    1                   TO  SPA-FLG-END
      *
           .
       10011-P02H-SYORI-EXT.
           EXIT.
      *
      ****************************************************************
      *    変更時保険の種類チェック処理
      ****************************************************************
       10011-HKNNUM-HENCHK-SEC     SECTION.
      *
           MOVE    SPACE               TO  WRK-HBTNUM-1
           MOVE    SPACE               TO  WRK-HBTNUM-2
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-KOHMAX
               IF      WRK-MAE-KOHNUM
                                      =    SPA-GMN-KOHNUML (IDX)
                    MOVE    SPA-NAI-HBTNUM (IDX)
                                             TO  WRK-HBTNUM-1
               END-IF
               IF      SPA-GMN-KOHNUML(IDX)  =   SPA-MAE-KOHNUM(IDX-K)
                    MOVE    SPA-NAI-HBTNUM (IDX)
                                             TO  WRK-HBTNUM-2
               END-IF
           END-PERFORM
           IF      WRK-HBTNUM-1    NOT =   WRK-HBTNUM-2
               MOVE    "0074"          TO  SPA-ERRCD
               MOVE    20              TO  SPA-GMN-CUR
           END-IF
      *
          .
       10011-HKNNUM-HENCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費の種類チェック処理
      *****************************************************************
       1001-KOHNUM-CHK-SEC           SECTION.
      *
      *    更新の時、保険区分変更時、エラーとする
      ***  MOVE    SPA-NAI-PTKOH-EDANUM (IDX-K)    TO  IDX-K2
           MOVE    ZERO                TO  FLG-CHK
           IF     (FLG-KOHINS          =   ZERO    )  AND
                  (SPA-MAE-KOHNUM (IDX-K)  NOT =   SPACE )
               IF      SPA-GMN-KOHNUM(IDX-K)
                                           =   SPA-MAE-KOHNUM(IDX-K)
                   IF     (SPA-MAE-KOHKBN (IDX-K)  NOT =   SPACE) AND
                          (SPA-GMN-KOHKBN(IDX-K)   NOT =
                                               SPA-MAE-KOHKBN(IDX-K))
                       MOVE    1               TO  FLG-CHK
                   END-IF
               ELSE
                   MOVE    SPA-GMN-KOHNUM(IDX-K)   TO  WRK-MAE-KOHNUM
                   PERFORM 10011-HKNNUM-HENCHK-SEC
      *************MOVE    1               TO  FLG-CHK
               END-IF
               IF     (FLG-CHK             =   1 )  OR
                      (SPA-ERRCD       NOT =   SPACE)
      *            前の公費へ戻す
                   MOVE    SPA-MAE-KOHNUM-G (IDX-K)
                                           TO  SPA-GMN-KOHNUM-G(IDX-K)
                   MOVE    "0074"          TO  SPA-ERRCD
                   MOVE    22              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      1001-KOHNUM-CHK-EXT
           END-IF
      *
           IF      SPA-GMN-KOHNUM(IDX-K)   =   SPACE
               GO      TO      1001-KOHNUM-CHK-EXT
           END-IF
      *
      *!９６１ 減免金額は仕様決定までエラーとする
      *    IF      SPA-GMN-KOHNUM(IDX-K)   =   "961"
      *        MOVE    "2074"          TO  SPA-ERRCD
      *        MOVE    22              TO  SPA-GMN-CUR
      *        GO      TO      1001-KOHNUM-CHK-EXT
      *    END-IF
      *!
      *    新規の時、チェック・編集
           IF     (FLG-KOHINS          =   1   )
               PERFORM 41011-HKNNUM-SYORI-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   MOVE    21              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       1001-KOHNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    公費の種類選択チェック・編集処理
      *****************************************************************
       41011-HKNNUM-SYORI-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-KOHMAX
               IF     (SPA-GMN-KOHNUM(IDX-K)
                                      =    SPA-GMN-KOHNUML (IDX)) AND
                      (SPA-GMN-KOHKBN(IDX-K)
                                      =    SPA-GMN-KOHKBNL (IDX))
      *            法別番号が同じこと
                   IF     (SPA-GMN-FTNJANUM(IDX-K) NOT =   SPACE )
                     AND  (SPA-GMN-FTNJANUM(IDX-K) (1:2)   NUMERIC)
      *                法別番号チェック区分
                       IF     (SPA-NAI-KOHHBTNUMKBN (IDX)  =   "1" ) AND
                              (SPA-GMN-FTNJANUM(IDX-K) (1:2)
                                           NOT =  SPA-NAI-HBTNUM
                                                              (IDX) )
                           MOVE    "0092"              TO  SPA-ERRCD
                       END-IF
                   END-IF
      *
      *            日付なしのみ
                   IF      SPA-NAI-KTEKSTYMD(IDX-K)    =   SPACE
      *
                   MOVE    SPA-GMN-KOHNUMLST (IDX)
                                           TO  SPA-GMN-KOHNUM-G(IDX-K)
                   MOVE    SPA-NAI-KOHSBTKBN (IDX)
                                           TO  SPA-NAI-PTKOH-SBTKBN
                                                              (IDX-K)
                   END-IF
      *
                   MOVE    SPA-NAI-KOHPAYKBN (IDX)
                                           TO  SPA-NAI-PTKOH-PAYKBN
                                                              (IDX-K)
                   MOVE    SPA-GMN-KOHMAX      TO  IDX
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41011-HKNNUM-SYORI-EXT
           END-IF
      *    ２７の老人公費で支払区分のない時
           IF     (FLG-OK              =   ZERO      )  AND
                  (SPA-GMN-KOHNUM(IDX-K)   =   "027" AND
                   SPA-GMN-KOHKBN(IDX-K)   =   SPACE )  AND
                  (SPA-NAI-KTEKSTYMD(IDX-K) <  "20021001" )
               MOVE    "3"                 TO  SPA-NAI-PTKOH-SBTKBN
                                                              (IDX-K)
               MOVE    SPACE               TO  SPA-NAI-PTKOH-PAYKBN
                                                              (IDX-K)
               MOVE    1                   TO  FLG-OK
           END-IF
           IF      FLG-OK              =   ZERO
               MOVE    "0072"              TO  SPA-ERRCD
           END-IF
      *
      *    老人保健で適用開始日未入力時、年齢チェックを行う
           IF     (SPA-NAI-PTKOH-SBTKBN(IDX-K) =   "3"  ) AND
                  (SPA-GMN-KTEKSTYMD (IDX-K)   =   SPACE) AND
                  (SPA-NAI-BIRTHDAY        NOT =   SPACE)
               PERFORM 410111-RJN-SYSYMD-CHK-SEC
           END-IF
      *
           .
       41011-HKNNUM-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    老人年齢チェック（システム日付）処理
      *****************************************************************
       410111-RJN-SYSYMD-CHK-SEC       SECTION.
      *
           IF     (SPA-NAI-KTEKSTYMD(IDX-K) >=   "20021001" )  OR
                  (SPA-NAI-KTEKSTYMD(IDX-K) =   SPACE   AND
                   SPA-SYSYMD               >=  "20021001"  )
      *        Ｈ１４．１０以降
               PERFORM 4101112-RJN-SYSYMDCHK-10-SEC
           ELSE
      *        Ｈ１４．１０まで
               PERFORM 4101111-RJN-SYSYMDCHK-09-SEC
           END-IF
      *
           .
       410111-RJN-SYSYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    老人年齢チェック（システム日付）処理(Ｈ１４．１０まで)
      *****************************************************************
       4101111-RJN-SYSYMDCHK-09-SEC    SECTION.
      *
      *    生年月日から基準日を求める
      *    １日ならば生年月日、１日以外ならば生年月日の翌月１日
           MOVE    SPA-NAI-BIRTHDAY     TO  WRK-BASEYMD
           IF      SPA-NAI-BIRTHDAY (7:2)   NOT =   "01"
               PERFORM 4910-BIRTH-AFTER1MON-GET-SEC
               IF      SPA-ERRCD        NOT =   SPACE
                   GO  TO               4101111-RJN-SYSYMDCHK-09-EXT
               END-IF
           END-IF
      *    ６５歳を迎える日
           MOVE    WRK-BASEYMD         TO  WRK-YMD65
           COMPUTE WRK-B65YY           =   WRK-B65YY   +   65
      *    ６５歳を迎える日−１月を対象とする
           COMPUTE WRK-B65MM           =   WRK-B65MM   -   1
           IF      WRK-B65MM           =   ZERO
               COMPUTE WRK-B65YY           =   WRK-B65YY   -   1
               MOVE    12                  TO  WRK-B65MM
           END-IF
      *
           EVALUATE    TRUE
      *    エラー
               WHEN    ( SPA-SYSYMD  <   WRK-YMD65 )
                   MOVE    "0087"           TO  SPA-ERRCD
                   MOVE    21               TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       4101111-RJN-SYSYMDCHK-09-EXT.
           EXIT.
      *****************************************************************
      *    老人年齢チェック（システム日付）処理(Ｈ１４．１０以降)
      *****************************************************************
       4101112-RJN-SYSYMDCHK-10-SEC     SECTION.
      *
      *    生年月日から基準日を求める
      *    １日ならば生年月日、１日以外ならば生年月日の翌月１日
           MOVE    SPA-NAI-BIRTHDAY     TO  WRK-BASEYMD
           IF      SPA-NAI-BIRTHDAY (7:2)   NOT =   "01"
               PERFORM 4910-BIRTH-AFTER1MON-GET-SEC
               IF      SPA-ERRCD        NOT =   SPACE
                   GO  TO               4101112-RJN-SYSYMDCHK-10-EXT
               END-IF
           END-IF
      *    ６５歳を迎える日
           MOVE    WRK-BASEYMD         TO  WRK-YMD65
           COMPUTE WRK-B65YY           =   WRK-B65YY   +   65
      *    ６５歳を迎える日−１月を対象とする
           COMPUTE WRK-B65MM           =   WRK-B65MM   -   1
           IF      WRK-B65MM           =   ZERO
               COMPUTE WRK-B65YY           =   WRK-B65YY   -   1
               MOVE    12                  TO  WRK-B65MM
           END-IF
      *
           EVALUATE    TRUE
      *    エラー
               WHEN    ( SPA-SYSYMD  <   WRK-YMD65 )
                   MOVE    "0087"           TO  SPA-ERRCD
                   MOVE    21               TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       4101112-RJN-SYSYMDCHK-10-EXT.
           EXIT.
      *****************************************************************
      *    受給者番号入力　処理
      *****************************************************************
       1001-JKYSNUM-CHK-SEC           SECTION.
      *
           IF      SPA-GMN-JKYSNUM (IDX-K) =   SPACE
               GO      TO      1001-JKYSNUM-CHK-EXT
           END-IF
      *
      *    半角・全角のチェックと桁数判定
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-JKYSNUM (IDX-K)
                                       TO  KANACHK-MAE-INPUT
           MOVE    20                  TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-JKYSNUM (IDX-K)
           IF      KANACHK-RC      NOT =   ZERO
      *        半角・全角混在エラー
               MOVE    "0075"              TO  SPA-ERRCD
               MOVE    22                  TO  SPA-GMN-CUR
               GO      TO          1001-JKYSNUM-CHK-EXT
           END-IF
           IF      KANACHK-RC2         =   8
      *        入力文字エラー
               MOVE    "0076"         TO  SPA-ERRCD
               MOVE    22                  TO  SPA-GMN-CUR
               GO  TO                 1001-JKYSNUM-CHK-EXT
           END-IF
      *****IF      KANACHK-MAX         <=  1
      *        MOVE    "0076"              TO  SPA-ERRCD
      *        MOVE    22                  TO  SPA-GMN-CUR
      *****END-IF
      *    全角の時、すべてのチェックをはずす
           IF     (KANACHK-RC          =   ZERO  )  AND
                  (KANACHK-SYUBETU     =   2     )
      *        老人公費の場合、全角はエラーとする
               IF      SPA-GMN-KOHNUM (IDX-K)  =   "027"
                   MOVE    "0095"              TO  SPA-ERRCD
                   MOVE    22                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       1001-JKYSNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    適応開始入力　処理
      *****************************************************************
       1001-KTEKSTYMD-CHK-SEC           SECTION.
      *
           IF      SPA-GMN-KTEKSTYMD(IDX-K)    =   SPACE
               MOVE    ZERO            TO  FLG-CHK-ERR
               EVALUATE    IDX-K
                   WHEN    1
                       IF      SPA-MCP-WIDGET  =   "P02_KTEKSTYMD1"
                                               OR  "P02_KTEKEDYMD1"
                           MOVE    1           TO  FLG-CHK-ERR
                       END-IF
                   WHEN    2
                       IF      SPA-MCP-WIDGET  =   "P02_KTEKSTYMD2"
                                               OR  "P02_KTEKEDYMD2"
                           MOVE    1           TO  FLG-CHK-ERR
                       END-IF
                   WHEN    3
                       IF      SPA-MCP-WIDGET  =   "P02_KTEKSTYMD3"
                                               OR  "P02_KTEKEDYMD3"
                           MOVE    1           TO  FLG-CHK-ERR
                       END-IF
               END-EVALUATE
               IF     (FLG-CHK-ERR         =   1        )  AND
                      (SPA-GMN-KOHNUM(IDX-K)   NOT =   SPACE)
                   CONTINUE
               ELSE
                   IF     (SPA-HKNKOH-FLG      =   1    )  AND
                          (SPA-GMN-KOHNUM(IDX-K)   NOT =   SPACE)
                       CONTINUE
                   ELSE
                       GO      TO      1001-KTEKSTYMD-CHK-EXT
                   END-IF
               END-IF
      *        システム日付を設定
               MOVE    SPA-SYSYMD          TO  SPA-NAI-KTEKSTYMD(IDX-K)
               MOVE    SPA-SYSYMDWH        TO  SPA-GMN-KTEKSTYMD(IDX-K)
           END-IF
      *
      *!   IF      SPA-GMN-KTEKSTYMD(IDX-K)    =   SPACE
      *!       GO      TO      1001-KTEKSTYMD-CHK-EXT
      *!   END-IF
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-GMN-KTEKSTYMD(IDX-K)
                                       TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  SPA-GMN-KTEKSTYMD(IDX-K)
               MOVE    SGDAY-SDAY          TO  SPA-NAI-KTEKSTYMD(IDX-K)
           ELSE
               MOVE    "0077"              TO  SPA-ERRCD
               MOVE    23                  TO  SPA-GMN-CUR
           END-IF
      *Ｈ２１．１対応
      *    ７５歳特例は、１日をエラーとする
           IF     (SPA-GMN-KOHNUM(IDX-K)   =   "969" )
             AND  (SPA-ERRCD               =   SPACE )
               IF      SPA-NAI-KTEKSTYMD(IDX-K)(7:2)   =   "01"
                   MOVE    "0177"              TO  SPA-ERRCD
                   MOVE    23                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      *!!
      *H25.12
      *    "961"減額（円）はＨ２４年４月からとする
           IF     (SPA-GMN-KOHNUM(IDX-K)   =   "961" )
             AND  (SPA-ERRCD               =   SPACE )
               IF      SPA-NAI-KTEKSTYMD(IDX-K)    <   "20120401"
                   MOVE    "2074"              TO  SPA-ERRCD
                   MOVE    23                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
      *!!
      *H26.12
      *    新規で開始日変更時に名称編集
           IF     (FLG-KOHINS          =   1     )
              AND (SPA-ERRCD           =   SPACE )
               IF     (SPA-NAI-KTEKSTYMD(IDX-K)
                                       NOT =   WRK-HZN-STYMD)
                OR    (SPA-GMN-KTEKEDYMD (IDX-K)
                                           =   SPACE )
                   MOVE    IDX-K               TO  IDX-K1
                   PERFORM 3101-HKNNUM-SET-SEC
               END-IF
           END-IF
      *
           .
       1001-KTEKSTYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力　処理
      *****************************************************************
       1001-KTEKEDYMD-CHK-SEC           SECTION.
      *
           IF     (SPA-GMN-KTEKSTYMD(IDX-K)    =   SPACE ) AND
                  (SPA-GMN-KTEKEDYMD(IDX-K)    =   SPACE )
               GO      TO      1001-KTEKEDYMD-CHK-EXT
           END-IF
      *!!
      *Ｈ２１．１対応
           IF     (SPA-GMN-KOHNUM(IDX-K)   =   "969" )
             AND  (SPA-NAI-KTEKSTYMD(IDX-K)    NOT =   SPACE )
               PERFORM 10011-969-KTEKEDYMD-SEC
           END-IF
      *!!
      *    空白で入力でない時、処理をしない
           IF     (SPA-HKNKOH-FLG      =   ZERO     )   AND
                  (SPA-GMN-KTEKEDYMD(IDX-K)    =   SPACE)
               EVALUATE    IDX-K
                   WHEN    1
                       IF      SPA-MCP-WIDGET  NOT =   "P02_KTEKEDYMD1"
                           GO      TO      1001-KTEKEDYMD-CHK-EXT
                       END-IF
                   WHEN    2
                       IF      SPA-MCP-WIDGET  NOT =   "P02_KTEKEDYMD2"
                           GO      TO      1001-KTEKEDYMD-CHK-EXT
                       END-IF
                   WHEN    3
                       IF      SPA-MCP-WIDGET  NOT =   "P02_KTEKEDYMD3"
                           GO      TO      1001-KTEKEDYMD-CHK-EXT
                       END-IF
               END-EVALUATE
           END-IF
      *
           IF     (SPA-GMN-KTEKEDYMD(IDX-K)    =   SPACE) OR
                  (SPA-GMN-KTEKEDYMD(IDX-K)(1:7)   =   "9999999" )
               MOVE    "9999999  "     TO  SPA-GMN-KTEKEDYMD(IDX-K)
               MOVE    ALL "9"         TO  SPA-NAI-KTEKEDYMD(IDX-K)
           ELSE
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN-KTEKEDYMD(IDX-K)
                                           TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY     TO  SPA-GMN-KTEKEDYMD(IDX-K)
                   MOVE    SGDAY-SDAY      TO  SPA-NAI-KTEKEDYMD(IDX-K)
               ELSE
                   MOVE    SPACE           TO  SPA-NAI-KTEKEDYMD(IDX-K)
                   MOVE    "0078"              TO  SPA-ERRCD
                   MOVE    24                  TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       1001-KTEKEDYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了日自動編集処理
      *****************************************************************
       10011-969-KTEKEDYMD-SEC           SECTION.
      *
      *    開始年月の月末
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    SPA-NAI-KTEKSTYMD(IDX-K)(1:6)
                                       TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING
                                           STS-AREA-DAY
                                           LNK-DAY7-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY7-KOYOMI     TO  SPA-GMN-KTEKEDYMD(IDX-K)
           END-IF
           .
       10011-969-KTEKEDYMD-EXT.
           EXIT.
      *****************************************************************
      *    確認日付入力　処理
      *****************************************************************
       1001-KKAKUNINYMD-CHK-SEC           SECTION.
      *
           IF     (SPA-GMN-KTEKSTYMD(IDX-K)    =   SPACE ) AND
                  (SPA-GMN-KTEKEDYMD(IDX-K)    =   SPACE ) AND
                  (SPA-GMN-KKAKUNINYMD(IDX-K)  =   SPACE )
               GO      TO      1001-KKAKUNINYMD-CHK-EXT
           END-IF
      *
      *
           IF      SPA-GMN-KKAKUNINYMD(IDX-K)  =   SPACE
               MOVE    SPA-SYSYMD          TO  SPA-NAI-KKAKUNINYMD
                                                              (IDX-K)
               MOVE    SPA-SYSYMDWH        TO  SPA-GMN-KKAKUNINYMD
                                                              (IDX-K)
           ELSE
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN-KKAKUNINYMD(IDX-K)
                                           TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY     TO  SPA-GMN-KKAKUNINYMD
                                                              (IDX-K)
                   MOVE    SGDAY-SDAY      TO  SPA-NAI-KKAKUNINYMD
                                                              (IDX-K)
               ELSE
                   MOVE    SPACE           TO  SPA-NAI-KKAKUNINYMD
                                                              (IDX-K)
                   MOVE    "0079"          TO  SPA-ERRCD
                   MOVE    25              TO  SPA-GMN-CUR
                   GO  TO                  1001-KKAKUNINYMD-CHK-EXT
               END-IF
      *        確認日変更時のみ
               IF      SPA-NAI-KKAKUNINYMD(IDX-K)
                                           >   SPA-SYSYMD
                 AND  (SPA-MAE-KKAKUNINYMD(IDX-K)
                                       NOT =   SPA-NAI-KKAKUNINYMD
                                                             (IDX-K))
                   IF      SPA-KOH-KAKUINFLG (IDX-K)   =   ZERO
                       MOVE    "0080"          TO  SPA-ERRCD
                       MOVE    25              TO  SPA-GMN-CUR
                       MOVE    1               TO  SPA-KOH-KAKUINFLG
                                                            (IDX-K)
                   END-IF
               END-IF
           END-IF
           .
       1001-KKAKUNINYMD-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    関連チェック処理
      ****************************************************************
       200-KANREN-CHK-SEC     SECTION.
      *
      *    保険入力なし
           IF      (SPA-GMN-FTNJANUM (IDX-K)   =   SPACE  )  AND
                   (SPA-GMN-KOHNUM   (IDX-K)   =   SPACE  )  AND
                   (SPA-GMN-JKYSNUM  (IDX-K)   =   SPACE  )  AND
                   (SPA-GMN-KTEKSTYMD (IDX-K)  =   SPACE  )  AND
                   (SPA-GMN-KTEKEDYMD (IDX-K)  =   SPACE  )  AND
                   (SPA-GMN-KKAKUNINYMD(IDX-K) =   SPACE  )
               GO      TO      200-KANREN-CHK-EXT
           END-IF
      *
      *    保険入力あり
           IF      (SPA-GMN-KOHNUM   (IDX-K)   NOT =   SPACE  )  AND
                   (SPA-GMN-KTEKSTYMD (IDX-K)  NOT =   SPACE  )  AND
                   (SPA-GMN-KTEKEDYMD (IDX-K)  NOT =   SPACE  )  AND
                   (SPA-GMN-KKAKUNINYMD(IDX-K) NOT =   SPACE  )
               MOVE    1               TO  FLG-KAKUTEI
           ELSE
               MOVE    ZERO            TO  FLG-KAKUTEI
           END-IF
      *
      *    必須入力チェック
           IF      SPA-HKNKOH-FLG      =   1
               PERFORM 2001-HISSU-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      200-KANREN-CHK-EXT
               END-IF
           END-IF
      *Ｈ１８．１０改正のチェック
           IF     (SPA-ERRCD           =   SPACE  )   AND
                  (SPA-GMN-KOHNUM(IDX-K)   =   "027") AND
                  (SPA-GMN-KOHKBN(IDX-K)   =   "2"  )
               IF      (SPA-NAI-KTEKSTYMD (IDX-K)
                                               >=  "20061001") OR
                      ((SPA-NAI-KTEKSTYMD (IDX-K)
                                               =   SPACE  )  AND
                       (SPA-SYSYMD             >=  "20061001" ))
      *            Ｈ１８．１０以降は、２割はエラー
                   MOVE    "0098"          TO  SPA-ERRCD
                   MOVE    21              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
      *    すべての項目が入力済みの時、以下のチェックを行う
           IF      FLG-KAKUTEI         =   ZERO
               GO      TO      200-KANREN-CHK-EXT
           END-IF
      *    保険を確認
           IF      SPA-GMN-KOHNUM(IDX-K)   NOT =   SPACE
               PERFORM 2002-KOHKIKAN-CHK-SEC
               IF     (SPA-ERRCD       NOT =   SPACE )  OR
                      (SPA-PIDCD       NOT =   SPACE )
                   GO      TO      200-KANREN-CHK-EXT
               END-IF
           END-IF
      *    負担者番号・受給者番号チェック
           PERFORM 2002-FTNJANUM-KANREN-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      200-KANREN-CHK-EXT
           END-IF
      *    適応期間範囲チェック
           IF      SPA-NAI-KTEKSTYMD(IDX-K)
      ******************************** >=  SPA-NAI-KTEKEDYMD(IDX-K)
                                       >   SPA-NAI-KTEKEDYMD(IDX-K)
               MOVE    "0081"          TO  SPA-ERRCD
               MOVE    23              TO  SPA-GMN-CUR
               GO      TO      200-KANREN-CHK-EXT
           END-IF
      *
      *    年齢チェック（生年月日登録時）
           IF      SPA-NAI-BIRTHDAY    =   SPACE
               GO      TO      200-KANREN-CHK-EXT
           END-IF
      *****IF      SPA-NAI-PTKOH-SBTKBN(IDX-K) =   "3"
           IF      SPA-GMN-KOHNUM (IDX-K)  =   "027"
      *        老人保険年齢チェック
               PERFORM 2004-RJN-TAISYOAGE-CHECK-SEC
           ELSE
      *        年齢チェック（老人年齢チェック以外）
               PERFORM 2003-NENREI-CHECK-SEC
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      200-KANREN-CHK-EXT
           END-IF
      *
      **** 保険者異動日チェック
      *    IF     (SPA-PTKOH-IDOYMD(IDX-K)     =   SPACE)  OR
      *           (SPA-NAI-PTKOH-KEIFLG (IDX-K)    =   "1")
      *        CONTINUE
      *    ELSE
      *        PERFORM 2005-IDOYMD-CHECK-SEC
      **** END-IF
      *H25.12
      *    生活保護重複チェック
           IF     (SPA-GMN-KOHNUM(IDX-K)   =   "012")
              AND (SPA-NAI-JFKCHKFLG(IDX-K)    =   ZERO)
              AND (SPA-ERRCD               =   SPACE)
               PERFORM 2006-SEIJYUFUKU-CHK-SEC
           END-IF
      *
           .
       200-KANREN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    必須入力チェック処理
      *****************************************************************
       2001-HISSU-CHK-SEC           SECTION.
      *
           IF      SPA-GMN-KOHNUM (IDX-K)  =   SPACE
               MOVE    "0073"          TO  SPA-ERRCD
               MOVE    21              TO  SPA-GMN-CUR
               GO      TO      2001-HISSU-CHK-EXT
           END-IF
      *
      *    適用開始日
           IF      SPA-GMN-KTEKSTYMD (IDX-K)   =   SPACE
               MOVE    "0062"          TO  SPA-ERRCD
               MOVE    23              TO  SPA-GMN-CUR
               GO      TO      2001-HISSU-CHK-EXT
           END-IF
      *
      *    適用終了日
           IF      SPA-GMN-KTEKEDYMD (IDX-K)   =   SPACE
               MOVE    "0078"          TO  SPA-ERRCD
               MOVE    24              TO  SPA-GMN-CUR
           END-IF
           .
       2001-HISSU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険確認チェック処理
      *****************************************************************
       2002-KOHKIKAN-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  WRK-STYEAR
           MOVE    ZERO                TO  WRK-EDYEAR
           MOVE    SPACE               TO  WRK-HKN-TEKSTYMD
           MOVE    SPACE               TO  WRK-HKN-TEKEDYMD
           MOVE    SPACE               TO  WRK-HKN2-TEKSTYMD
           MOVE    SPACE               TO  WRK-HKN2-TEKEDYMD
      *
           MOVE    SPACE               TO  HKNNUM-REC
           MOVE    SPA-HOSPNUM          TO  HKN-HOSPNUM
           MOVE    SPA-GMN-KOHNUM(IDX-K)
                                       TO  HKN-HKNNUM
           IF     (SPA-GMN-KOHNUM(IDX-K)   =   "027"  )  AND
                 ((SPA-NAI-PTKOH-PAYKBN (IDX-K)
                                           =   ZERO  OR  SPACE ) OR
                  (SPA-NAI-KTEKSTYMD (IDX-K)
                                           <   "20021001"))
      *        Ｈ１２．１２．３１までの支払区分は００
               IF      SPA-NAI-KTEKEDYMD (IDX-K)
                                               >=  "20010101"
                   MOVE    SPA-NAI-HOSPSBT         TO  HKN-PAYKBN(1:1)
                   MOVE    SPA-NAI-ROUPAYKBN       TO  HKN-PAYKBN(2:1)
               ELSE
                   MOVE    ZERO                TO  HKN-PAYKBN
               END-IF
           ELSE
               MOVE    SPA-NAI-PTKOH-PAYKBN (IDX-K)
                                           TO  HKN-PAYKBN
           END-IF
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 921-HKNMST-NEXT-SEC
           ELSE
               INITIALIZE              HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM
               UNTIL   FLG-HKNNUM      =   1
               MOVE    2                   TO  FLG-CHK
      *        範囲内
               IF     (SPA-NAI-KTEKSTYMD (IDX-K)
                                            >=  HKN-TEKSTYMD )  AND
                      (SPA-NAI-KTEKEDYMD (IDX-K)
                                            <=  HKN-TEKEDYMD )
                   MOVE    HKN-JOKEN-STYEAR    TO  WRK-STYEAR
                   MOVE    HKN-JOKEN-EDYEAR    TO  WRK-EDYEAR
                   MOVE    1                   TO  FLG-HKNNUM
                   MOVE    1                   TO  FLG-CHK
               END-IF
      *        開始同一
               IF     (SPA-NAI-KTEKSTYMD (IDX-K)
                                           >=  HKN-TEKSTYMD )  AND
                      (SPA-NAI-KTEKEDYMD (IDX-K)
                                           >   HKN-TEKEDYMD )
                   MOVE    HKN-TEKSTYMD    TO  WRK-HKN-TEKSTYMD
                   MOVE    HKN-TEKEDYMD    TO  WRK-HKN-TEKEDYMD
               END-IF
               IF      FLG-CHK         NOT =   1
                   MOVE    HKN-TEKSTYMD    TO  WRK-HKN2-TEKSTYMD
                   MOVE    HKN-TEKEDYMD    TO  WRK-HKN2-TEKEDYMD
               END-IF
      *
               IF      FLG-HKNNUM          =   ZERO
                   MOVE    "tbl_hknnum"        TO  MCP-TABLE
                   MOVE    "key7"              TO  MCP-PATHNAME
                   PERFORM 921-HKNMST-NEXT-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_hknnum"       TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-CHK             =   ZERO
      *        マスタなし
               MOVE   SPACE            TO  SPA-GMN-KOHNUMMEI(IDX-K)
               MOVE    "0072"          TO  SPA-ERRCD
               MOVE    21              TO  SPA-GMN-CUR
               GO      TO      2002-KOHKIKAN-CHK-EXT
           END-IF
      *R01.11
      *    公費以外はエラー
           IF      HKN-HKNKOHSBTKBN    =   "1"  OR  "4"
                                       OR  "8"  OR  "9"
               MOVE   SPACE            TO  SPA-GMN-KOHNUMMEI(IDX-K)
               MOVE   "0072"           TO  SPA-ERRCD
               MOVE    21              TO  SPA-GMN-CUR
               GO      TO      2002-KOHKIKAN-CHK-EXT
           END-IF
      *
      *R01.11（HAORI対応名称再編集）
           IF      SPA-GMN-KOHNUM(IDX-K)
                                    NOT =   SPA-MAE-KOHNUM(IDX-K)
               MOVE   HKN-TANSEIDONAME    TO  SPA-GMN-KOHNUMMEI(IDX-K)
           END-IF
      *
      *    対象あり
           IF      FLG-CHK             =   1
               GO      TO      2002-KOHKIKAN-CHK-EXT
           END-IF
      *
           IF      WRK-HKN-TEKSTYMD    =   SPACE
               MOVE    WRK-HKN2-TEKSTYMD   TO  WRK-HKN-TEKSTYMD
               MOVE    WRK-HKN2-TEKEDYMD   TO  WRK-HKN-TEKEDYMD
               MOVE    SPACE               TO  WRK-HKN2-TEKSTYMD
               MOVE    SPACE               TO  WRK-HKN2-TEKEDYMD
           END-IF
      *    適応期間エラー
      *    保険期間の終了日が変更された時、終了日を自動変更する
           IF     (SPA-NAI-KTEKSTYMD(IDX-K) >=  WRK-HKN-TEKSTYMD) AND
                  (SPA-NAI-KTEKEDYMD(IDX-K) >   WRK-HKN-TEKEDYMD)  AND
                  (SPA-NAI-KTEKSTYMD(IDX-K) <   WRK-HKN-TEKEDYMD) AND
                  (WRK-HKN-TEKEDYMD    NOT =   ALL "9"          )
               MOVE    IDX-K                TO  SPA-KOH-IDX1
               MOVE    WRK-HKN-TEKEDYMD     TO  SPA-HKN-TEKEDYMD
               MOVE    SPACE                TO  SPA-HKN2-TEKSTYMD
               MOVE    SPACE                TO  SPA-HKN2-TEKEDYMD
               MOVE    SPACE                TO  SPA-HKN2-KOHNUM
               MOVE    ZERO                 TO  SPA-HKN2-KOHID
      *
      *        公費の切替えで次ぎの公費も対象の時、追加とする
               IF     (WRK-HKN2-TEKSTYMD    NOT =   SPACE   )
                 AND  (WRK-HKN2-TEKSTYMD    NOT =   WRK-HKN-TEKSTYMD  )
                 AND  (SPA-NAI-KTEKEDYMD(IDX-K) >=  WRK-HKN2-TEKSTYMD)
                   MOVE    WRK-HKN2-TEKSTYMD       TO  SPA-HKN2-TEKSTYMD
                   MOVE    SPA-GMN-KOHNUM(IDX-K)   TO  SPA-HKN2-KOHNUM
                   MOVE    SPA-NAI-PTKOH-KOHID(IDX-K)
                                                   TO  SPA-HKN2-KOHID
                   MOVE    SPA-GMN-JKYSNUM(IDX-K)
                                                   TO  SPA-HKN2-JKYSNUM
                   IF      SPA-NAI-KTEKEDYMD(IDX-K)
                                                <=  WRK-HKN2-TEKEDYMD
                       MOVE    SPA-NAI-KTEKEDYMD(IDX-K)
                                                   TO  SPA-HKN2-TEKEDYMD
                   ELSE
                       MOVE    WRK-HKN2-TEKEDYMD   TO  SPA-HKN2-TEKEDYMD
                   END-IF
               END-IF
      *
               MOVE    SPACE                TO  SPA-PIDMSG
      *
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-HKN-TEKEDYMD    TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
      *
               STRING  SPA-GMN-KOHNUM (IDX-K)  DELIMITED  BY  SIZE
                       SPA-GMN-KOHKBN (IDX-K)  DELIMITED  BY  SIZE
                       SPA-GMN-KOHNUMMEI(IDX-K)
                                               DELIMITED  BY  SPACE
                       "の期限を"              DELIMITED  BY  SIZE
                       LNK-DAY2-EDTYMD3        DELIMITED  BY  SIZE
                       "に変更します。"        DELIMITED  BY  SIZE
                       "よろしいですか。"      DELIMITED  BY  SIZE
                       INTO                    SPA-PIDMSG
               END-STRING
               MOVE    "0107"              TO  SPA-PIDCD
               MOVE    21                  TO  SPA-GMN-CUR
           ELSE
      *        適用期間開始のエラー
               IF      SPA-NAI-KTEKSTYMD(IDX-K) >   WRK-HKN-TEKEDYMD
                   MOVE    "0093"              TO  SPA-ERRCD
               ELSE
                   MOVE    "0094"              TO  SPA-ERRCD
               END-IF
               MOVE    21                  TO  SPA-GMN-CUR
           END-IF
      *
           .
       2002-KOHKIKAN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    負担者番号・受給者番号チェック処理
      *****************************************************************
       2002-FTNJANUM-KANREN-SEC           SECTION.
      *
      *    保険番号が検索済みとする
      *
      *    負担者番号入力チェック（長期）
           IF      HKN-HKNKOHSBTKBN    =   "2"
               IF      SPA-GMN-FTNJANUM(IDX-K) NOT  =   SPACE
                   MOVE    "0082"          TO  SPA-ERRCD
                   MOVE    20              TO  SPA-GMN-CUR
                   GO      TO      2002-FTNJANUM-KANREN-EXT
               END-IF
           END-IF
      *    負担者番号モジュラス10チェック
           IF      HKN-KENSNUMCHKKBN   =   "1"
               PERFORM 4911-FTNJANUM-CHECK-SEC
               IF      SPA-ERRCD       NOT =  SPACE
                   GO      TO      2002-FTNJANUM-KANREN-EXT
               END-IF
           END-IF
      *    受給者番号入力チェック（長期）
           IF      HKN-HKNKOHSBTKBN    =   "2"
               IF     SPA-GMN-JKYSNUM (IDX-K) NOT  =   SPACE
                   MOVE    "0082"          TO  SPA-ERRCD
                   MOVE    20              TO  SPA-GMN-CUR
                   GO      TO      2002-FTNJANUM-KANREN-EXT
               END-IF
           END-IF
      *    受給者番号モジュラス10チェック
           IF      HKN-JKYSKENSNUMCHKKBN   =   "1"
                                           OR  "3"
               PERFORM 4912-JKYSNUM-CHECK-SEC
           END-IF
      *
           .
       2002-FTNJANUM-KANREN-EXT.
           EXIT.
      *
      *****************************************************************
      *    負担者番号モジュラス10チェック処理
      *****************************************************************
       4911-FTNJANUM-CHECK-SEC        SECTION.
      *
           IF      SPA-GMN-FTNJANUM (IDX-K)    =   SPACE
               GO      TO      4911-FTNJANUM-CHECK-EXT
           END-IF
      *
      *    入力された保険者番号をセットすれば、サブプロ内で
      *    検証してくれる (処理区分:2)
           INITIALIZE                       LNK-CHKDGT-AREA
           MOVE    2                   TO   LNK-I-SYORIKBN
           MOVE    SPA-GMN-FTNJANUM (IDX-K)
                                       TO   LNK-I-CODE
           CALL    "ORCSCHKDGT"    USING    LNK-CHKDGT-AREA
           IF      LNK-O-ERRCD     NOT =   ZERO
               MOVE    "0083"          TO  SPA-ERRCD
               MOVE    20              TO  SPA-GMN-CUR
           END-IF
      *
          .
       4911-FTNJANUM-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    受給者番号モジュラス10チェック処理
      *****************************************************************
       4912-JKYSNUM-CHECK-SEC        SECTION.
      *
           IF     SPA-GMN-JKYSNUM (IDX-K)  =   SPACE
               GO  TO               4912-JKYSNUM-CHECK-EXT
           END-IF
      *
      *    入力された保険者番号をセットすれば、サブプロ内で
      *    検証してくれる (処理区分:2)
           INITIALIZE                      LNK-CHKDGT-AREA
           MOVE    2                   TO  LNK-I-SYORIKBN
           MOVE    SPA-GMN-JKYSNUM (IDX-K)
                                       TO  LNK-I-CODE
           CALL    "ORCSCHKDGT"    USING   LNK-CHKDGT-AREA
           IF      LNK-O-ERRCD     NOT =   ZERO
      *        警告
               IF      HKN-JKYSKENSNUMCHKKBN   =  "3"
                   IF      SPA-NAI-JYUCHK (IDX-K)  =   ZERO
                       MOVE    "K084"          TO  SPA-ERRCD
                       MOVE    22              TO  SPA-GMN-CUR
                       MOVE    1               TO  SPA-NAI-JYUCHK
                                                           (IDX-K)
                   END-IF
               ELSE
                   MOVE    "0084"          TO  SPA-ERRCD
                   MOVE    22              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4912-JKYSNUM-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    年齢チェック（老人年齢チェック以外）処理
      *****************************************************************
       2003-NENREI-CHECK-SEC          SECTION.
      *    年齢チェックなし
           IF     ((WRK-STYEAR         =   ZERO    )  AND
                   (WRK-EDYEAR         =   999     ))  OR
                  ((WRK-STYEAR         =   ZERO    )  AND
                   (WRK-EDYEAR         =   ZERO    ))
               GO      TO      2003-NENREI-CHECK-EXT
           END-IF
           IF     (SPA-NAI-KTEKEDYMD (IDX-K)   =   ALL "9" )
              AND (WRK-EDYEAR      NOT =   999  )
               MOVE    "0085"          TO  SPA-ERRCD
               MOVE    24              TO  SPA-GMN-CUR
               GO      TO      2003-NENREI-CHECK-EXT
           END-IF
      *
           IF      SPA-NAI-BIRTHDAY    =   SPACE
               MOVE    "0016"          TO  SPA-ERRCD
               MOVE    23              TO  SPA-GMN-CUR
               GO      TO      2003-NENREI-CHECK-EXT
           END-IF
      *    生年月日から基準日を求める
      *      １日ならば生年月日、１日以外ならば生年月日の翌月１日
           MOVE    SPA-NAI-BIRTHDAY    TO  WRK-YMDS1
           IF      SPA-NAI-BIRTHDAY (7:2)  NOT =   "01"
               PERFORM 4910-BIRTH-AFTER1MON-GET-SEC
               MOVE    WRK-BASEYMD    TO  WRK-YMDS1
           END-IF
           PERFORM   VARYING   IDX    FROM    1   BY  1
                     UNTIL   ( IDX            >   2 )
                     OR      ( SPA-ERRCD  NOT =   SPACE )
               IF    IDX                  =   1
                   MOVE    SPA-NAI-KTEKSTYMD(IDX-K) TO  WRK-YMDS2
               ELSE
                   MOVE    SPA-NAI-KTEKEDYMD(IDX-K) TO  WRK-YMDS2
               END-IF
      *        誕生日より前はゼロ
               IF      WRK-YYS2           <   WRK-YYS1
                   MOVE    ZERO           TO  WRK-NENREI-YY
               ELSE
      *
               COMPUTE   WRK-NENREI-YY    =   WRK-YYS2
                                          -   WRK-YYS1
               IF  WRK-YMDS1 (5:4)        >   WRK-YMDS2 (5:4)
                   COMPUTE WRK-NENREI-YY  =   WRK-NENREI-YY
                                          -   1
               END-IF
      *
               END-IF
      *
               IF  ( WRK-NENREI-YY         <   WRK-STYEAR )  OR
                   ( WRK-NENREI-YY         >   WRK-EDYEAR )
                   IF     (FLG-AUTO            =   ZERO  )  AND
                          (SPA-NAI-AGECHK (IDX-K)  =   ZERO)
                       MOVE    1               TO  SPA-NAI-AGECHK(IDX-K)
      *
      **************** MOVE    "0086"          TO  SPA-ERRCD
                       IF      IDX             =   1
                           MOVE    23              TO  SPA-GMN-CUR
                           MOVE    "K005"          TO  SPA-ERRCD
                       ELSE
                           MOVE    24              TO  SPA-GMN-CUR
                           MOVE    "K006"          TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
          .
       2003-NENREI-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人保険年齢チェック処理
      *****************************************************************
       2004-RJN-TAISYOAGE-CHECK-SEC     SECTION.
      *
           IF     (SPA-NAI-KTEKSTYMD(IDX-K) >=   "20021001"  )  OR
                  (SPA-NAI-KTEKSTYMD(IDX-K) =   SPACE   AND
                   SPA-SYSYMD               >=  "20021001"  )
      *        Ｈ１４．１０以降
               PERFORM 20042-RJN-TAISYOAGE-10-SEC
           ELSE
      *        Ｈ１４．１０まで
               PERFORM 20041-RJN-TAISYOAGE-09-SEC
           END-IF
      *
           .
       2004-RJN-TAISYOAGE-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人保険年齢チェック処理(Ｈ１４．１０まで)
      *****************************************************************
       20041-RJN-TAISYOAGE-09-SEC    SECTION.
      *
      *    生年月日から基準日を求める
      *      １日ならば生年月日、１日以外ならば生年月日の翌月１日
           MOVE    SPA-NAI-BIRTHDAY     TO  WRK-BASEYMD
           IF      SPA-NAI-BIRTHDAY (7:2)   NOT =   "01"
               PERFORM 4910-BIRTH-AFTER1MON-GET-SEC
               IF      SPA-ERRCD        NOT =   SPACE
                   GO  TO               20041-RJN-TAISYOAGE-09-EXT
               END-IF
           END-IF
      *    ６５歳を迎える日
           MOVE      WRK-BASEYMD        TO  WRK-YMD65
           COMPUTE   WRK-B65YY          =   WRK-B65YY   +   65
      *    ７０歳を迎える日
           MOVE      WRK-BASEYMD        TO  WRK-YMD70
           COMPUTE   WRK-B70YY          =   WRK-B70YY   +   70
      *
           EVALUATE    TRUE
           WHEN    ( SPA-NAI-KTEKSTYMD(IDX-K)  >=  WRK-YMD70 )
               CONTINUE
      *    警告
           WHEN    ( SPA-NAI-KTEKSTYMD(IDX-K)  <   WRK-YMD70 )  AND
                   ( SPA-NAI-KTEKSTYMD(IDX-K)  >=  WRK-YMD65 )
               IF      SPA-NAI-AGE65 (IDX-K)   =   ZERO
                   MOVE    "K001"           TO  SPA-ERRCD
                   MOVE    23               TO  SPA-GMN-CUR
                   MOVE    1                TO  SPA-NAI-AGE65 (IDX-K)
               END-IF
      *    エラー
           WHEN    ( SPA-NAI-KTEKSTYMD(IDX-K)  <   WRK-YMD65 )
               MOVE    "0087"           TO  SPA-ERRCD
               MOVE    23               TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       20041-RJN-TAISYOAGE-09-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人保険年齢チェック処理(Ｈ１４．１０から)
      *****************************************************************
       20042-RJN-TAISYOAGE-10-SEC    SECTION.
      *
      *    生年月日から基準日を求める
      *      １日ならば生年月日、１日以外ならば生年月日の翌月１日
           MOVE    SPA-NAI-BIRTHDAY     TO  WRK-BASEYMD
           IF      SPA-NAI-BIRTHDAY (7:2)   NOT =   "01"
               PERFORM 4910-BIRTH-AFTER1MON-GET-SEC
               IF      SPA-ERRCD        NOT =   SPACE
                   GO  TO               20042-RJN-TAISYOAGE-10-EXT
               END-IF
           END-IF
      *    ６５歳を迎える日
           MOVE      WRK-BASEYMD        TO  WRK-YMD65
           COMPUTE   WRK-B65YY          =   WRK-B65YY   +   65
      *
           EVALUATE    TRUE
           WHEN    ( SPA-NAI-KTEKSTYMD(IDX-K)  >=  SPA-BYMD-70)
               CONTINUE
      *    警告（６５歳から７０歳未満）
           WHEN    ( SPA-NAI-KTEKSTYMD(IDX-K)  <   SPA-BYMD-70) AND
                   ( SPA-NAI-KTEKSTYMD(IDX-K)  >=  WRK-YMD65 )
               IF      SPA-NAI-AGE65 (IDX-K)   =   ZERO
                   MOVE    "K002"           TO  SPA-ERRCD
                   MOVE    23               TO  SPA-GMN-CUR
                   MOVE    1                TO  SPA-NAI-AGE65 (IDX-K)
               END-IF
      *    エラー
           WHEN    ( SPA-NAI-KTEKSTYMD(IDX-K)  <   WRK-YMD65 )
               MOVE    "0087"           TO  SPA-ERRCD
               MOVE    23               TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       20042-RJN-TAISYOAGE-10-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者異動日チェック処理
      *****************************************************************
       2005-IDOYMD-CHECK-SEC         SECTION.
      *
           IF      (SPA-PTKOH-IDOYMD (IDX-K)
                                       <=  SPA-NAI-KTEKSTYMD(IDX-K)) OR
                   (SPA-PTKOH-IDOYMD (IDX-K)
                                       <   SPA-NAI-KTEKEDYMD(IDX-K))
               MOVE    "K004"          TO  SPA-ERRCD
               MOVE    "1"             TO  SPA-NAI-PTKOH-KEIFLG (IDX-K)
               MOVE    20              TO  SPA-GMN-CUR
           ELSE
               MOVE    SPACE           TO  SPA-NAI-PTKOH-KEIFLG (IDX-K)
           END-IF
      *
           .
       2005-IDOYMD-CHECK-EXT.
           EXIT.
      *
      *v4.7
      *****************************************************************
      *    生活保護の期間重複チェック処理
      *****************************************************************
       2006-SEIJYUFUKU-CHK-SEC         SECTION.
      *
           MOVE    1               TO  SPA-NAI-JFKCHKFLG (IDX-K)
      *
           MOVE    ZERO                TO  WRK-IDX-AREA
           PERFORM VARYING     IDK2     FROM    1   BY  1
                   UNTIL      (IDK2         >   3   )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-GMN-KOHNUM(IDK2)   NOT =   SPACE
                   MOVE    SPA-NAI-PTKOH-EDANUM (IDK2)
                                               TO  WRK-IDX (IDK2)
               END-IF
               IF     (SPA-GMN-KOHNUM(IDK2)    =   "012") AND
                      (IDX-K               NOT =   IDK2 )
                   IF      (SPA-NAI-KTEKEDYMD (IDX-K)
                                       <   SPA-NAI-KTEKSTYMD(IDK2) ) OR
                           (SPA-NAI-KTEKSTYMD (IDX-K)
                                       >   SPA-NAI-KTEKEDYMD(IDK2) )
                       CONTINUE
                   ELSE
                       MOVE    "K136"          TO  SPA-ERRCD
                       MOVE    23              TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-PERFORM
      *    テーブル分
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX             >   SPA-PTKOH-MAX )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               MOVE    ZERO            TO  FLG-CHK
      *        現在表示分、削除分はチェックしない
               IF     (IDX             =   WRK-IDX (1) OR
                                           WRK-IDX (2)  OR
                                           WRK-IDX (3) ) OR
                      (SPA-PTKOH-SAKJOKBN(IDX)     =   "1" )
                   CONTINUE
               ELSE
      *            範囲の重複
                   IF      (SPA-PTKOH-TEKEDYMD (IDX)
                                       <   SPA-NAI-KTEKSTYMD(IDX-K)) OR
                           (SPA-PTKOH-TEKSTYMD (IDX)
                                       >   SPA-NAI-KTEKEDYMD(IDX-K))
                       CONTINUE
                   ELSE
                       MOVE    1            TO  FLG-CHK
                   END-IF
               END-IF
               IF     (FLG-CHK              =   1  )
                  AND (SPA-PTKOH-KOHNUM(IDX)  =   "012")
                   MOVE    "K136"          TO  SPA-ERRCD
                   MOVE    23              TO  SPA-GMN-CUR
               END-IF
           END-PERFORM
           .
      *
       2006-SEIJYUFUKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費期間重複チェック処理
      *****************************************************************
       300-KANREN-KOH-CHECK-SEC    SECTION.
      *
           MOVE    ZERO                TO  WRK-IDX-AREA
           INITIALIZE                  SPA-KOH-KENKAN-AREA
      *
      *    今回入力分でチェックする
           PERFORM VARYING     IDX-K    FROM    1   BY  1
                   UNTIL      (IDX-K        >   3   )  OR
                              (SPA-ERRCD   NOT =   SPACE )  OR
                              (SPA-PIDCD   NOT =   SPACE )
               IF      SPA-GMN-KOHNUM(IDX-K)   NOT =   SPACE
                   MOVE    SPA-NAI-PTKOH-EDANUM (IDX-K)
                                               TO  WRK-IDX (IDX-K)
                   PERFORM 3001-INPUT-KOH-CHK-SEC
               END-IF
               IF      SPA-ERRCD       NOT =   SPACE
                   COMPUTE SPA-GMN-CUR     =   SPA-GMN-CUR
                                           +  ((IDX-K  -   1  )  *  6 )
               END-IF
               IF     (SPA-ERRCD       NOT =   SPACE )  OR
                      (SPA-PIDCD       NOT =   SPACE )
                   MOVE    IDX-K           TO  SPA-KOH-ERRIDX
               END-IF
           END-PERFORM
      *
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-PIDCD       NOT =   SPACE )
               GO      TO      300-KANREN-KOH-CHECK-EXT
           END-IF
      *
      *    ＴＢＬでチェックする
           PERFORM VARYING     IDX-K    FROM    1   BY  1
                   UNTIL      (IDX-K        >   3   )  OR
                              (SPA-ERRCD   NOT =   SPACE )  OR
                              (SPA-PIDCD   NOT =   SPACE )
               IF      SPA-GMN-KOHNUM(IDX-K)   NOT =   SPACE
                   PERFORM 3002-TBL-KOH-CHK-SEC
               END-IF
               IF      SPA-ERRCD       NOT =   SPACE
                   COMPUTE SPA-GMN-CUR     =   SPA-GMN-CUR
                                           +  ((IDX-K  -   1  )  *  6 )
               END-IF
      *APIV3対応
               IF     (SPA-ERRCD       NOT =   SPACE )  OR
                      (SPA-PIDCD       NOT =   SPACE )
                   MOVE    IDX-K           TO  SPA-KOH-ERRIDX
               END-IF
           END-PERFORM
      *
           .
       300-KANREN-KOH-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費期間重複チェック処理（今回入力分）
      *****************************************************************
       3001-INPUT-KOH-CHK-SEC    SECTION.
      *
           PERFORM VARYING     IDK2     FROM    1   BY  1
                   UNTIL      (IDK2         >   3   )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF     (SPA-GMN-KOHNUM(IDK2)    NOT =   SPACE) AND
                      (IDX-K                   NOT =   IDK2 )
                   IF      (SPA-NAI-KTEKEDYMD (IDX-K)
                                       <   SPA-NAI-KTEKSTYMD(IDK2) ) OR
                           (SPA-NAI-KTEKSTYMD (IDX-K)
                                       >   SPA-NAI-KTEKEDYMD(IDK2) )
                       CONTINUE
                   ELSE
      *                特定は重複可とする
                       IF     (SPA-GMN-KOHNUM (IDX-K)
                                           =   SPA-GMN-KOHNUM(IDK2))
                       AND   ((SPA-GMN-KOHNUM(IDX-K)
                                           =   "051"
                                           OR  "091"
      *                肝炎治療　追加
                                           OR  "038"
      *ver.4.7         生活保護　追加
                                           OR  "012"
      *                更正・育成追加
                                           OR  "015"  OR  "016"    )
      *                北海道の地方公費
                          OR  (SPA-GMN-KOHNUM(IDX-K)   =   "183"  AND
                               SPA-PREFNUM             =   01    )
      *H25.7
      *                第三者行為
                          OR  (SPA-GMN-KOHNUM(IDX-K)   =   "970" )
      *H27.1
      *                難病
                          OR ( SPA-GMN-KOHNUM(IDX-K)   =   "054" )
      *H30.11
      *                肝がん治療
                          OR ( SPA-GMN-KOHNUM(IDX-K)   =   "092" )
      *H26.11
      *                小児特定　Ｈ２６．１２まで
      *                   OR ((SPA-GMN-KOHNUM(IDX-K)   =   "052" ) AND
      *                       (SPA-NAI-KTEKSTYMD (IDX-K)
      *                                                <=  "20141231"))
      *R02.4           小児特定 Ｈ２６．１２までを廃止
                          OR ( SPA-GMN-KOHNUM(IDX-K)   =   "052" )
      *H28.2
      *                京都府　京都子　H25.9.1から
                          OR ((SPA-GMN-KOHNUM(IDX-K)   =   "745" ) AND
                              (SPA-PREFNUM             =   26    ) AND
                              (SPA-NAI-KTEKSTYMD (IDX-K)
                                                       >=  "20130901")))
                           CONTINUE
                       ELSE
      *
                       IF      SPA-GMN-KOHNUM (IDX-K)
                                           =   SPA-GMN-KOHNUM(IDK2)
                           MOVE    "0088"      TO  SPA-ERRCD
                           MOVE    23          TO  SPA-GMN-CUR
                           MOVE    IDX-K       TO  WRK-IDX1
                           MOVE    IDK2        TO  WRK-IDX2
                       END-IF
                       IF      (SPA-NAI-PTKOH-SBTKBN (IDX-K) =  "3") AND
                               (SPA-NAI-PTKOH-SBTKBN (IDK2)  =  "3")
                           MOVE    "0088"      TO  SPA-ERRCD
                           MOVE    23          TO  SPA-GMN-CUR
                           MOVE    IDX-K       TO  WRK-IDX1
                           MOVE    IDK2        TO  WRK-IDX2
                       END-IF
      *                生活保護・残留邦人の重複不可
                       IF      (SPA-NAI-PTKOH-SBTKBN (IDX-K) =  "5") AND
                               (SPA-NAI-PTKOH-SBTKBN (IDK2)  =  "5")
                           MOVE    "0127"      TO  SPA-ERRCD
                           MOVE    23          TO  SPA-GMN-CUR
                           MOVE    IDX-K       TO  WRK-IDX1
                           MOVE    IDK2        TO  WRK-IDX2
                       END-IF
      *H24.11
      *                長期高額療養費の重複不可
                       IF      (SPA-NAI-PTKOH-SBTKBN (IDX-K) =  "2") AND
                               (SPA-NAI-PTKOH-SBTKBN (IDK2)  =  "2")
                           MOVE    "0136"      TO  SPA-ERRCD
                           MOVE    23          TO  SPA-GMN-CUR
                           MOVE    IDX-K       TO  WRK-IDX1
                           MOVE    IDK2        TO  WRK-IDX2
                       END-IF
      *
      *ver.4.7
      *                ０７、０８入所中の重複不可
      *H30.4           948 医併、949 医療の追加
                       IF      (SPA-GMN-KOHNUM (IDX-K) =   "950"
                                                       OR  "951"
                                                       OR  "948"
                                                       OR  "949" )  AND
                               (SPA-GMN-KOHNUM (IDK2)  =   "950"
                                                       OR  "951"
                                                       OR  "948"
                                                       OR  "949" )
                           MOVE    "0135"      TO  SPA-ERRCD
                           MOVE    23          TO  SPA-GMN-CUR
                           MOVE    IDX-K       TO  WRK-IDX1
                           MOVE    IDK2        TO  WRK-IDX2
                       END-IF
      *
                   END-IF
      *
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD           =   SPACE
             OR   (SPA-ERRCD(1:1)      =   "K"   )
               GO      TO      3001-INPUT-KOH-CHK-EXT
           END-IF
      *
      *    新規のチェック
           MOVE    ZERO                TO  WRK-IDX-SIN1
           MOVE    ZERO                TO  WRK-IDX-SIN2
           MOVE    SPA-NAI-PTKOH-EDANUM (IDX-K)
                                       TO  IDX-K2
           IF     (IDX-K2                  =   ZERO      ) OR
                  (SPA-PTKOH-SINKIKBN (IDX-K2)
                                           =   "1"      )
               MOVE    IDX-K               TO  WRK-IDX-SIN1
           END-IF
           MOVE    SPA-NAI-PTKOH-EDANUM (WRK-IDX2) TO  IDX-K2
           IF     (IDX-K2                  =   ZERO      ) OR
                  (SPA-PTKOH-SINKIKBN (IDX-K2)
                                           =   "1"      )
               MOVE    WRK-IDX2            TO  WRK-IDX-SIN2
           END-IF
      *    ともに新規の時、エラーとする
           IF     (WRK-IDX-SIN1    NOT =   ZERO )  AND
                  (WRK-IDX-SIN2    NOT =   ZERO )
               GO      TO      3001-INPUT-KOH-CHK-EXT
           END-IF
      *
           IF      WRK-IDX-SIN1    NOT =   ZERO
               IF      (SPA-NAI-KTEKSTYMD (WRK-IDX-SIN1)
                                       <=  SPA-NAI-KTEKEDYMD(WRK-IDX2))
                   AND (SPA-NAI-KTEKSTYMD (WRK-IDX-SIN1)
                                       >   SPA-NAI-KTEKSTYMD(WRK-IDX2))
                   MOVE    WRK-IDX-SIN1    TO  SPA-KOH-IDX1
                   MOVE    WRK-IDX2        TO  SPA-KOH-IDX2
                   MOVE    "0106"          TO  SPA-PIDCD
                   MOVE    SPACE           TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      WRK-IDX-SIN2    NOT =   ZERO
               IF      (SPA-NAI-KTEKSTYMD (WRK-IDX-SIN2)
                                       <=  SPA-NAI-KTEKEDYMD(WRK-IDX1))
                   AND (SPA-NAI-KTEKSTYMD (WRK-IDX-SIN2)
                                       >   SPA-NAI-KTEKSTYMD(WRK-IDX1))
                   MOVE    WRK-IDX-SIN2    TO  SPA-KOH-IDX1
                   MOVE    WRK-IDX1        TO  SPA-KOH-IDX2
                   MOVE    "0106"          TO  SPA-PIDCD
                   MOVE    SPACE           TO  SPA-ERRCD
               END-IF
           END-IF
           .
       3001-INPUT-KOH-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費期間重複チェック処理（ＴＢＬ分）
      *****************************************************************
       3002-TBL-KOH-CHK-SEC    SECTION.
      *
           MOVE    ZERO                TO  WRK-IDX1
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX             >   SPA-PTKOH-MAX )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               MOVE    ZERO            TO  FLG-CHK
      *        現在表示分、削除分はチェックしない
               IF     (IDX             =   WRK-IDX (1) OR
                                           WRK-IDX (2)  OR
                                           WRK-IDX (3) ) OR
                      (SPA-PTKOH-SAKJOKBN(IDX)     =   "1" )
                   CONTINUE
               ELSE
      *            範囲の重複
                   IF      (SPA-PTKOH-TEKEDYMD (IDX)
                                       <   SPA-NAI-KTEKSTYMD(IDX-K)) OR
                           (SPA-PTKOH-TEKSTYMD (IDX)
                                       >   SPA-NAI-KTEKEDYMD(IDX-K))
                       CONTINUE
                   ELSE
                       MOVE    1            TO  FLG-CHK
                   END-IF
               END-IF
      *
      *
               IF      FLG-CHK         =   1
      *            特定は重複可とする
                   IF     (SPA-GMN-KOHNUM (IDX-K)
                                           =   SPA-PTKOH-KOHNUM(IDX))
                       IF     (SPA-GMN-KOHNUM(IDX-K)
                                               =   "051"
                                               OR  "052"
                                               OR  "091" 
      *                   肝炎治療　追加
                                               OR  "038"
      *ver.4.7            生活保護　追加
                                               OR  "012"
      *                   更正・育成追加
                                               OR  "015"  OR  "016"  )
      *                  北海道の地方公費
                         OR  ((SPA-GMN-KOHNUM(IDX-K)   =   "183" ) AND
                              (SPA-PREFNUM             =   01    ))
      *H25.7              第三者行為
                         OR   (SPA-GMN-KOHNUM(IDX-K)   =   "970" )
      *H27.1             難病
                          OR ( SPA-GMN-KOHNUM(IDX-K)   =   "054" )
      *H30.11
      *                肝がん治療
                          OR ( SPA-GMN-KOHNUM(IDX-K)   =   "092" )
      *H28.2
      *                京都府　京都子　H25.9.1から
                          OR ((SPA-GMN-KOHNUM(IDX-K)   =   "745" ) AND
                              (SPA-PREFNUM             =   26    ) AND
                              (SPA-NAI-KTEKSTYMD (IDX-K)
                                                       >=  "20130901"))
      *
                             MOVE    ZERO              TO  FLG-CHK
                       END-IF
                   END-IF
               END-IF
               IF      FLG-CHK         =   1
      *            [公費]期間重複チェック
                   IF     (SPA-GMN-KOHNUM (IDX-K)
                                       =   SPA-PTKOH-KOHNUM (IDX))  OR
                         ((SPA-NAI-PTKOH-SBTKBN (IDX-K) =   "3")  AND
                          (SPA-PTKOH-HKN-SBTKBN (IDX)   =   "3"))
                       MOVE    "0088"      TO  SPA-ERRCD
                       MOVE    23          TO  SPA-GMN-CUR
                       MOVE    IDX         TO  WRK-IDX1
                   END-IF
      *
      *            生活保護・残留邦人の重複不可
                   IF      (SPA-NAI-PTKOH-SBTKBN (IDX-K) =  "5") AND
                           (SPA-PTKOH-HKN-SBTKBN (IDX)   =  "5")
                       MOVE    "0127"      TO  SPA-ERRCD
                       MOVE    23          TO  SPA-GMN-CUR
                       MOVE    IDX         TO  WRK-IDX1
                   END-IF
      *H24.11
      *            長期高額療養の重複不可
                   IF      (SPA-NAI-PTKOH-SBTKBN (IDX-K) =  "2") AND
                           (SPA-PTKOH-HKN-SBTKBN (IDX)   =  "2")
                       MOVE    "0136"      TO  SPA-ERRCD
                       MOVE    23          TO  SPA-GMN-CUR
                       MOVE    IDX         TO  WRK-IDX1
                   END-IF
      *ver.4.7
      *            ０７、０８入所中の重複不可
      *H30.4           948 医併、949 医療の追加
                   IF      (SPA-GMN-KOHNUM (IDX-K) =   "950"
                                                   OR  "951"
                                                   OR  "948"
                                                   OR  "949" )  AND
                           (SPA-PTKOH-KOHNUM(IDX)  =   "950"
                                                   OR  "951"
                                                   OR  "948"
                                                   OR  "949" )
                       MOVE    "0135"      TO  SPA-ERRCD
                       MOVE    23          TO  SPA-GMN-CUR
                       MOVE    IDX         TO  WRK-IDX1
                   END-IF
      *
               END-IF
      *
           END-PERFORM
      *
           IF      (WRK-IDX1           >   ZERO  )  AND
                   (SPA-PTKOH-TEKEDYMD (WRK-IDX1)
                                       >=  SPA-NAI-KTEKSTYMD(IDX-K)) AND
                   (SPA-PTKOH-TEKSTYMD (WRK-IDX1)
                                       <   SPA-NAI-KTEKSTYMD(IDX-K))
               AND (SPA-ERRCD(1:1) NOT =   "K"   )
      *
               MOVE    IDX-K           TO  SPA-KOH-IDX1
               MOVE    WRK-IDX1        TO  SPA-KOH-IDX2
               MOVE    1               TO  SPA-KOH-FLG
               MOVE    "0106"          TO  SPA-PIDCD
               MOVE    SPACE           TO  SPA-ERRCD
           END-IF
      *
           .
       3002-TBL-KOH-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費重複期間処理
      *****************************************************************
       300-KOHKIKAN-SYORI-SEC         SECTION.
      *
           IF      SPA-KOH-IDX1        =   ZERO   OR
                   SPA-KOH-IDX2        =   ZERO
               GO     TO     300-KOHKIKAN-SYORI-EXT
           END-IF
           IF       SPA-KOH-FLG        =   ZERO
               IF  (SPA-NAI-KTEKSTYMD (SPA-KOH-IDX1)
                                       <=  SPA-NAI-KTEKEDYMD
                                                    (SPA-KOH-IDX2)) AND
                   (SPA-NAI-KTEKSTYMD (SPA-KOH-IDX1)
                                       >   SPA-NAI-KTEKSTYMD
                                                     (SPA-KOH-IDX2))
                   CONTINUE
               ELSE
                   GO     TO     300-KOHKIKAN-SYORI-EXT
               END-IF
           ELSE
               IF  (SPA-NAI-KTEKSTYMD (SPA-KOH-IDX1)
                                       <=  SPA-PTKOH-TEKEDYMD
                                                    (SPA-KOH-IDX2)) AND
                   (SPA-NAI-KTEKSTYMD (SPA-KOH-IDX1)
                                       >   SPA-PTKOH-TEKSTYMD
                                                     (SPA-KOH-IDX2))
                   CONTINUE
                ELSE
                   GO     TO     300-KOHKIKAN-SYORI-EXT
               END-IF
           END-IF
      *
      *    現在表示分の開始日付−１日
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    SPA-NAI-KTEKSTYMD (SPA-KOH-IDX1)
                                       TO  LNK-DAY6-KIJUN
           MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    -1                  TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING
                                           STS-AREA-DAY
                                           LNK-DAY6-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY6-KEISAN     TO  WRK-SYMD
           ELSE
               GO      TO      300-KOHKIKAN-SYORI-EXT
           END-IF
      *
           PERFORM 410112-SEIYMDHEN-SEC
      *    期間変更
           IF      SPA-KOH-FLG         =   ZERO
               MOVE    WRK-SYMD        TO  SPA-NAI-KTEKEDYMD
                                                     (SPA-KOH-IDX2)
               MOVE    WRK-HENYMD      TO  SPA-GMN-KTEKEDYMD
                                                     (SPA-KOH-IDX2)
           ELSE
               MOVE    WRK-SYMD        TO  SPA-PTKOH-TEKEDYMD
                                                     (SPA-KOH-IDX2)
               MOVE    WRK-HENYMD      TO  SPA-PTKOH-TEKEDYMDW
                                                     (SPA-KOH-IDX2)
           END-IF
           EVALUATE    SPA-KOH-IDX1
               WHEN    1
                   MOVE    26              TO  SPA-GMN-CUR
               WHEN    2
                   MOVE    32              TO  SPA-GMN-CUR
               WHEN    3
                   MOVE    40              TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       300-KOHKIKAN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費終了期間処理
      *****************************************************************
       310-KOHEDYMD-SYORI-SEC         SECTION.
      *
           IF      SPA-KOH-IDX1        =   ZERO   OR
                   SPA-HKN-TEKEDYMD    =   SPACE
               GO     TO     310-KOHEDYMD-SYORI-EXT
           END-IF
      *
           MOVE    SPA-HKN-TEKEDYMD    TO  WRK-SYMD
           PERFORM 410112-SEIYMDHEN-SEC
      *    期間変更
           MOVE    WRK-SYMD        TO  SPA-NAI-KTEKEDYMD (SPA-KOH-IDX1)
           MOVE    WRK-HENYMD      TO  SPA-GMN-KTEKEDYMD (SPA-KOH-IDX1)
           MOVE    SPA-SYSYMD      TO  SPA-NAI-KKAKUNINYMD(SPA-KOH-IDX1)
           MOVE    SPA-SYSYMDWH    TO  SPA-GMN-KKAKUNINYMD(SPA-KOH-IDX1)
      *    公費名編集
           MOVE    SPA-KOH-IDX1    TO  IDX-K1
      **** PERFORM 3101-HKNNUM-SET-SEC
      *
      *    老人公費の時、新規公費を追加する
      *    Ｈ１４．１０改正分のみ
           IF    ((SPA-GMN-KOHNUM (SPA-KOH-IDX1)   =   "027" )  AND
                  (SPA-NAI-PTKOH-PAYKBN (SPA-KOH-IDX1)
                                                   =   ZERO  ))
      *        次の開始日がある時、新規追加
               OR  (SPA-HKN2-TEKSTYMD   NOT =   SPACE  )
      *D       MOVE    1                   TO  SPA-KOH-ADD
               IF     (SPA-KOH-MAX         =   3 )  OR
                      (SPA-KOH-IDX1        =   3 )
                   CONTINUE
                   MOVE    1                   TO  SPA-KOH-ADD
               ELSE
                   ADD     1               TO  SPA-KOH-MAX
                   MOVE    SPA-KOH-MAX     TO  IDX-K
                   IF      SPA-KOH-IDX1    =   IDX-K
                       ADD     1               TO  IDX-K
                   END-IF
                   PERFORM 3101-RJN-KOHADD2-SEC
                   MOVE    ZERO                TO  SPA-KOH-ADD
                   INITIALIZE                  SPA-KOH-KENKAN-AREA
                   GO      TO      310-KOHEDYMD-SYORI-EXT
               END-IF
           ELSE
               MOVE    ZERO                TO  SPA-KOH-ADD
               INITIALIZE                  SPA-KOH-KENKAN-AREA
           END-IF
           MOVE    1                   TO  FLG-AUTO
           EVALUATE    SPA-KOH-IDX1
               WHEN    1
                   MOVE    26              TO  SPA-GMN-CUR
               WHEN    2
                   MOVE    32              TO  SPA-GMN-CUR
               WHEN    3
                   MOVE    40              TO  SPA-GMN-CUR
           END-EVALUATE
           MOVE    SPA-GMN-CUR         TO  WRK-CUR
      *
           .
       310-KOHEDYMD-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    公費の種類選択チェック・編集処理
      *****************************************************************
       3101-HKNNUM-SET-SEC           SECTION.
      *
      *    MOVE    ZERO                TO  FLG-OK
      *    PERFORM VARYING     IDX     FROM    1   BY  1
      *            UNTIL       IDX     >   SPA-GMN-KOHMAX
      *        IF     (SPA-GMN-KOHNUM(IDX-K1)
      *                               =    SPA-GMN-KOHNUML (IDX)) AND
      *               (SPA-GMN-KOHKBN(IDX-K1)
      *                               =    SPA-GMN-KOHKBNL (IDX))
      *            MOVE    SPA-GMN-KOHNUMLST (IDX)
      *                                    TO  SPA-GMN-KOHNUM-G(IDX-K1)
      *            MOVE    SPA-GMN-KOHMAX      TO  IDX
      *        END-IF
      *    END-PERFORM
      *
      *H26.12
      *    保険番号マスタの開始日の名称編集
           MOVE    SPA-GMN-KOHNUM (IDX-K1)     TO  WRK-HKNNUM
           MOVE    SPA-GMN-KOHKBN (IDX-K1)     TO  WRK-PAYKBN
           MOVE    SPA-NAI-KTEKSTYMD(IDX-K1)   TO  WRK-TEKSTYMD
      *
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    WRK-HKNNUM          TO  HKN-HKNNUM
           IF      WRK-PAYKBN          =   SPACE
               MOVE    ZERO                TO  HKN-PAYKBN
           ELSE
               MOVE    WRK-PAYKBN          TO  HKN-PAYKBN
           END-IF
           MOVE    WRK-TEKSTYMD        TO  HKN-TEKSTYMD
           MOVE    WRK-TEKSTYMD        TO  HKN-TEKEDYMD
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 921-HKNMST-NEXT-SEC
           ELSE
               INITIALIZE              HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    SPACE               TO  WRK-HKNKOHSBTKBN
           MOVE    SPACE               TO  WRK-TANSEIDONAME
           IF      FLG-HKNNUM          =   ZERO
               MOVE    HKN-HKNKOHSBTKBN    TO  WRK-HKNKOHSBTKBN
               MOVE    HKN-TANSEIDONAME    TO  WRK-TANSEIDONAME
      *        リストの名称より長くする
      *****    INSPECT WRK-TANSEIDONAME    REPLACING ALL "  " BY "　"
      *
               MOVE    WRK-TANSEIDONAME    TO  SPA-GMN-KOHNUMMEI
                                                            (IDX-K1)
               MOVE    WRK-HKNKOHSBTKBN    TO  SPA-NAI-PTKOH-SBTKBN
                                                            (IDX-K1)
           END-IF
      *
           .
       3101-HKNNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人公費切替え処理
      *****************************************************************
       320-RJNKOH-SYORI-SEC         SECTION.
      *
      *    開始日がＨ１８．１０以降の場合、補助区分変更
           IF      SPA-PTKOH-TEKSTYMD(SPA-KOH-IDX2)
                                       >=  "20061001"
               MOVE    SPA-KOH-GMN1        TO  IDX-K
               MOVE    "3"                 TO  SPA-GMN-KOHKBN (IDX-K)
      *        支払区分（３割）
               MOVE    "03"                TO  SPA-NAI-PTKOH-PAYKBN
                                                             (IDX-K)
               GO      TO      320-RJNKOH-SYORI-EXT
           END-IF
      *
      *    前の終了日保存
           MOVE    SPA-PTKOH-TEKEDYMD(SPA-KOH-IDX2)
                                       TO  WRK-TEKEDYMD
           MOVE    WRK-TEKEDYMD        TO  WRK-SYMD
           PERFORM 410112-SEIYMDHEN-SEC
           MOVE    WRK-HENYMD          TO  WRK-TEKEDYMD-W
      *    期間変更
           MOVE    "20060930"      TO  SPA-PTKOH-TEKEDYMD (SPA-KOH-IDX2)
           MOVE    SPA-SYSYMD      TO  SPA-PTKOH-KAKUNINYMD
                                                         (SPA-KOH-IDX2)
           MOVE    "H18.9.30"      TO  SPA-PTKOH-TEKEDYMDW
                                                         (SPA-KOH-IDX2)
           MOVE    SPA-SYSYMDWH    TO  SPA-PTKOH-KAKUNINYMDW
                                                         (SPA-KOH-IDX2)
      *    画面表示中
           IF      SPA-KOH-GMN1        >   ZERO
               MOVE    "20060930"      TO  SPA-NAI-KTEKEDYMD
                                                         (SPA-KOH-GMN1)
               MOVE    "H18.9.30"      TO  SPA-GMN-KTEKEDYMD
                                                         (SPA-KOH-GMN1)
               MOVE    SPA-SYSYMD      TO  SPA-NAI-KKAKUNINYMD
                                                         (SPA-KOH-GMN1)
               MOVE    SPA-SYSYMDWH    TO  SPA-GMN-KKAKUNINYMD
                                                         (SPA-KOH-GMN1)
           END-IF
      *
      *    ３割追加
           MOVE    SPA-KOH-GMN2        TO  IDX-K
           MOVE    SPA-KOH-IDX2        TO  IDX
      *
           MOVE    SPA-PTKOH-KOHNUM (IDX)
                                       TO  SPA-GMN-KOHNUM (IDX-K)
           MOVE    "3"                 TO  SPA-GMN-KOHKBN (IDX-K)
      *    支払区分（３割）
           MOVE    "03"                TO  SPA-NAI-PTKOH-PAYKBN(IDX-K)
           MOVE    SPA-PTKOH-FTNJANUM (IDX)
                                       TO  SPA-GMN-FTNJANUM (IDX-K)
           MOVE    SPA-PTKOH-JKYSNUM (IDX)
                                       TO  SPA-GMN-JKYSNUM (IDX-K)
           MOVE    "H18.10.01"         TO  SPA-GMN-KTEKSTYMD (IDX-K)
           MOVE    "20061001"          TO  SPA-NAI-KTEKSTYMD (IDX-K)
           MOVE    WRK-TEKEDYMD-W      TO  SPA-GMN-KTEKEDYMD (IDX-K)
           MOVE    WRK-TEKEDYMD        TO  SPA-NAI-KTEKEDYMD (IDX-K)
           MOVE    SPA-SYSYMD          TO  SPA-NAI-KKAKUNINYMD
                                                              (IDX-K)
           MOVE    SPA-SYSYMDWH        TO  SPA-GMN-KKAKUNINYMD
                                                              (IDX-K)
      *
            MOVE    SPA-PTKOH-HKN-SBTKBN (IDX)
                                       TO  SPA-NAI-PTKOH-SBTKBN(IDX-K)
      *
           MOVE    1                   TO  FLG-AUTO
           EVALUATE    SPA-KOH-IDX1
               WHEN    1
                   MOVE    26              TO  SPA-GMN-CUR
               WHEN    2
                   MOVE    32              TO  SPA-GMN-CUR
               WHEN    3
                   MOVE    40              TO  SPA-GMN-CUR
           END-EVALUATE
           MOVE    SPA-GMN-CUR         TO  WRK-CUR
      *
           .
       320-RJNKOH-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    老人公費の時、新規公費編集処理
      *****************************************************************
       3101-RJN-KOHADD-SEC           SECTION.
      *
      *
           INITIALIZE                  SPA-GMN-KOH-TBL (IDX-K)
           MOVE    ZERO                TO  IDX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-PTKOH-MAX
      *      老人公費以外
             IF    SPA-HKN2-TEKSTYMD   NOT =   SPACE
               IF     (SPA-HKN2-KOHNUM     =   SPA-PTKOH-KOHNUM (IDX))
                  IF  ((SPA-HKN2-KOHID NOT =   ZERO  )  AND
                       (SPA-HKN2-KOHID     =   SPA-PTKOH-KOHID(IDX)))
                   OR ( SPA-HKN2-JKYSNUM   =   SPA-PTKOH-JKYSNUM (IDX))
                       PERFORM 31012-IPN-KOHHEN-SEC
                       MOVE    SPA-PTKOH-MAX       TO  IDX
                   END-IF
               END-IF
             ELSE
      *        老人公費
               IF     (SPA-PTKOH-KOHNUM (IDX)  =   "027" )  AND
                      (SPA-PTKOH-PAYKBN (IDX)  =   ZERO  )  AND
                      (SPA-PTKOH-SAKJOKBN(IDX) =   SPACE )
                   PERFORM 31011-RJN-KOHHEN-SEC
                   MOVE    SPA-PTKOH-MAX       TO  IDX
               END-IF
             END-IF
           END-PERFORM
      *
           MOVE    ZERO                TO  SPA-KOH-ADD
           EVALUATE    IDX-K
               WHEN    1
                   MOVE    20              TO  SPA-GMN-CUR
               WHEN    2
                   MOVE    26              TO  SPA-GMN-CUR
               WHEN    3
                   MOVE    32              TO  SPA-GMN-CUR
           END-EVALUATE
           MOVE    1                   TO  FLG-AUTO
           MOVE    SPA-GMN-CUR         TO  WRK-CUR
      *
           .
       3101-RJN-KOHADD-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    老人公費追加編集処理
      *****************************************************************
       31011-RJN-KOHHEN-SEC           SECTION.
      *
           MOVE    SPA-PTKOH-KOHNUM (IDX)
                                       TO  SPA-GMN-KOHNUM (IDX-K)
           MOVE    "1"                 TO  SPA-GMN-KOHKBN (IDX-K)
      *    支払区分（１割）
           MOVE    "01"                TO  SPA-NAI-PTKOH-PAYKBN(IDX-K)
           MOVE    SPA-PTKOH-FTNJANUM (IDX)
                                       TO  SPA-GMN-FTNJANUM (IDX-K)
           MOVE    SPA-PTKOH-JKYSNUM (IDX)
                                       TO  SPA-GMN-JKYSNUM (IDX-K)
           MOVE    "H14.10.01"         TO  SPA-GMN-KTEKSTYMD (IDX-K)
           MOVE    "20021001"          TO  SPA-NAI-KTEKSTYMD (IDX-K)
           MOVE    "9999999"           TO  SPA-GMN-KTEKEDYMD (IDX-K)
           MOVE    ALL "9"             TO  SPA-NAI-KTEKEDYMD (IDX-K)
           MOVE    SPA-SYSYMD          TO  SPA-NAI-KKAKUNINYMD
                                                              (IDX-K)
           MOVE    SPA-SYSYMDWH        TO  SPA-GMN-KKAKUNINYMD
                                                              (IDX-K)
      *
            MOVE    SPA-PTKOH-HKN-SBTKBN (IDX)
                                       TO  SPA-NAI-PTKOH-SBTKBN(IDX-K)
      *
      *    開始日セット
           MOVE    SPACE               TO  HKNNUM-REC
           MOVE    SPA-HOSPNUM          TO  HKN-HOSPNUM
           MOVE    SPA-GMN-KOHNUM(IDX-K)
                                       TO  HKN-HKNNUM
           MOVE    SPA-NAI-PTKOH-PAYKBN (IDX-K)
                                       TO  HKN-PAYKBN
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 921-HKNMST-NEXT-SEC
           ELSE
               INITIALIZE              HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           MOVE    "tbl_hknnum"       TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-HKNNUM          =   ZERO
      *        開始日
               MOVE    HKN-TEKSTYMD        TO  WRK-SYMD
               PERFORM 410112-SEIYMDHEN-SEC
               MOVE    HKN-TEKSTYMD        TO  SPA-NAI-KTEKSTYMD (IDX-K)
               MOVE    WRK-HENYMD          TO  SPA-GMN-KTEKSTYMD (IDX-K)
      *        終了日
               IF      HKN-TEKEDYMD        =   ALL "9"
                   MOVE    "9999999"           TO  SPA-GMN-KTEKEDYMD
                                                                 (IDX-K)
               ELSE
                   MOVE    HKN-TEKEDYMD        TO  WRK-SYMD
                   PERFORM 410112-SEIYMDHEN-SEC
                   MOVE    WRK-HENYMD          TO  SPA-GMN-KTEKEDYMD
                                                                (IDX-K)
               END-IF
               MOVE    HKN-TEKEDYMD        TO  SPA-NAI-KTEKEDYMD (IDX-K)
           END-IF
           .
       31011-RJN-KOHHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人公費以外追加編集処理
      *****************************************************************
       31012-IPN-KOHHEN-SEC           SECTION.
      *
           MOVE    SPA-PTKOH-KOHNUM (IDX)
                                       TO  SPA-GMN-KOHNUM (IDX-K)
           MOVE    SPACE               TO  SPA-GMN-KOHKBN (IDX-K)
           MOVE    SPA-PTKOH-PAYKBN (IDX)
                                       TO  SPA-NAI-PTKOH-PAYKBN(IDX-K)
           MOVE    SPA-PTKOH-FTNJANUM (IDX)
                                       TO  SPA-GMN-FTNJANUM (IDX-K)
           MOVE    SPA-PTKOH-JKYSNUM (IDX)
                                       TO  SPA-GMN-JKYSNUM (IDX-K)
      *    開始日
           MOVE    SPA-HKN2-TEKSTYMD   TO  WRK-SYMD
           PERFORM 410112-SEIYMDHEN-SEC
           MOVE    SPA-HKN2-TEKSTYMD   TO  SPA-NAI-KTEKSTYMD (IDX-K)
           MOVE    WRK-HENYMD          TO  SPA-GMN-KTEKSTYMD (IDX-K)
      *    終了日
           IF      SPA-HKN2-TEKEDYMD   =   ALL "9"
               MOVE    "9999999"           TO  SPA-GMN-KTEKEDYMD
                                                                 (IDX-K)
           ELSE
               MOVE    SPA-HKN2-TEKEDYMD   TO  WRK-SYMD
               PERFORM 410112-SEIYMDHEN-SEC
               MOVE    WRK-HENYMD          TO  SPA-GMN-KTEKEDYMD
                                                                (IDX-K)
           END-IF
           MOVE    SPA-HKN2-TEKEDYMD   TO  SPA-NAI-KTEKEDYMD (IDX-K)
           MOVE    SPA-SYSYMD          TO  SPA-NAI-KKAKUNINYMD
                                                              (IDX-K)
           MOVE    SPA-SYSYMDWH        TO  SPA-GMN-KKAKUNINYMD
                                                              (IDX-K)
      *
           MOVE    SPA-PTKOH-HKN-SBTKBN (IDX)
                                       TO  SPA-NAI-PTKOH-SBTKBN(IDX-K)
      *
      *H26.12
      *    保険番号マスタから名称編集
           MOVE    IDX-K           TO  IDX-K1
           PERFORM 3101-HKNNUM-SET-SEC
      *
           .
       31012-IPN-KOHHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人公費の時、新規公費編集処理（画面内）
      *****************************************************************
       3101-RJN-KOHADD2-SEC           SECTION.
      *
           IF     SPA-HKN2-TEKSTYMD    NOT =   SPACE
      *        老人以外の公費追加
               PERFORM 310111-IPN-KOHADD2-SEC
               GO      TO      3101-RJN-KOHADD2-EXT
           END-IF
      *
           INITIALIZE                  SPA-GMN-KOH-TBL (IDX-K)
           MOVE    SPA-GMN-KOHNUM (SPA-KOH-IDX1)
                                       TO  SPA-GMN-KOHNUM (IDX-K)
           MOVE    "1"                 TO  SPA-GMN-KOHKBN (IDX-K)
      *    支払区分（１割）
           MOVE    "01"                TO  SPA-NAI-PTKOH-PAYKBN(IDX-K)
           MOVE    SPA-GMN-FTNJANUM (SPA-KOH-IDX1)
                                       TO  SPA-GMN-FTNJANUM (IDX-K)
           MOVE    SPA-GMN-JKYSNUM  (SPA-KOH-IDX1)
                                       TO  SPA-GMN-JKYSNUM (IDX-K)
           MOVE    "H14.10.01"         TO  SPA-GMN-KTEKSTYMD (IDX-K)
           MOVE    "20021001"          TO  SPA-NAI-KTEKSTYMD (IDX-K)
           MOVE    "9999999"           TO  SPA-GMN-KTEKEDYMD (IDX-K)
           MOVE    ALL "9"             TO  SPA-NAI-KTEKEDYMD (IDX-K)
           MOVE    SPA-SYSYMD          TO  SPA-NAI-KKAKUNINYMD
                                                              (IDX-K)
           MOVE    SPA-SYSYMDWH        TO  SPA-GMN-KKAKUNINYMD
                                                              (IDX-K)
      *
            MOVE    SPA-NAI-PTKOH-SBTKBN (SPA-KOH-IDX1)
                                       TO  SPA-NAI-PTKOH-SBTKBN(IDX-K)
      *
      *    開始日セット
           MOVE    SPACE               TO  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    SPA-GMN-KOHNUM(IDX-K)
                                       TO  HKN-HKNNUM
           MOVE    SPA-NAI-PTKOH-PAYKBN (IDX-K)
                                       TO  HKN-PAYKBN
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 921-HKNMST-NEXT-SEC
           ELSE
               INITIALIZE              HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-HKNNUM          =   ZERO
      *        開始日
               MOVE    HKN-TEKSTYMD        TO  WRK-SYMD
               PERFORM 410112-SEIYMDHEN-SEC
               MOVE    HKN-TEKSTYMD        TO  SPA-NAI-KTEKSTYMD (IDX-K)
               MOVE    WRK-HENYMD          TO  SPA-GMN-KTEKSTYMD (IDX-K)
      *        終了日
               IF      HKN-TEKEDYMD        =   ALL "9"
                   MOVE    "9999999"           TO  SPA-GMN-KTEKEDYMD
                                                                 (IDX-K)
               ELSE
                   MOVE    HKN-TEKEDYMD        TO  WRK-SYMD
                   PERFORM 410112-SEIYMDHEN-SEC
                   MOVE    WRK-HENYMD          TO  SPA-GMN-KTEKEDYMD
                                                                (IDX-K)
               END-IF
               MOVE    HKN-TEKEDYMD        TO  SPA-NAI-KTEKEDYMD (IDX-K)
           END-IF
      *
           MOVE    ZERO                TO  SPA-KOH-ADD
           EVALUATE    IDX-K
               WHEN    1
                   MOVE    20              TO  SPA-GMN-CUR
               WHEN    2
                   MOVE    26              TO  SPA-GMN-CUR
               WHEN    3
                   MOVE    32              TO  SPA-GMN-CUR
           END-EVALUATE
           MOVE    1                   TO  FLG-AUTO
           MOVE    SPA-GMN-CUR         TO  WRK-CUR
      *
           .
       3101-RJN-KOHADD2-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    老人以外の公費新規公費編集処理（画面内）
      *****************************************************************
       310111-IPN-KOHADD2-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-KOH-TBL (IDX-K)
           MOVE    SPA-GMN-KOH-TBL (SPA-KOH-IDX1)
                                       TO  SPA-GMN-KOH-TBL (IDX-K)
      *
           INITIALIZE                  SPA-NAI-PTKOH-EDANUM(IDX-K)
           INITIALIZE                  SPA-NAI-PTKOH-KOHID (IDX-K)
           INITIALIZE                  SPA-MAE-KOHNUM-G    (IDX-K)
      *    開始日
           MOVE    SPA-HKN2-TEKSTYMD   TO  WRK-SYMD
           PERFORM 410112-SEIYMDHEN-SEC
           MOVE    SPA-HKN2-TEKSTYMD   TO  SPA-NAI-KTEKSTYMD (IDX-K)
           MOVE    WRK-HENYMD          TO  SPA-GMN-KTEKSTYMD (IDX-K)
      *    終了日
           IF      SPA-HKN2-TEKEDYMD   =   ALL "9"
               MOVE    "9999999"           TO  SPA-GMN-KTEKEDYMD(IDX-K)
           ELSE
               MOVE    SPA-HKN2-TEKEDYMD   TO  WRK-SYMD
               PERFORM 410112-SEIYMDHEN-SEC
               MOVE    WRK-HENYMD          TO  SPA-GMN-KTEKEDYMD(IDX-K)
           END-IF
           MOVE    SPA-HKN2-TEKEDYMD   TO  SPA-NAI-KTEKEDYMD (IDX-K)
      *
           MOVE    SPA-SYSYMD          TO  SPA-NAI-KKAKUNINYMD
                                                              (IDX-K)
           MOVE    SPA-SYSYMDWH        TO  SPA-GMN-KKAKUNINYMD
                                                              (IDX-K)
      *H26.12
      *    保険番号マスタから名称編集
           MOVE    IDX-K           TO  IDX-K1
           PERFORM 3101-HKNNUM-SET-SEC
      *
      *
           MOVE    ZERO                TO  SPA-KOH-ADD
           EVALUATE    IDX-K
               WHEN    1
                   MOVE    20              TO  SPA-GMN-CUR
               WHEN    2
                   MOVE    26              TO  SPA-GMN-CUR
               WHEN    3
                   MOVE    32              TO  SPA-GMN-CUR
           END-EVALUATE
           MOVE    1                   TO  FLG-AUTO
           MOVE    SPA-GMN-CUR         TO  WRK-CUR
      *
           .
       310111-IPN-KOHADD2-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦→和暦FORMAT日付編集
      *****************************************************************
       410112-SEIYMDHEN-SEC           SECTION.
      *
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMD
           ELSE
               MOVE    SPACE               TO  WRK-HENYMD
           END-IF
           .
      *
       410112-SEIYMDHEN-EXT.
           EXIT.
      *****************************************************************
      *    誕生日の翌月取得処理
      *****************************************************************
       4910-BIRTH-AFTER1MON-GET-SEC     SECTION.
      *
           MOVE    SPACE            TO  STS-AREA-DAY
           INITIALIZE                   STS-AREA-DAY
           INITIALIZE                   LNK-DAY6-AREA
           MOVE    "61"             TO  LNK-DAY6-IRAI
           MOVE    SPA-NAI-BIRTHDAY TO  LNK-DAY6-KIJUN
           MOVE    "01"             TO  LNK-DAY6-KIJUN(7:2)
           MOVE    "2"              TO  LNK-DAY6-ZOGENPTN
           MOVE    1                TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"    USING   STS-AREA-DAY
                                        LNK-DAY6-AREA
           IF  STS-DAY-RC1          =   ZERO
               MOVE    LNK-DAY6-KEISAN  TO  WRK-BASEYMD
           ELSE
               MOVE    SPACE            TO  WRK-BASEYMD
               MOVE    "0099"           TO  SPA-ERRCD
           END-IF
      *
           .
       4910-BIRTH-AFTER1MON-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスタ次検索処理
      *****************************************************************
       921-HKNMST-NEXT-SEC              SECTION.
      *
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE              HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           .
       921-HKNMST-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者番号マスタ次検索処理
      *****************************************************************
       900-HKNJAINF-READ-SEC              SECTION.
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNJAINF-REC
               MOVE    ZERO                TO  FLG-HKNJAINF
           ELSE
               INITIALIZE                  HKNJAINF-REC
               MOVE    1                   TO  FLG-HKNJAINF
           END-IF
           .
       900-HKNJAINF-READ-EXT.
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
