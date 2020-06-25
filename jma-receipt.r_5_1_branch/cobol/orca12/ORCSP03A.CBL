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
       PROGRAM-ID.             ORCSP03A.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 保険入力チェックサブ（Ｐ０２Ｗ１のサブ）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/06/26    MCC-多々納    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-多々納  02/10/07  船員老人補助区分チェック追加
      * 01.00.02     NACL-多々納  02/10/18  任継の自動発生を２年チェックを５年に変更
      * 01.00.03     NACL-多々納  02/10/31  船員保険補助区分追加
      * 01.00.04     NACL-多々納  02/11/21  共済下船３月追加
      * 01.00.05     NACL-多々納  03/02/14  法別番号チェック区分追加
      * 01.00.06     NACL-多々納  03/03/19  Ｈ１５年４月改正対応追加
      * 01.00.07     NACL-多々納  03/07/08  保険者異動日チェック追加
      * 01.00.08     NACL-多々納  03/09/30  国保家族の被保険者編集修正
      * 01.00.09     NACL-多々納  03/11/26  任継の５５歳以上判定追加
      * 01.00.10     NACL-多々納  05/10/19  特別療養費追加
      *                                     自費補助区分変更修正
      * 01.00.11     NACL-多々納  05/11/15  MONFUNC 対応
      * 03.00.01     NACL-多々納  06/08/25  平成１８年１０月改正対応
      *                                     （老人割合変更）
      * 03.05.00     NACL-多々納  07/05/XX  グループ診療対応
      * 04.01.00     NACL-多々納  07/11/XX  公害保険対応
      * 04.01.00     NACL-多々納  07/12/12  記号番号空白チェック
      * 04.02.00     NACL-多々納  08/03/XX  平成２０年４月改正対応
      * 04.03.00     NACL-多々納  08/06/XX  家族年齢・被保険者名設定対応
      * 04.06.00     NACL-多々納  10/06/30  本人家族区分、補助区分変更対応
      * 04.07.00     NACL-多々納  12/07/XX  全後期高齢者の開始日対応
      * 04.07.00     NACL-多々納  14/03/XX  前期期高齢者２割対応
      * 04.07.00     NACL-多々納  14/08/08  高齢非該当２割対応
      *  05.00.00    NMED-多々納  20/04/08  被保険者証の枝番対応
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
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-KAKUTEI         PIC 9(01).
           03  FLG-KOH-CHK         PIC 9(01).
           03  FLG-HONKZKKBN       PIC 9(01).
      *
           03  FLG-RJNCHK          PIC 9(01).
      *H26.5
           03  FLG-RJN-02          PIC 9(01).
           03  FLG-RJNCHK2         PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDT                 PIC 9(04).
           03  IDH                 PIC 9(04).
           03  IDX2                PIC 9(02).
           03  IDX3                PIC 9(02).
      *
           03  IDX-S1              PIC 9(04).
           03  IDX-S2              PIC 9(04).
      *
           03  IDC                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMMDD.
                   07  WRK-SMM         PIC 9(02).
                   07  WRK-SDD         PIC 9(02).
      *
      *    検証番号チェック
           03  WRK-MOJ-AREA.
               05  WRK-MOJ-MAX         PIC 9(02).
               05  WRK-MOJ-NXT         PIC 9(02).
      *    保険区分
           03  WRK-HKNJA-HKNNUM        PIC X(03).
      *
           03  WRK-EDANUM          PIC 9(04).
           03  WRK-DAY-AREA.
               05  WRK-DAY-YMD     PIC X(08).
               05  WRK-DAY-ZOGEN   PIC S9(02).
               05  WRK-DAY-PTN     PIC X(01).
           03  WRK-WWYMD           PIC X(10).
      *
           03  WRK-HKNHBTNUM-1         PIC X(02).
           03  WRK-HKNHBTNUM-2         PIC X(02).
      *
           03  WRK-MAE-TEKEDYMD        PIC X(08).
           03  WRK-ATO-TEKSTYMD        PIC X(08).
      *
           03  WRK-TEKEDYMD            PIC X(08).
           03  WRK-TEKEDYMDW           PIC X(09).
      *
           03  WRK-IDX                 PIC 9(04).
           03  WRK-KYURATE1            PIC 9(03).
           03  WRK-KYURATE2            PIC 9(03).
      *
           03  WRK-CNT                 PIC 9(04).
      *
           03  WRK-SEINAME             PIC X(100).
      *
      *    半角全角変換
       01  WRK-HENKAN-AREA.
           03  WRK-MAE-INPUT       PIC X(100).
           03  WRK-ATO-INPUT       PIC X(100).
           03  WRK-MAX             PIC 9(04).
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
           03  WRK-HONKA-TBL.
               05  WRK-HONKA-REC-G.
                   07  FILLER        PIC X(06) VALUE "1 本人".
                   07  FILLER        PIC X(06) VALUE "2 家族".
               05  WRK-HONKA-REC-R   REDEFINES   WRK-HONKA-REC-G.
                   07  WRK-HONKA-REC   PIC X(06) OCCURS  2.
      *
           03  WRK-HJO-TBL.
               05  WRK-HJO-MAX         PIC 9(04).
               05  WRK-HJO-REC-G1.
                   07  FILLER          PIC X(10) VALUE "1 職務".
                   07  FILLER          PIC X(10) VALUE "2 下船".
                   07  FILLER          PIC X(10) VALUE "3 通勤".
                   07  FILLER          PIC X(10) VALUE "8 ２割".
                   07  FILLER          PIC X(10) VALUE "9 １割".
                   07  FILLER          PIC X(10) VALUE "A １割職務".
                   07  FILLER          PIC X(10) VALUE "B １割下船".
                   07  FILLER          PIC X(10) VALUE "C １割通勤".
                   07  FILLER          PIC X(10) VALUE "D ２割職務".
                   07  FILLER          PIC X(10) VALUE "E ２割下船".
                   07  FILLER          PIC X(10) VALUE "F ２割通勤".
               05  WRK-HJO-REC-R1      REDEFINES   WRK-HJO-REC-G1.
                   07  WRK-HJO-REC1    PIC X(10) OCCURS  11.
               05  WRK-HJO-REC-G2.
                   07  FILLER          PIC X(16) VALUE "0 無し".
                   07  FILLER          PIC X(16) VALUE "1 １割".
                   07  FILLER          PIC X(16) VALUE "2 ２割".
                   07  FILLER          PIC X(16) VALUE "3 ３割".
                   07  FILLER          PIC X(16) VALUE
                                                 "4 外１割 入０割".
                   07  FILLER          PIC X(16) VALUE
                                                 "5 外２割 入１割".
                   07  FILLER          PIC X(16) VALUE
                                                 "6 外３割 入２割".
               05  WRK-HJO-REC-R2      REDEFINES   WRK-HJO-REC-G2.
                   07  WRK-HJO-REC2    PIC X(16) OCCURS  7.
               05  WRK-HJO-REC-G3.
                   07  FILLER          PIC X(06) VALUE "1 課税".
                   07  FILLER          PIC X(06) VALUE "2 非課".
               05  WRK-HJO-REC-R3      REDEFINES   WRK-HJO-REC-G3.
                   07  WRK-HJO-REC3    PIC X(06) OCCURS  2.
      *        老人の補助区分追加
               05  WRK-HJO-REC-G4.
                   07  FILLER          PIC X(06) VALUE "8 ２割".
                   07  FILLER          PIC X(06) VALUE "9 １割".
               05  WRK-HJO-REC-R4      REDEFINES   WRK-HJO-REC-G4.
                   07  WRK-HJO-REC4    PIC X(06) OCCURS  2.
      *        共済の下船後補助区分追加
               05  WRK-HJO-REC-G5.
                   07  FILLER          PIC X(10) VALUE "2 下船３月".
                   07  FILLER          PIC X(10) VALUE "8 ２割".
                   07  FILLER          PIC X(10) VALUE "9 １割".
                   07  FILLER          PIC X(10) VALUE "B １割下船".
                   07  FILLER          PIC X(10) VALUE "E ２割下船".
               05  WRK-HJO-REC-R5      REDEFINES   WRK-HJO-REC-G5.
                   07  WRK-HJO-REC5    PIC X(10) OCCURS  5.
      *        船員（Ｈ１８．１０以降）
               05  WRK-HJO-REC-G1-2.
                   07  FILLER          PIC X(10) VALUE "1 職務".
                   07  FILLER          PIC X(10) VALUE "2 下船".
                   07  FILLER          PIC X(10) VALUE "3 通勤".
                   07  FILLER          PIC X(10) VALUE "7 ３割".
                   07  FILLER          PIC X(10) VALUE "9 １割".
                   07  FILLER          PIC X(10) VALUE "A １割職務".
                   07  FILLER          PIC X(10) VALUE "B １割下船".
                   07  FILLER          PIC X(10) VALUE "C １割通勤".
                   07  FILLER          PIC X(10) VALUE "G ３割職務".
                   07  FILLER          PIC X(10) VALUE "H ３割下船".
                   07  FILLER          PIC X(10) VALUE "I ３割通勤".
               05  WRK-HJO-REC-R1-2    REDEFINES   WRK-HJO-REC-G1-2.
                   07  WRK-HJO-REC1-2  PIC X(10) OCCURS  11.
      *        老人の補助区分追加（Ｈ１８．１０以降）
               05  WRK-HJO-REC-G4-2.
                   07  FILLER          PIC X(06) VALUE "7 ３割".
                   07  FILLER          PIC X(06) VALUE "9 １割".
               05  WRK-HJO-REC-R4-2    REDEFINES   WRK-HJO-REC-G4-2.
                   07  WRK-HJO-REC4-2  PIC X(06) OCCURS  2.
      *        共済の下船後補助区分追加（Ｈ１８．１０以降）
               05  WRK-HJO-REC-G5-2.
                   07  FILLER          PIC X(10) VALUE "2 下船３月".
                   07  FILLER          PIC X(10) VALUE "7 ３割".
                   07  FILLER          PIC X(10) VALUE "9 １割".
                   07  FILLER          PIC X(10) VALUE "B １割下船".
                   07  FILLER          PIC X(10) VALUE "H ３割下船".
               05  WRK-HJO-REC-R5-2      REDEFINES   WRK-HJO-REC-G5-2.
                   07  WRK-HJO-REC5-2    PIC X(10) OCCURS  5.
      *
      *        後期高齢者の補助区分（Ｈ２０．４以降）
               05  WRK-HJO-REC-G6.
                   07  FILLER          PIC X(10) VALUE "1 １割".
                   07  FILLER          PIC X(10) VALUE "3 ３割".
               05  WRK-HJO-REC-R6        REDEFINES   WRK-HJO-REC-G6.
                   07  WRK-HJO-REC6      PIC X(10) OCCURS  2.
      *
      *H25.5 開始日（S19.4.2生　以降）
      *        老人の補助区分追加（前期高齢者）
               05  WRK-HJO-REC-G4-3.
                   07  FILLER          PIC X(06) VALUE "7 ３割".
                   07  FILLER          PIC X(06) VALUE "8 ２割".
               05  WRK-HJO-REC-R4-3    REDEFINES   WRK-HJO-REC-G4-3.
                   07  WRK-HJO-REC4-3      PIC X(06) OCCURS  2.
      *        共済の下船後補助区分追加（前期高齢者）
               05  WRK-HJO-REC-G5-3.
                   07  FILLER          PIC X(10) VALUE "2 下船３月".
                   07  FILLER          PIC X(10) VALUE "7 ３割".
                   07  FILLER          PIC X(10) VALUE "8 ２割".
                   07  FILLER          PIC X(10) VALUE "E ２割下船".
                   07  FILLER          PIC X(10) VALUE "H ３割下船".
               05  WRK-HJO-REC-R5-3    REDEFINES   WRK-HJO-REC-G5-3.
                   07  WRK-HJO-REC5-3      PIC X(10) OCCURS  5.
      *        船員（前期高齢者）
               05  WRK-HJO-REC-G1-3.
                   07  FILLER          PIC X(10) VALUE "1 職務".
                   07  FILLER          PIC X(10) VALUE "2 下船".
                   07  FILLER          PIC X(10) VALUE "3 通勤".
                   07  FILLER          PIC X(10) VALUE "7 ３割".
                   07  FILLER          PIC X(10) VALUE "8 ２割".
                   07  FILLER          PIC X(10) VALUE "D ２割職務".
                   07  FILLER          PIC X(10) VALUE "E ２割下船".
                   07  FILLER          PIC X(10) VALUE "F ２割通勤".
                   07  FILLER          PIC X(10) VALUE "G ３割職務".
                   07  FILLER          PIC X(10) VALUE "H ３割下船".
                   07  FILLER          PIC X(10) VALUE "I ３割通勤".
               05  WRK-HJO-REC-R1-3    REDEFINES   WRK-HJO-REC-G1-3.
                   07  WRK-HJO-REC1-3  PIC X(10) OCCURS  11.
      *
      *
           03  WRK-KEI-TBL.
               05  WRK-KEI-REC-G.
                   07  FILLER          PIC X(06) VALUE "1 継続".
                   07  FILLER          PIC X(06) VALUE "2 任継".
                   07  FILLER          PIC X(06) VALUE "3 特別".
               05  WRK-KEI-REC-R       REDEFINES   WRK-KEI-REC-G.
                   07  WRK-KEI-REC     PIC X(06) OCCURS  3.
               05  WRK-CONT-MAX        PIC 9(04).
      *
           03  WRK-KEI1            PIC X(06)   VALUE  "（継）".
           03  WRK-KEI2            PIC X(06)   VALUE  "（任）".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    保険番号マスタ
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
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   チェックデジット
           COPY    "CPORCCHKDGT.INC".
      *
      *   画面日付変換サブ
          COPY    "CPORCSGDAY.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *    全角チェックパラメタ（拡張漢字）
           COPY    "CPORCSKANACHK2.INC".
      *
      *    拡張文字対応
           COPY    "CPORCSSTRING.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
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
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           EVALUATE    SPA-PIDCD
      *        老人保険自動切り替え処理
               WHEN    "0104"
                   PERFORM 310-RJNHENKAN-SYORI-SEC
                   MOVE    SPACE           TO  SPA-PIDCD
      *        保険重複期間処理
               WHEN    "0105"
                   PERFORM 300-HKNKIKAN-SYORI-SEC
                   MOVE    SPACE           TO  SPA-PIDCD
      *        老人２割切替え処理（Ｈ１８．１０改正）
               WHEN    "0110"
                   PERFORM 330-RJNHKNKIKAN-SYORI-SEC
                   MOVE    SPACE           TO  SPA-PIDCD
      *        保険自動切り替え処理
               WHEN    "0201"
               WHEN    "0202"
               WHEN    "0203"
               WHEN    "0204"
                   PERFORM 320-HKNCHANGE-SYORI-SEC
                   MOVE    SPACE           TO  SPA-PIDCD
      *
               WHEN    OTHER
                   MOVE    SPACE           TO  SPA-PIDCD
      *            基本チェック
                   PERFORM 100-KIHON-CHK-SEC
                   IF      SPA-ERRCD           =   SPACE
      *                関連チェックを行う
                       PERFORM 200-KANREN-CHK-SEC
                   END-IF
           END-EVALUATE
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
      *    保険者番号
           IF      SPA-ERRCD           =   SPACE
               PERFORM 100-HKNJANUM-CHK-SEC
           END-IF
      *
      *    保険の種類
           IF      SPA-ERRCD           =   SPACE
               PERFORM 1001-HKNNUM-CHK-SEC
           END-IF
      *
      *    労災保険の時、以下処理なし
           IF      SPA-GMN-HKNNUM      =   "971"
                                       OR  "973"
               GO      TO      100-KIHON-CHK-EXT
           END-IF
      *
      *    本家
           IF      SPA-ERRCD           =   SPACE
               PERFORM 1001-HONKZKKBN-CHK-SEC
           END-IF
      *    補助
           IF      SPA-ERRCD           =   SPACE
               PERFORM 1001-HOJOKBN-CHK-SEC
           END-IF
      *    継続
           IF      SPA-ERRCD           =   SPACE
               PERFORM 1001-CONTKBN-CHK-SEC
           END-IF
      *    記号・番号
           IF      SPA-ERRCD           =   SPACE
               PERFORM 1001-KIGO-NUM-CHK-SEC
           END-IF
      *    資格取得日
           IF      SPA-ERRCD           =   SPACE
               PERFORM 1001-SKKGETYMD-CHK-SEC
           END-IF
      *    適用開始日
           IF      SPA-ERRCD           =   SPACE
               PERFORM 1001-HTEKSTYMD-CHK-SEC
           END-IF
      *    適用終了日
           IF      SPA-ERRCD           =   SPACE
               PERFORM 1001-HTEKEDYMD-CHK-SEC
           END-IF
      *    被保険者
           IF      SPA-ERRCD           =   SPACE
               PERFORM 1001-HIHKNJANAME-CHK-SEC
           END-IF
      *    確認日
           IF      SPA-ERRCD           =   SPACE
               PERFORM 1001-HKAKUNINYMD-CHK-SEC
           END-IF
           .
       100-KIHON-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    保険者番号チェック処理
      ****************************************************************
       100-HKNJANUM-CHK-SEC     SECTION.
      *
      *    未入力判定
           IF      SPA-GMN-HKNJANUM    =   SPACE
               MOVE    SPACE               TO  SPA-HKNJA-AREA
               INITIALIZE                      SPA-HKNJA-AREA
               GO      TO      100-HKNJANUM-CHK-EXT
           END-IF
      *
      *    桁数チェック
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-HKNJANUM    TO  KANACHK-MAE-INPUT
           MOVE    8                   TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-HKNJANUM
      *    半角で間に空白がないこと
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-RC2     NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   2     )
               MOVE    "0041"              TO  SPA-ERRCD
               MOVE    8                   TO  SPA-GMN-CUR
               GO      TO      100-HKNJANUM-CHK-EXT
           END-IF
           IF      KANACHK-MAX         =   4  OR  6  OR  8
               MOVE    KANACHK-MAX         TO  WRK-MOJ-MAX
           ELSE
               MOVE    "0041"              TO  SPA-ERRCD
               MOVE    8                   TO  SPA-GMN-CUR
               GO      TO      100-HKNJANUM-CHK-EXT
           END-IF
      *
      *    保険者番号　検索チェック
           INITIALIZE                    HKNJAINF-REC
           MOVE    SPA-HOSPNUM           TO  HKNJA-HOSPNUM
           MOVE    SPA-GMN-HKNJANUM      TO  HKNJA-HKNJANUM
      *
           MOVE    HKNJAINF-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknjainf"        TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknjainf"        TO  MCP-TABLE
               MOVE    "key"                 TO  MCP-PATHNAME
               PERFORM 900-HKNJAINF-READ-SEC
           ELSE
               INITIALIZE                  HKNJAINF-REC
               MOVE    1               TO  FLG-HKNJAINF
           END-IF
           MOVE    "tbl_hknjainf"        TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    SPACE               TO  WRK-HKNJA-HKNNUM
      *    保険者番号あり
           IF      FLG-HKNJAINF        =   ZERO
               MOVE    HKNJA-HKNJANAME     TO  SPA-HKNJA-HKNJANAME
               MOVE    HKNJA-ADRS          TO  SPA-HKNJA-ADRS
               MOVE    HKNJA-BANTI         TO  SPA-HKNJA-BANTI
               MOVE    HKNJA-TEL           TO  SPA-HKNJA-TEL
               MOVE    HKNJA-HKNNUM        TO  WRK-HKNJA-HKNNUM
      *        記号が空白以外の時、再編集
               IF     (HKNJA-KIGO      NOT =   SPACE )  AND
                      (SPA-GMN-KIGO        =   SPACE )
                   MOVE    HKNJA-KIGO          TO  SPA-GMN-KIGO
               END-IF
      *
               MOVE    HKNJA-IDOYMD        TO  SPA-HKNJA-IDOYMD
               MOVE    HKNJA-IDOKBN        TO  SPA-HKNJA-IDOKBN
               MOVE    HKNJA-HON-GAIKYURATE
                                           TO  SPA-HKNJA-HON-GAIKYURATE
               MOVE    HKNJA-HON-NYUKYURATE
                                           TO  SPA-HKNJA-HON-NYUKYURATE
               MOVE    HKNJA-KZK-GAIKYURATE
                                           TO  SPA-HKNJA-KZK-GAIKYURATE
               MOVE    HKNJA-KZK-NYUKYURATE
                                           TO  SPA-HKNJA-KZK-NYUKYURATE
           ELSE
               MOVE    SPACE               TO  SPA-HKNJA-HKNJANAME
               MOVE    SPACE               TO  SPA-HKNJA-ADRS
               MOVE    SPACE               TO  SPA-HKNJA-BANTI
               MOVE    SPACE               TO  SPA-HKNJA-TEL
      *
               MOVE    SPACE               TO  SPA-HKNJA-KEIFLG
               MOVE    SPACE               TO  SPA-HKNJA-IDOYMD
               MOVE    SPACE               TO  SPA-HKNJA-IDOKBN
               MOVE    ZERO                TO  SPA-HKNJA-HON-GAIKYURATE
               MOVE    ZERO                TO  SPA-HKNJA-HON-NYUKYURATE
               MOVE    ZERO                TO  SPA-HKNJA-KZK-GAIKYURATE
               MOVE    ZERO                TO  SPA-HKNJA-KZK-NYUKYURATE
      *
               MOVE    SPACE               TO  WRK-HKNJA-HKNNUM
      *        保険者番号モジュラス10チェック
               IF      WRK-MOJ-MAX            >   4
                   PERFORM 1001-HKNNUM-CHECK-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       GO      TO      100-HKNJANUM-CHK-EXT
                   END-IF
               END-IF
      *
      *        法より編集
               EVALUATE    WRK-MOJ-MAX
                   WHEN    4
      *                政府管掌
                       MOVE    "001"           TO  WRK-HKNJA-HKNNUM
                   WHEN    6
      *                国保
                       MOVE    "060"           TO  WRK-HKNJA-HKNNUM
                   WHEN    8
      *            その他
                       PERFORM 1003-HKNNUM-HBTNUM-SEC
               END-EVALUATE
           END-IF
      *
           IF      WRK-HKNJA-HKNNUM    =   SPACE
               GO      TO      100-HKNJANUM-CHK-EXT
           END-IF
      *    更新の時、保険区分変更時、エラーとする
           IF     (SPA-NAI-HKNSTR      >   ZERO      ) AND
                  (SPA-PTHKN-SINKIKBN (SPA-NAI-HKNSTR)
                                       =    SPACE    )
      *        保険区分を変更する
               IF    ((WRK-HKNJA-HKNNUM    =   "060" ) AND
                      (SPA-MAE-HKNNUM      =   "068" ))
                  OR ((WRK-HKNJA-HKNNUM    =   "067" ) AND
                      (SPA-MAE-HKNNUM      =   "069" ))
                  OR ((WRK-HKNJA-HKNNUM    =   "039" ) AND
                      (SPA-MAE-HKNNUM      =   "040" ))
                   MOVE    SPA-MAE-HKNNUM      TO  WRK-HKNJA-HKNNUM
               END-IF
      *
               IF      WRK-HKNJA-HKNNUM    NOT =   SPA-MAE-HKNNUM
                   PERFORM 10011-HKNNUM-HENCHK-SEC
      *************IF      SPA-GMN-HKNJANUM    NOT =   SPA-MAE-HKNJANUM
      *                MOVE    "0042"          TO  SPA-ERRCD
      *                MOVE    8               TO  SPA-GMN-CUR
      ************ END-IF
               END-IF
           ELSE
               IF      WRK-HKNJA-HKNNUM    NOT =   SPA-GMN-HKNNUM
      *            労災保険は変更できない
                   IF     (WRK-HKNJA-HKNNUM    =   "971" OR "973")  OR
                          (SPA-GMN-HKNNUM      =   "971" OR "973")
                       MOVE    "0043"          TO  SPA-ERRCD
                       MOVE    61              TO  SPA-GMN-CUR
                       GO      TO      100-HKNJANUM-CHK-EXT
                   END-IF
      *            保険区分を変更する
                   IF    ((WRK-HKNJA-HKNNUM    =   "060" ) AND
                          (SPA-GMN-HKNNUM      =   "068" ))
                      OR ((WRK-HKNJA-HKNNUM    =   "067" ) AND
                          (SPA-GMN-HKNNUM      =   "069" ))
                      OR ((WRK-HKNJA-HKNNUM    =   "039" ) AND
                          (SPA-GMN-HKNNUM      =   "040" ))
                        CONTINUE
                   ELSE
                        MOVE    WRK-HKNJA-HKNNUM    TO  SPA-GMN-HKNNUM
                   END-IF
               END-IF
           END-IF
           .
      *
       100-HKNJANUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者番号モジュラス10チェック処理
      *****************************************************************
       1001-HKNNUM-CHECK-SEC           SECTION.
      *
      *    入力された保険者番号をセットすれば、サブプロ内で
      *    検証してくれる (処理区分:2)
           INITIALIZE                      LNK-CHKDGT-AREA
           MOVE    2                   TO  LNK-I-SYORIKBN
           MOVE    SPA-GMN-HKNJANUM    TO  LNK-I-CODE
           CALL    "ORCSCHKDGT"        USING   LNK-CHKDGT-AREA
           IF      LNK-O-ERRCD     NOT =   ZERO
               MOVE    "0040"          TO  SPA-ERRCD
               MOVE    8               TO  SPA-GMN-CUR
           END-IF
      *
          .
       1001-HKNNUM-CHECK-EXT.
           EXIT.
      ****************************************************************
      *    法別番号から保険種類編集処理
      ****************************************************************
       1003-HKNNUM-HBTNUM-SEC     SECTION.
      *
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    SPA-GMN-HKNJANUM(1:2)
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
                MOVE    1                  TO  FLG-HKNNUM
           END-IF
           PERFORM UNTIL    FLG-HKNNUM     =  1
               IF      HKN-HKNKOHSBTKBN    =   "1"  OR  "4"
                                           OR  "8"  OR  "9"
                   MOVE    HKN-HKNNUM      TO  WRK-HKNJA-HKNNUM
                   MOVE    1               TO  FLG-HKNNUM
               END-IF
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
          .
       1003-HKNNUM-HBTNUM-EXT.
           EXIT.
      ****************************************************************
      *    変更時保険の種類チェック処理
      ****************************************************************
       10011-HKNNUM-HENCHK-SEC     SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNHBTNUM-1
           MOVE    SPACE               TO  WRK-HKNHBTNUM-2
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-HKNMAX
               IF      SPA-GMN-HKNNUML(IDX)  =   WRK-HKNJA-HKNNUM
                    MOVE    SPA-NAI-HKNHBTNUM (IDX)
                                             TO  WRK-HKNHBTNUM-1
               END-IF
               IF      SPA-GMN-HKNNUML(IDX)  =   SPA-MAE-HKNNUM
                    MOVE    SPA-NAI-HKNHBTNUM (IDX)
                                             TO  WRK-HKNHBTNUM-2
               END-IF
           END-PERFORM
           IF     (WRK-HKNHBTNUM-1     =   SPACE )  AND
                  (WRK-HKNHBTNUM-2     =   SPACE )
               IF     (WRK-HKNJA-HKNNUM    =   "060"
                                           OR  "001"
                                           OR  "068"
                                           OR  "069" )  OR
                      (SPA-MAE-HKNNUM      =   "060"
                                           OR  "001"
                                           OR  "068"
                                           OR  "069" )
                   MOVE    "0042"          TO  SPA-ERRCD
                   MOVE    8               TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF      WRK-HKNHBTNUM-1     NOT =   WRK-HKNHBTNUM-2
               MOVE    "0042"          TO  SPA-ERRCD
               MOVE    8               TO  SPA-GMN-CUR
           END-IF
      *
          .
       10011-HKNNUM-HENCHK-EXT.
           EXIT.
      ****************************************************************
      *    保険の種類チェック処理
      ****************************************************************
       1001-HKNNUM-CHK-SEC     SECTION.
      *
      *    更新の時、保険区分変更時、エラーとする
           IF     (SPA-NAI-HKNSTR      >   ZERO      ) AND
                  (SPA-PTHKN-SINKIKBN (SPA-NAI-HKNSTR)
                                       =   SPACE    )
               IF      SPA-MAE-HKNNUM  NOT =   SPA-GMN-HKNNUM
      *            前の種類へ戻す
                   MOVE    SPA-MAE-HKNNUM      TO  SPA-GMN-HKNNUM
                   PERFORM VARYING     IDX     FROM    1   BY  1
                           UNTIL       IDX     >   SPA-GMN-HKNMAX
                       IF      SPA-GMN-HKNNUML(IDX)  =   SPA-GMN-HKNNUM
                           MOVE    SPA-GMN-HKNNUMLST (IDX)
                                               TO  SPA-GMN-HKNNUM-G
      *                    識別
                           MOVE    SPA-NAI-HKNSBTKBN (IDX)
                                               TO  SPA-NAI-PTHKN-SBTKBN
                           MOVE    SPA-GMN-HKNMAX      TO  IDX
                       END-IF
                   END-PERFORM
                   MOVE    "0042"              TO  SPA-ERRCD
                   MOVE    9                   TO  SPA-GMN-CUR
               END-IF
           ELSE
      *        労災保険は入力できない
               IF     (SPA-NAI-HKNSTR      =   ZERO   ) AND
                      (SPA-GMN-HKNNUM      =   "971"
                                           OR  "973" )
                   MOVE    "0043"          TO  SPA-ERRCD
                   MOVE    61              TO  SPA-GMN-CUR
                   GO      TO      1001-HKNNUM-CHK-EXT
               END-IF
           END-IF
      *    労災保険の時、以下処理なし
           IF      SPA-GMN-HKNNUM      =   "971"
                                       OR  "973"
               GO      TO      1001-HKNNUM-CHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
      *    保険情報
           IF    ( SPA-GMN-HKNNUM  NOT =   SPACE     )  AND
                 ((SPA-NAI-HKNSTR      =   ZERO      ) OR
                  (SPA-PTHKN-SINKIKBN (SPA-NAI-HKNSTR)
                                       =   "1"       ))
      *        新規の時保険情報
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   SPA-GMN-HKNMAX
                   IF      SPA-GMN-HKNNUML (IDX)   =   SPA-GMN-HKNNUM
                       MOVE    SPA-GMN-HKNNUMLST (IDX)
                                               TO  SPA-GMN-HKNNUM-G
      *                法別番号が同じこと
                       IF     (SPA-GMN-HKNJANUM        =   SPACE) OR
                              (SPA-GMN-HKNNUM          =   "001"  OR
                                                           "060"  OR
                                                           "068"  OR
                                                           "069" )
                           CONTINUE
                       ELSE
      *                    法別番号チェック区分
                           IF     (SPA-NAI-HKNHBTNUMKBN(IDX)  =  "1" )
                              AND (SPA-GMN-HKNJANUM(1:2)
                                               NOT =  SPA-NAI-HKNHBTNUM
                                                              (IDX) )
                               MOVE    "0067"              TO  SPA-ERRCD
                           END-IF
                       END-IF
      *                識別
                       MOVE    SPA-NAI-HKNSBTKBN (IDX)
                                               TO  SPA-NAI-PTHKN-SBTKBN
                       MOVE    SPA-GMN-HKNMAX      TO  IDX
                       MOVE    1                   TO  FLG-CHK
                   END-IF
               END-PERFORM
               IF      SPA-ERRCD       NOT =   SPACE
                   MOVE    9               TO  SPA-GMN-CUR
               END-IF
               IF     (FLG-CHK             =   ZERO  )
                 AND  (SPA-ERRCD           =   SPACE )
                   MOVE    "0044"          TO  SPA-ERRCD
                   MOVE    9               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
      *    本人家族区分等
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-MAE-HKNNUM  NOT =   SPA-GMN-HKNNUM
                   PERFORM 31011-HKN-HEN-SEC
               END-IF
           END-IF
           .
      *
       1001-HKNNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    本人家族区分等編集処理
      *****************************************************************
       31011-HKN-HEN-SEC      SECTION.
      *
      *    本人家族
           IF      SPA-NAI-HONKZKKBN-FLG   =   "1"
               CONTINUE
           ELSE
             IF      SPA-GMN-HONKZKKBN-G     =   SPACE
      ***      １５歳未満の時、家族とする
      **       IF      SPA-NAI-NENREI-YY   <=  15
      *        家族年齢までを家族とする
               IF      SPA-NAI-NENREI-YY   <=  SPA-NAI-KZKNEN
                   MOVE    WRK-HONKA-REC (2)   TO  SPA-GMN-HONKZKKBN-G
               ELSE
                   MOVE    WRK-HONKA-REC (1)   TO  SPA-GMN-HONKZKKBN-G
               END-IF
             END-IF
           END-IF
      *    自費の時、本人
           IF      SPA-GMN-HKNNUM (1:2)    =   "98"
               MOVE    WRK-HONKA-REC (1)   TO  SPA-GMN-HONKZKKBN-G
           END-IF
      *    治験の時、本人
           IF      SPA-GMN-HKNNUM          >=  "900"  AND
                                           <=  "919"
               MOVE    WRK-HONKA-REC (1)   TO  SPA-GMN-HONKZKKBN-G
           END-IF
      *
           INITIALIZE                      SPA-GMN-HONKZKKBN-LIST
           MOVE    ZERO                TO  SPA-GMN-HONKZK-MAX
           PERFORM VARYING     IDT     FROM    1  BY  1
                   UNTIL       IDT     >   2
               MOVE    WRK-HONKA-REC (IDT)
                                       TO  SPA-GMN-HONKZKKBNL (IDT)
               MOVE    IDT             TO  SPA-GMN-HONKZK-MAX
               IF      SPA-GMN-HONKZKKBNL (IDT)(1:1)
                                           =   SPA-GMN-HONKZKKBN
                   MOVE    SPA-GMN-HONKZKKBNL (IDT)
                                           TO  SPA-GMN-HONKZKKBN-G
               END-IF
      *        自官は本人のみ
               IF      SPA-GMN-HKNNUM      =   "007"
      *        公害保険は本人のみ
                                           OR  "975"
      *        後期高齢者は本人のみ
                                           OR  "039"
                                           OR  "040"
      ************ IF      SPA-GMN-HONKZKKBN-G     =   SPACE
                   IF      IDT                 =   1
      *                空白の時、本人を設定
                       MOVE    SPA-GMN-HONKZKKBNL (IDT)
                                               TO  SPA-GMN-HONKZKKBN-G
                   END-IF
                   MOVE    2               TO  IDT
               END-IF
           END-PERFORM
      *
      *    継続区分
           INITIALIZE                      SPA-GMN-CONTKBN-LIST
           IF    ( SPA-NAI-PTHKN-SBTKBN    NOT =   "1"  ) OR
                 ( SPA-GMN-HKNNUM          =   "060"
                                           OR  "067"
      *              特別療養費追加
                                           OR  "068"
                                           OR  "069"
      *              後期高齢者追加
                                           OR  "039"
                                           OR  "040" )
               MOVE    ZERO                TO  SPA-GMN-CONT-MAX
           ELSE
               IF      SPA-GMN-HKNNUM      =   "003"
                   MOVE    3               TO  WRK-CONT-MAX
               ELSE
                   MOVE    2               TO  WRK-CONT-MAX
               END-IF
               PERFORM VARYING IDT     FROM    1  BY  1
                       UNTIL   IDT     >   WRK-CONT-MAX
                   MOVE    WRK-KEI-REC (IDT)
                                           TO  SPA-GMN-CONTKBNL (IDT)
                   IF      WRK-KEI-REC (IDT)(1:1)
                                           =   SPA-GMN-CONTKBN
                       MOVE    SPA-GMN-CONTKBNL (IDT)
                                               TO  SPA-GMN-CONTKBN-G
                   END-IF
                   MOVE    IDT             TO  SPA-GMN-CONT-MAX
               END-PERFORM
           END-IF
      *
      *    補助区分
           PERFORM 310110-HOJKBN-HENSYU-SEC
      *
           .
       31011-HKN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    補助区分編集処理
      *****************************************************************
       310110-HOJKBN-HENSYU-SEC            SECTION.
      *!!!
      *Ｈ２２．９　高齢者非該当判定
           PERFORM 310111-HIGAITOU-CHK-SEC
      *
           INITIALIZE                      SPA-GMN-HOJOKBN-LIST
      *
      *H26.4
      *    H26.5.1以降のS19.4.2生以降の高齢者負担割合
           MOVE    ZERO                TO  FLG-RJN-02
           IF      (SPA-NAI-BIRTHDAY       >=  "19440402")
             AND   (SPA-GMN-HKNNUM(1:1) NOT =  "9"       )
             AND  ((SPA-NAI-HTEKSTYMD      >=  SPA-BYMD-70)
               OR  (SPA-SYSYMD             >=  SPA-BYMD-70 ))
               IF      (SPA-NAI-HTEKSTYMD      >=  "20140501")
                   OR ((SPA-NAI-HTEKSTYMD      =   SPACE     ) AND
                       (SPA-SYSYMD             >=  "20140501"))
                   MOVE    1               TO  FLG-RJN-02
               END-IF
           END-IF
      *    後期高齢者も対象
           IF    ((SPA-GMN-HKNNUM(1:1) =   "9"   )  OR
                  (SPA-GMN-HKNNUM      =   "039"
                                       OR  "040" ))
               CONTINUE
           ELSE
               IF      (FLG-RJNCHK2        =   1   )
                  AND ((SPA-NAI-HTEKSTYMD  >=  SPA-BYMD-75-2 ) OR
                       (SPA-SYSYMD         >=  SPA-BYMD-75-2 ))
                   IF      (SPA-NAI-HTEKSTYMD  >=  "20140501")
                       OR ((SPA-NAI-HTEKSTYMD  =   SPACE     ) AND
                           (SPA-SYSYMD         >=  "20140501"))
                       MOVE    1               TO  FLG-RJN-02
                   END-IF
               END-IF
           END-IF
      *
      *!!
      *H30.11
      *    開始日が７０歳未満は一般
           IF    (FLG-RJN-02               =   1     )
             AND (SPA-NAI-HTEKSTYMD    NOT =   SPACE )
               IF    (SPA-NAI-HTEKSTYMD    <   SPA-BYMD-70)
                   MOVE    ZERO            TO  FLG-RJN-02
               END-IF
           END-IF
      *!
           IF      FLG-RJN-02          =   1
               PERFORM 3101104-HOJOKBN-HEN02-SEC
           ELSE
      *
           EVALUATE    SPA-GMN-HKNNUM
      *        船員
               WHEN    "002"
                   IF   (  SPA-BYMD-70             >   ZERO      )  AND
                        (((SPA-NAI-HTEKSTYMD       >=  "20021001") AND
                          (SPA-NAI-HTEKSTYMD       >=  SPA-BYMD-70)) OR
                         ((SPA-NAI-HTEKSTYMD       =   SPACE     ) AND
                          (SPA-SYSYMD              >=  "20021001") AND
                          (SPA-SYSYMD              >=  SPA-BYMD-70)))
                       MOVE    4               TO  IDY
                       MOVE    11              TO  WRK-HJO-MAX
                   ELSE
                       MOVE    1               TO  IDY
                       MOVE    3               TO  WRK-HJO-MAX
                   END-IF
                   MOVE    ZERO                TO  SPA-GMN-HOJO-MAX
                   PERFORM VARYING     IDT     FROM    IDY BY  1
                           UNTIL       IDT     >   WRK-HJO-MAX
                       ADD     1           TO  SPA-GMN-HOJO-MAX
      *            Ｈ１８．１０ 改正より
                       IF     (SPA-NAI-HTEKSTYMD   >=  "20061001")
                          OR ((SPA-NAI-HTEKSTYMD   =   SPACE  )  AND
                              (SPA-SYSYMD          >=  "20061001"))
                           MOVE    WRK-HJO-REC1-2 (IDT)
                                           TO  SPA-GMN-HOJOKBNL
                                                    (SPA-GMN-HOJO-MAX)
                       ELSE
                           MOVE    WRK-HJO-REC1 (IDT)
                                           TO  SPA-GMN-HOJOKBNL
                                                    (SPA-GMN-HOJO-MAX)
                       END-IF
                   END-PERFORM
                   IF     (SPA-GMN-HOJOKBN     =   SPACE      )  AND
                          (SPA-NAI-BIRTHDAY    >=  "19321001" )
      *!!
      *            高齢者非該当以外
                   AND    (FLG-RJNCHK          =   0    )
      *                Ｈ１４．１０以降、老人は１割を初期とする
                       IF    ((SPA-NAI-HTEKSTYMD   >=  "20021001" ) AND
                              (SPA-NAI-HTEKSTYMD   >=  SPA-BYMD-70)) OR
                             ((SPA-SYSYMD          >=  "20021001" ) AND
                              (SPA-SYSYMD          >=  SPA-BYMD-70))
                           IF     (SPA-BYMD-70       >   ZERO      )
                               MOVE    "9"         TO  SPA-GMN-HOJOKBN
                           END-IF
                       END-IF
      *           前の補助区分が空白の時、そのままとする
                       IF     (SPA-MAE-HOJOKBN     =   SPACE )
                       AND    (SPA-NAI-HKNSTR  NOT =   ZERO  )
                       AND    (SPA-PTHKN-SINKIKBN (SPA-NAI-HKNSTR)
                                                   = SPACE)
                           MOVE    SPA-MAE-HOJOKBN TO  SPA-GMN-HOJOKBN
                       END-IF
                   END-IF
      *        国保
               WHEN    "060"
               WHEN    "068"
                   PERFORM VARYING   IDT     FROM    1  BY  1
                         UNTIL       IDT     >   7
                       MOVE    WRK-HJO-REC2 (IDT)
                                           TO  SPA-GMN-HOJOKBNL (IDT)
                       MOVE    IDT         TO  SPA-GMN-HOJO-MAX
                   END-PERFORM
                   IF     (SPA-GMN-HOJOKBN     =   SPACE )
      *           前の補助区分が空白の時、そのままとする
                      IF  (SPA-MAE-HOJOKBN     =   SPACE )
                      AND (SPA-NAI-HKNSTR  NOT =   ZERO  )
                      AND (SPA-PTHKN-SINKIKBN (SPA-NAI-HKNSTR) = SPACE)
                       CONTINUE
                      ELSE
      *                給付割合から
                       IF     (SPA-HKNJA-HON-GAIKYURATE  NOT =   ZERO)
                          OR  (SPA-HKNJA-HON-NYUKYURATE  NOT =   ZERO)
                          OR  (SPA-HKNJA-KZK-GAIKYURATE  NOT =   ZERO)
                          OR  (SPA-HKNJA-KZK-NYUKYURATE  NOT =   ZERO)
                           PERFORM 3101101-HOJO-KYURATE-SEC
                       ELSE
                           MOVE    "3"             TO  SPA-GMN-HOJOKBN
                       END-IF
      *
      *                Ｈ１４．１０以降、老人は１割を初期とする
                       IF    ((SPA-NAI-HTEKSTYMD   >=  "20021001" ) AND
                              (SPA-NAI-HTEKSTYMD   >=  SPA-BYMD-70)) OR
                             ((SPA-SYSYMD          >=  "20021001" ) AND
                              (SPA-SYSYMD          >=  SPA-BYMD-70))
                           IF     (SPA-BYMD-70       >   ZERO      )
      *!!
      *                    高齢者非該当以外
                           AND    (FLG-RJNCHK          =   0    )
                               MOVE    "1"         TO  SPA-GMN-HOJOKBN
                           END-IF
                       END-IF
                       END-IF
                   END-IF
      *        共済
               WHEN    "031"
               WHEN    "032"
               WHEN    "033"
               WHEN    "034"
                   IF    ((SPA-NAI-HTEKSTYMD       >=  "20021001") AND
                          (SPA-NAI-HTEKSTYMD       >=  SPA-BYMD-70)) OR
                         ((SPA-NAI-HTEKSTYMD       =   SPACE     ) AND
                          (SPA-SYSYMD              >=  "20021001") AND
                          (SPA-SYSYMD              >=  SPA-BYMD-70))
                       MOVE    2               TO  IDY
                       MOVE    5               TO  WRK-HJO-MAX
                   ELSE
                       MOVE    1               TO  IDY
                       MOVE    1               TO  WRK-HJO-MAX
                   END-IF
                   IF      SPA-BYMD-70         =   ZERO
                       MOVE    1               TO  IDY
                       MOVE    1               TO  WRK-HJO-MAX
                   END-IF
                   MOVE    ZERO                TO  SPA-GMN-HOJO-MAX
                   PERFORM VARYING     IDT     FROM    IDY BY  1
                           UNTIL       IDT     >   WRK-HJO-MAX
                       ADD     1           TO  SPA-GMN-HOJO-MAX
      *            Ｈ１８．１０ 改正より
                       IF     (SPA-NAI-HTEKSTYMD   >=  "20061001")
                          OR ((SPA-NAI-HTEKSTYMD   =   SPACE  )  AND
                              (SPA-SYSYMD          >=  "20061001"))
                           MOVE    WRK-HJO-REC5-2 (IDT)
                                           TO  SPA-GMN-HOJOKBNL
                                                    (SPA-GMN-HOJO-MAX)
                       ELSE
                           MOVE    WRK-HJO-REC5 (IDT)
                                           TO  SPA-GMN-HOJOKBNL
                                                    (SPA-GMN-HOJO-MAX)
                       END-IF
                   END-PERFORM
                   IF     (SPA-GMN-HOJOKBN     =   SPACE      )  AND
                          (SPA-NAI-BIRTHDAY    >=  "19321001" )
      *!!
      *            高齢者非該当以外
                   AND    (FLG-RJNCHK          =   0    )
      *!!
      *                Ｈ１４．１０以降、老人は１割を初期とする
                       IF    ((SPA-NAI-HTEKSTYMD   >=  "20021001" ) AND
                              (SPA-NAI-HTEKSTYMD   >=  SPA-BYMD-70)) OR
                             ((SPA-SYSYMD          >=  "20021001" ) AND
                              (SPA-SYSYMD          >=  SPA-BYMD-70))
                           IF     (SPA-BYMD-70       >   ZERO      )
                               MOVE    "9"         TO  SPA-GMN-HOJOKBN
                           END-IF
                       END-IF
      *           前の補助区分が空白の時、そのままとする
                       IF     (SPA-MAE-HOJOKBN     =   SPACE )
                       AND    (SPA-NAI-HKNSTR  NOT =   ZERO  )
                       AND    (SPA-PTHKN-SINKIKBN (SPA-NAI-HKNSTR)
                                                   = SPACE)
                           MOVE    SPA-MAE-HOJOKBN TO  SPA-GMN-HOJOKBN
                       END-IF
                   END-IF
      *Ｈ２０．４
      *        後期高齢者
               WHEN    "039"
               WHEN    "040"
                   PERFORM VARYING   IDT     FROM    1  BY  1
                         UNTIL       IDT     >   2
                       MOVE    WRK-HJO-REC6 (IDT)
                                           TO  SPA-GMN-HOJOKBNL (IDT)
                       MOVE    IDT         TO  SPA-GMN-HOJO-MAX
                   END-PERFORM
                   IF      SPA-GMN-HOJOKBN     =   SPACE
      *                給付割合から
                       MOVE    "1"             TO  SPA-GMN-HOJOKBN
                   END-IF
      *        その他
               WHEN    OTHER
                   EVALUATE    SPA-GMN-HKNNUM (1:2)
      *            自費
                   WHEN    "98"
                       PERFORM VARYING     IDT     FROM    1  BY  1
                               UNTIL       IDT     >   2
                           MOVE    WRK-HJO-REC3 (IDT)
                                           TO  SPA-GMN-HOJOKBNL (IDT)
                           MOVE    IDT         TO  SPA-GMN-HOJO-MAX
                       END-PERFORM
                       IF      SPA-GMN-HOJOKBN     =   SPACE
      *                    新規の時、システム管理より
                           IF      SPA-NAI-JIHIHKN-TAX     =   "1"
      *                        非課税
                               MOVE    "2"         TO SPA-GMN-HOJOKBN
                           ELSE
      *                        課税
                               MOVE    "1"         TO SPA-GMN-HOJOKBN
                           END-IF
                       END-IF
      *            治験
                   WHEN    "90"
                   WHEN    "91"
                       PERFORM VARYING     IDT     FROM    1  BY  1
                               UNTIL       IDT     >   2
                           MOVE    WRK-HJO-REC3 (IDT)
                                           TO  SPA-GMN-HOJOKBNL (IDT)
                           MOVE    IDT         TO  SPA-GMN-HOJO-MAX
                       END-PERFORM
      *??              IF      SPA-GMN-HOJOKBN     =   SPACE
      *                    課税
      *??                  MOVE    "1"         TO SPA-GMN-HOJOKBN
      *??              END-IF
      *                新規は課税を初期表示
                       IF      (SPA-NAI-HKNSTR      =   ZERO      ) OR
                               (SPA-PTHKN-SINKIKBN (SPA-NAI-HKNSTR)
                                                    =   "1"       )
                           IF      SPA-GMN-HOJOKBN     =   SPACE
      *                        課税
                               MOVE    "1"         TO  SPA-GMN-HOJOKBN
                           END-IF
                       END-IF
                   WHEN    OTHER
                       MOVE    ZERO        TO  SPA-GMN-HOJO-MAX
      *                Ｈ１４．１０ 改正より
                       IF     (SPA-NAI-HTEKSTYMD     >=  "20021001") OR
                              (SPA-NAI-HTEKSTYMD     =   SPACE  AND
                               SPA-SYSYMD            >=  "20021001" )
                           PERFORM 310111-HKNTBL-RJNHEN-SEC
                       END-IF
                   END-EVALUATE
           END-EVALUATE
      *
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDT     FROM    1  BY  1
                   UNTIL       IDT     >   SPA-GMN-HOJO-MAX
               IF      SPA-GMN-HOJOKBNL (IDT)(1:1)
                                           =   SPA-GMN-HOJOKBN
                   MOVE    SPA-GMN-HOJOKBNL (IDT)
                                           TO  SPA-GMN-HOJOKBN-G
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    SPACE           TO  SPA-GMN-HOJOKBN-G
           END-IF
      *
           .
       310110-HOJKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    給付割合から補助区分編集処理
      *****************************************************************
       3101101-HOJO-KYURATE-SEC            SECTION.
      *
           IF      SPA-GMN-HONKZKKBN   =   "1"
      *        本人
               MOVE    SPA-HKNJA-HON-GAIKYURATE  TO  WRK-KYURATE1
               MOVE    SPA-HKNJA-HON-NYUKYURATE  TO  WRK-KYURATE2
           ELSE
      *        家族
               MOVE    SPA-HKNJA-KZK-GAIKYURATE  TO  WRK-KYURATE1
               MOVE    SPA-HKNJA-KZK-NYUKYURATE  TO  WRK-KYURATE2
           END-IF
           EVALUATE    WRK-KYURATE1  ALSO  WRK-KYURATE2
               WHEN    000           ALSO  000
                   MOVE    "3"             TO  SPA-GMN-HOJOKBN
               WHEN    080           ALSO  080
                   MOVE    "2"             TO  SPA-GMN-HOJOKBN
               WHEN    090           ALSO  090
                   MOVE    "1"             TO  SPA-GMN-HOJOKBN
               WHEN    100           ALSO  100
                   MOVE    "0"             TO  SPA-GMN-HOJOKBN
      *
               WHEN    090           ALSO  100
                   MOVE    "4"             TO  SPA-GMN-HOJOKBN
               WHEN    080           ALSO  090
                   MOVE    "5"             TO  SPA-GMN-HOJOKBN
               WHEN    000           ALSO  080
                   MOVE    "6"             TO  SPA-GMN-HOJOKBN
               WHEN    OTHER
                   MOVE    "3"             TO  SPA-GMN-HOJOKBN
           END-EVALUATE
      *
           .
       3101101-HOJO-KYURATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人補助区分編集処理
      *****************************************************************
       310111-HKNTBL-RJNHEN-SEC            SECTION.
      *
      *    Ｈ１４．１０時点で７０歳以上の時補助区分は空白でも可とする
           IF      (SPA-NAI-BIRTHDAY   <    "19321001" )  AND
                   (SPA-GMN-HOJOKBN    =   SPACE       )
               GO      TO      310111-HKNTBL-RJNHEN-EXT
           END-IF
      *    適用開始日が７０歳以上で、労災・治験・自費以外
           IF     ((SPA-NAI-HTEKSTYMD  >=  SPA-BYMD-70)  OR
                   (SPA-NAI-HTEKSTYMD  =   SPACE    AND
                    SPA-SYSYMD         >=  SPA-BYMD-70))  AND
                   (SPA-BYMD-70        >   ZERO        )  AND
                   (SPA-GMN-HKNNUM(1:1)    NOT =   "9" )
               PERFORM VARYING     IDT     FROM    1  BY  1
                       UNTIL       IDT     >   2
      *            Ｈ１８．１０ 改正より
                   IF     (SPA-NAI-HTEKSTYMD   >=  "20061001")
                      OR ((SPA-NAI-HTEKSTYMD   =   SPACE  )  AND
                          (SPA-SYSYMD          >=  "20061001"))
                       MOVE    WRK-HJO-REC4-2 (IDT)
                                           TO  SPA-GMN-HOJOKBNL (IDT)
                   ELSE
                       MOVE    WRK-HJO-REC4 (IDT)
                                           TO  SPA-GMN-HOJOKBNL (IDT)
                   END-IF
                   MOVE    IDT             TO  SPA-GMN-HOJO-MAX
               END-PERFORM
      *        初期値は１割とする
               IF      SPA-GMN-HOJOKBN     =   SPACE
      ******       MOVE    "9"             TO  SPA-GMN-HOJOKBN
      *        前の補助区分が空白の時、そのままとする
                   IF      (SPA-MAE-HOJOKBN     =   SPACE )
                       AND (SPA-NAI-HKNSTR  NOT =   ZERO  )
                       AND (SPA-PTHKN-SINKIKBN (SPA-NAI-HKNSTR)
                                                = SPACE   )
                       CONTINUE
                   ELSE
      *!!!
      *                高齢者非該当
                       IF      (FLG-RJNCHK          =   1 )
                           CONTINUE
                       ELSE
      *!!!
                           MOVE    "9"             TO  SPA-GMN-HOJOKBN
                       END-IF
                   END-IF
               END-IF
           END-IF
           .
       310111-HKNTBL-RJNHEN-EXT.
           EXIT.
      *
      *H22.9
      ****************************************************************
      *    高齢非該当判定処理
      ****************************************************************
       310111-HIGAITOU-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-RJNCHK
           MOVE    ZERO                TO  FLG-RJNCHK2
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   3
              IF     (SPA-GMN-KOHNUM (IDX)     =   "976" ) 
               AND   (SPA-NAI-KTEKSTYMD(IDX)   <=  SPA-NAI-HTEKSTYMD)
               AND   (SPA-NAI-KTEKEDYMD(IDX)   >=  SPA-NAI-HTEKEDYMD)
                   MOVE    1                   TO  FLG-RJNCHK
               END-IF
      *H26.8
               IF     (SPA-GMN-KOHNUM (IDX)    =   "977" ) 
               AND   (SPA-NAI-KTEKSTYMD(IDX)   <=  SPA-NAI-HTEKSTYMD)
               AND   (SPA-NAI-KTEKEDYMD(IDX)   >=  SPA-NAI-HTEKEDYMD)
                   MOVE    1                   TO  FLG-RJNCHK2
               END-IF
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-PTKOH-MAX   )
                         OR   (FLG-RJNCHK      =   1           )
      *        画面表示以外の高齢者非該当
               IF     (IDX                        >   SPA-NAI-KOHEND   )
                 AND  (SPA-PTKOH-TEKSTYMD (IDX)   <=  SPA-NAI-HTEKSTYMD)
                 AND  (SPA-PTKOH-TEKEDYMD (IDX)   >=  SPA-NAI-HTEKEDYMD)
                 AND  (SPA-PTKOH-SAKJOKBN (IDX) NOT =  "1"        )
                   IF     (SPA-PTKOH-KOHNUM  (IDX) =   "976" )
                       MOVE    1                   TO  FLG-RJNCHK
                   END-IF
                   IF     (SPA-PTKOH-KOHNUM  (IDX) =   "977" )
                       MOVE    1                   TO  FLG-RJNCHK2
                   END-IF
               END-IF
           END-PERFORM
           .
       310111-HIGAITOU-CHK-EXT.
           EXIT.
      *
      *H26.4
      *****************************************************************
      *    高齢者補助区分編集（２割対応）処理(開始日H26.5から）
      *****************************************************************
       3101104-HOJOKBN-HEN02-SEC            SECTION.
      *
      *
           EVALUATE    SPA-GMN-HKNNUM
      *        船員
               WHEN    "002"
                   MOVE    4                   TO  IDY
                   MOVE    11                  TO  WRK-HJO-MAX
                   MOVE    ZERO                TO  SPA-GMN-HOJO-MAX
                   PERFORM VARYING     IDT     FROM    IDY BY  1
                           UNTIL       IDT     >   WRK-HJO-MAX
                       ADD     1           TO  SPA-GMN-HOJO-MAX
                       MOVE    WRK-HJO-REC1-3 (IDT)
                                           TO  SPA-GMN-HOJOKBNL
                                                    (SPA-GMN-HOJO-MAX)
                   END-PERFORM
                   IF     (SPA-GMN-HOJOKBN     =   SPACE  )
      *            高齢者非該当以外
                   AND    (FLG-RJNCHK          =   0      )
      *                ２割を初期とする
                       MOVE    "8"             TO  SPA-GMN-HOJOKBN
      *           前の補助区分が空白の時、そのままとする
                       IF     (SPA-MAE-HOJOKBN     =   SPACE )
                       AND    (SPA-NAI-HKNSTR  NOT =   ZERO  )
                       AND    (SPA-PTHKN-SINKIKBN (SPA-NAI-HKNSTR)
                                                   = SPACE)
                           MOVE    SPA-MAE-HOJOKBN TO  SPA-GMN-HOJOKBN
                       END-IF
                   END-IF
      *        国保
               WHEN    "060"
               WHEN    "068"
                   PERFORM VARYING   IDT     FROM    1  BY  1
                         UNTIL       IDT     >   7
                       MOVE    WRK-HJO-REC2 (IDT)
                                           TO  SPA-GMN-HOJOKBNL (IDT)
                       MOVE    IDT         TO  SPA-GMN-HOJO-MAX
                   END-PERFORM
                   IF     (SPA-GMN-HOJOKBN     =   SPACE )
      *           前の補助区分が空白の時、そのままとする
                      IF  (SPA-MAE-HOJOKBN     =   SPACE )
                      AND (SPA-NAI-HKNSTR  NOT =   ZERO  )
                      AND (SPA-PTHKN-SINKIKBN (SPA-NAI-HKNSTR) = SPACE)
                       CONTINUE
                      ELSE
      *                給付割合から
                       IF     (SPA-HKNJA-HON-GAIKYURATE  NOT =   ZERO)
                          OR  (SPA-HKNJA-HON-NYUKYURATE  NOT =   ZERO)
                          OR  (SPA-HKNJA-KZK-GAIKYURATE  NOT =   ZERO)
                          OR  (SPA-HKNJA-KZK-NYUKYURATE  NOT =   ZERO)
                           PERFORM 3101101-HOJO-KYURATE-SEC
                       ELSE
                           MOVE    "3"             TO  SPA-GMN-HOJOKBN
                       END-IF
      *
      *                老人は２割を初期とする
      *                高齢者非該当以外
                       IF         (FLG-RJNCHK          =   0    )
                            MOVE    "2"            TO  SPA-GMN-HOJOKBN
                       END-IF
                      END-IF
                   END-IF
      *        共済
               WHEN    "031"
               WHEN    "032"
               WHEN    "033"
               WHEN    "034"
                   MOVE    2                   TO  IDY
                   MOVE    5                   TO  WRK-HJO-MAX
                   MOVE    ZERO                TO  SPA-GMN-HOJO-MAX
                   PERFORM VARYING     IDT     FROM    IDY BY  1
                           UNTIL       IDT     >   WRK-HJO-MAX
                       ADD     1           TO  SPA-GMN-HOJO-MAX
                       MOVE    WRK-HJO-REC5-3 (IDT)
                                           TO  SPA-GMN-HOJOKBNL
                                                    (SPA-GMN-HOJO-MAX)
                   END-PERFORM
                   IF     (SPA-GMN-HOJOKBN     =   SPACE ) 
      *            高齢者非該当以外
                   AND    (FLG-RJNCHK          =   0    )
      *                ２割を初期とする
                       MOVE    "8"             TO  SPA-GMN-HOJOKBN
      *           前の補助区分が空白の時、そのままとする
                       IF     (SPA-MAE-HOJOKBN     =   SPACE )
                       AND    (SPA-NAI-HKNSTR  NOT =   ZERO  )
                       AND    (SPA-PTHKN-SINKIKBN (SPA-NAI-HKNSTR)
                                                   = SPACE)
                           MOVE    SPA-MAE-HOJOKBN TO  SPA-GMN-HOJOKBN
                       END-IF
                   END-IF
      *        後期高齢者（１割）
               WHEN    "039"
               WHEN    "040"
                   PERFORM VARYING   IDT     FROM    1  BY  1
                         UNTIL       IDT     >   2
                       MOVE    WRK-HJO-REC6 (IDT)
                                           TO  SPA-GMN-HOJOKBNL (IDT)
                       MOVE    IDT         TO  SPA-GMN-HOJO-MAX
                   END-PERFORM
                   IF      SPA-GMN-HOJOKBN     =   SPACE
      *                給付割合から
                       MOVE    "1"             TO  SPA-GMN-HOJOKBN
                   END-IF
      *        その他
               WHEN    OTHER
                   MOVE    ZERO        TO  SPA-GMN-HOJO-MAX
                   PERFORM VARYING     IDT     FROM    1  BY  1
                           UNTIL       IDT     >   2
                       MOVE    WRK-HJO-REC4-3 (IDT)
                                           TO  SPA-GMN-HOJOKBNL (IDT)
                       MOVE    IDT             TO  SPA-GMN-HOJO-MAX
                   END-PERFORM
      *            初期値は２割とする
                   IF     (SPA-GMN-HOJOKBN     =   SPACE)
      *            高齢者非該当以外
                   AND    (FLG-RJNCHK          =   0    )
      *            前の補助区分が空白の時、そのままとする
                       IF      (SPA-MAE-HOJOKBN     =   SPACE )
                           AND (SPA-NAI-HKNSTR  NOT =   ZERO  )
                           AND (SPA-PTHKN-SINKIKBN (SPA-NAI-HKNSTR)
                                                    = SPACE   )
                           CONTINUE
                       ELSE
                           MOVE    "8"             TO  SPA-GMN-HOJOKBN
                       END-IF
                   END-IF
           END-EVALUATE
      *
           .
       3101104-HOJOKBN-HEN02-EXT.
           EXIT.
      *
      ****************************************************************
      *    本人家族区分チェック処理
      ****************************************************************
       1001-HONKZKKBN-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-HONKZKKBN   =   SPACE
               GO      TO      1001-HONKZKKBN-CHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-HONKZK-MAX
               IF      SPA-GMN-HONKZKKBN   =   SPA-GMN-HONKZKKBNL
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-HONKZKKBNL(IDX)
                                               TO  SPA-GMN-HONKZKKBN-G
                   MOVE    1                   TO  FLG-CHK
               END-IF
           END-PERFORM
           IF     FLG-CHK              =   ZERO
               MOVE    "0045"          TO  SPA-ERRCD
               MOVE    10              TO  SPA-GMN-CUR
           END-IF
      *
      *    船員で老人家族の時、１割を対象とする
           IF      (SPA-GMN-HKNNUM     =   "002")  AND
                   (SPA-GMN-HOJO-MAX   =   08    )  AND
                   (SPA-GMN-HOJOKBN    =   SPACE)  AND
                   (SPA-GMN-HONKZKKBN  =   "2" )
              AND  (SPA-NAI-BIRTHDAY   >=  "19321001" )
               IF     (SPA-NAI-HKNSTR      =   ZERO      ) OR
                      (SPA-PTHKN-SINKIKBN (SPA-NAI-HKNSTR)
                                           =   "1"    )
                   MOVE    "9"             TO  SPA-GMN-HOJOKBN
                   PERFORM 1001-HOJOKBN-CHK-SEC
               END-IF
           END-IF
      *
      *   国保で新規の場合
           IF     (SPA-MAE-HONKZKKBN   NOT =   SPA-GMN-HONKZKKBN) AND
                  (SPA-GMN-HKNNUM          =   "060"
                                           OR  "068" )  AND
                 ((SPA-NAI-HKNSTR          =   ZERO  ) OR
                  (SPA-PTHKN-SINKIKBN (SPA-NAI-HKNSTR)
                                           =   "1"    ))
      *        前期高齢者以外
               IF    ((SPA-NAI-HTEKSTYMD   >=  "20021001" ) AND
                      (SPA-NAI-HTEKSTYMD   >=  SPA-BYMD-70)) OR
                     ((SPA-SYSYMD          >=  "20021001" ) AND
                      (SPA-SYSYMD          >=  SPA-BYMD-70))
                   CONTINUE
                ELSE
                   PERFORM 3101101-HOJO-KYURATE-SEC
               END-IF
           END-IF
           MOVE    SPA-GMN-HONKZKKBN   TO  SPA-MAE-HONKZKKBN
      *
           .
       1001-HONKZKKBN-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    補助区分チェック処理
      ****************************************************************
       1001-HOJOKBN-CHK-SEC     SECTION.
      *
           IF      SPA-GMN-HOJOKBN     =   SPACE
               MOVE    SPACE           TO  SPA-GMN-HOJOKBN-G
               GO      TO      1001-HOJOKBN-CHK-EXT
           END-IF
      *訂正の補助区分変更をすべて認める(2005.12)
      *    訂正で、老人割合編集分の時、補助区分の変更はできない
      *    特別療養費・自費は対象外
      *    IF     (SPA-GMN-HKNNUM      =   "068"  OR   "069" )
      *       OR  (SPA-GMN-HKNNUM(1:2) =   "98"      )
      *        CONTINUE
      *    ELSE
      *
      *    IF     (SPA-NAI-HKNSTR      >   ZERO      ) AND
      *           (SPA-PTHKN-SINKIKBN (SPA-NAI-HKNSTR)
      *                               NOT  =   "1"    ) AND
      *           (SPA-NAI-HTEKSTYMD   >=  "20021001" ) AND
      *           (SPA-NAI-HTEKSTYMD   >=  SPA-BYMD-70)
      *        IF      SPA-GMN-HOJOKBN     NOT =   SPA-MAE-HOJOKBN
      *            MOVE    "0068"          TO  SPA-ERRCD
      *            MOVE    11              TO  SPA-GMN-CUR
      *            GO      TO      1001-HOJOKBN-CHK-EXT
      *        END-IF
      *    END-IF
      *
      **** END-IF
      *
      *    国保で、割合を変更された時
      *    IF     (SPA-NAI-HKNSTR      >   ZERO      ) AND
      *           (SPA-PTHKN-SINKIKBN (SPA-NAI-HKNSTR)
      *                           NOT  =   "1"       ) AND
      *           (SPA-GMN-HKNNUM      =   "060"     )  AND
      *           (SPA-GMN-HOJOKBN NOT =   SPA-MAE-HOJOKBN)
      *        IF     (SPA-SYSYMD      >=   "20021001" )  AND
      *               (SPA-SYSYMD      >=   SPA-BYMD-70 )
      *            MOVE    "0068"          TO  SPA-ERRCD
      *            MOVE    11              TO  SPA-GMN-CUR
      *            GO      TO      1001-HOJOKBN-CHK-EXT
      *        END-IF
      *****END-IF
      *
      *    国保の前期高齢者の補助区分判定
      *    IF    ( SPA-GMN-HKNNUM      =   "060"  OR  "068"  )  AND
      *          ( SPA-NAI-BIRTHDAY    >   "19320930"        )  AND
      *          ((SPA-NAI-HTEKSTYMD   >=  "20021001"   ) AND
      *           (SPA-NAI-HTEKSTYMD   >=  SPA-BYMD-70  )    )
      *        IF      (SPA-GMN-HOJOKBN    =   "3"  OR  "6")
      *            Ｈ１８．１０ 改正 チェックなし
      *            IF      SPA-NAI-HTEKSTYMD   >=  "20061001"
      *                CONTINUE
      *            ELSE
      *
      *            MOVE    "1066"          TO  SPA-ERRCD
      *            MOVE    11              TO  SPA-GMN-CUR
      *            GO      TO      1001-HOJOKBN-CHK-EXT
      *            END-IF
      *        END-IF
      ***  END-IF
      *
      *    船員家族で老人以外時、補助区分は入力できない
           IF     (SPA-GMN-HONKZKKBN   =   "2"       ) AND
                  (SPA-GMN-HKNNUM      =   "002"     )
               IF      (SPA-GMN-HOJO-MAX    =   3       ) AND
                       (SPA-GMN-HOJOKBN     NOT  =   SPACE   )
                   MOVE    "1065"          TO  SPA-ERRCD
                   MOVE    11              TO  SPA-GMN-CUR
                   GO      TO      1001-HOJOKBN-CHK-EXT
               END-IF
      *        老人
      *        Ｈ１８．１０ 改正
      *        IF      SPA-NAI-HTEKSTYMD   >=  "20061001"
      *            IF      (SPA-GMN-HOJO-MAX    =   8      )
      *                IF     (SPA-GMN-HOJOKBN     =   "7"
      *                                            OR  "9")
      *                    CONTINUE
      *                ELSE
      *                    MOVE    "1066"          TO  SPA-ERRCD
      *                    MOVE    11              TO  SPA-GMN-CUR
      *                END-IF
      *            END-IF
      *        ELSE
      *            IF      (SPA-NAI-HTEKSTYMD  NOT =   SPACE )
      *               AND  (SPA-GMN-HOJO-MAX       =   8     )
      *                IF     (SPA-GMN-HOJOKBN     =   "8"
      *                                            OR  "9")
      *                    CONTINUE
      *                ELSE
      *                    MOVE    "1066"          TO  SPA-ERRCD
      *                    MOVE    11              TO  SPA-GMN-CUR
      *                END-IF
      *            END-IF
      ******   END-IF
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      1001-HOJOKBN-CHK-EXT
           END-IF
      *
      *    共済家族で老人以外時、補助区分は入力できない
           IF     (SPA-GMN-HONKZKKBN   =   "2"       ) AND
                  (SPA-GMN-HKNNUM      =   "031" OR
                                           "032" OR
                                           "033" OR
                                           "034"     )
               IF      (SPA-GMN-HOJO-MAX    =   1    ) AND
                       (SPA-GMN-HOJOKBN     NOT =   SPACE   )
                   MOVE    "1065"          TO  SPA-ERRCD
                   MOVE    11              TO  SPA-GMN-CUR
                   GO      TO      1001-HOJOKBN-CHK-EXT
               END-IF
      *        老人
      *        Ｈ１８．１０ 改正
      *        IF      SPA-NAI-HTEKSTYMD   >=  "20061001"
      *            IF      (SPA-GMN-HOJO-MAX    =   4      )  AND
      *                    (SPA-GMN-HOJOKBN     =   "B"   OR
      *                                             "H"   )
      *                MOVE    "1066"          TO  SPA-ERRCD
      *                MOVE    11              TO  SPA-GMN-CUR
      *            END-IF
      *        ELSE
      *            IF      (SPA-GMN-HOJO-MAX    =   4      )  AND
      *                    (SPA-GMN-HOJOKBN     =   "B"   OR
      *                                             "E"   )
      *                MOVE    "1066"          TO  SPA-ERRCD
      *                MOVE    11              TO  SPA-GMN-CUR
      *            END-IF
      *******  END-IF
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      1001-HOJOKBN-CHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-HOJO-MAX
               IF      SPA-GMN-HOJOKBN     =   SPA-GMN-HOJOKBNL
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-HOJOKBNL(IDX)
                                               TO  SPA-GMN-HOJOKBN-G
                   MOVE    1                   TO  FLG-CHK
               END-IF
           END-PERFORM
           IF     FLG-CHK              =   ZERO
               MOVE    "0046"          TO  SPA-ERRCD
               MOVE    11              TO  SPA-GMN-CUR
           END-IF
      *
           .
       1001-HOJOKBN-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    継続区分チェック処理
      ****************************************************************
       1001-CONTKBN-CHK-SEC     SECTION.
      *
           IF      SPA-GMN-CONTKBN     =   SPACE
               MOVE    SPACE           TO  SPA-GMN-CONTKBN-G
               GO      TO      1001-CONTKBN-CHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-CONT-MAX
               IF      SPA-GMN-CONTKBN     =   SPA-GMN-CONTKBNL
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-CONTKBNL(IDX)
                                               TO  SPA-GMN-CONTKBN-G
                   MOVE    1                   TO  FLG-CHK
               END-IF
           END-PERFORM
           IF     FLG-CHK              =   ZERO
               MOVE    "0047"          TO  SPA-ERRCD
               MOVE    12              TO  SPA-GMN-CUR
               GO      TO      1001-CONTKBN-CHK-EXT
           END-IF
      *    共済下船３月は継続なしとする
           IF     (SPA-GMN-HKNNUM      =   "031" OR  "032" OR
                                           "033" OR  "034"  )  AND
                  (SPA-GMN-HOJOKBN     =   "2"   OR  "B"  OR "E")
               IF      SPA-GMN-CONTKBN     NOT =   SPACE
                   MOVE    "1047"          TO  SPA-ERRCD
                   MOVE    12              TO  SPA-GMN-CUR
                   GO      TO      1001-CONTKBN-CHK-EXT
               END-IF
           END-IF
      *
      *    特別は日雇特例の時のみとする
           IF      SPA-GMN-CONTKBN     =   "3"
               IF      SPA-GMN-HKNNUM      =   "003"
                   CONTINUE
               ELSE
                   MOVE    "1048"          TO  SPA-ERRCD
                   MOVE    12              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       1001-CONTKBN-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    記号・番号チェック処理
      ****************************************************************
       1001-KIGO-NUM-CHK-SEC     SECTION.
      *
      *    記号全角チェック
           IF      SPA-GMN-KIGO    NOT =   SPACE
               MOVE    SPA-GMN-KIGO        TO  WRK-MAE-INPUT
               MOVE    40                  TO  WRK-MAX
               IF      SPA-GMN-KIGO    NOT =   SPA-OLD-KIGO
      *            変更ありは半角変換
                   PERFORM 890-HALF-ZENC-SEC
                   MOVE    WRK-ATO-INPUT       TO  SPA-GMN-KIGO
                   IF      FLG-ERR             =   1
                       MOVE    "0048"              TO  SPA-ERRCD
                       MOVE    13                  TO  SPA-GMN-CUR
                   END-IF
               ELSE
      *            変更なしは混在チェックのみ（半角ＯＫ）
                   PERFORM 890-HALF-ZEN-SEC
                   MOVE    WRK-ATO-INPUT       TO  SPA-GMN-KIGO
                   IF      FLG-ERR             =   1
                       MOVE    "0048"              TO  SPA-ERRCD
                       MOVE    13                  TO  SPA-GMN-CUR
                   END-IF
               END-IF
      *        半角の時、変更はＯＫとする
      *        IF      FLG-ERR             =   2
      *            IF      SPA-GMN-KIGO    NOT =   SPA-OLD-KIGO
      *                MOVE    "0048"              TO  SPA-ERRCD
      *                MOVE    13                  TO  SPA-GMN-CUR
      *            END-IF
      *****    END-IF
      *        後期高齢者は記号なしとする
               IF      SPA-GMN-HKNNUM      =   "039"
                   IF      SPA-GMN-KIGO    NOT =   SPA-MAE-KIGO
                       MOVE    ZERO            TO  SPA-KIGO-KEIFLG
                   END-IF
                   IF     (SPA-GMN-KIGO    NOT =   SPACE ) AND
                          (SPA-KIGO-KEIFLG     =   ZERO  )
                       MOVE    "K050"              TO  SPA-ERRCD
                       MOVE    13                  TO  SPA-GMN-CUR
                   END-IF
                   MOVE    1                   TO  SPA-KIGO-KEIFLG
                   MOVE    SPA-GMN-KIGO        TO  SPA-MAE-KIGO
               ELSE
      *
      *        空白チェック
               IF     (SPA-ERRCD           =   SPACE  )
                  AND (SPA-GMN-HKNNUM      >=  "001"  AND
                                           <=  "099"  )
                   IF      SPA-GMN-KIGO    NOT =   SPA-MAE-KIGO
                       MOVE    ZERO            TO  SPA-KIGO-KEIFLG
                   END-IF
                   IF      SPA-KIGO-KEIFLG     =   ZERO
                       IF      KANACHK-RC3     NOT =   ZERO
                           MOVE    "K048"              TO  SPA-ERRCD
                           MOVE    13                  TO  SPA-GMN-CUR
                       END-IF
                       MOVE    1                   TO  SPA-KIGO-KEIFLG
                   END-IF
                   MOVE    SPA-GMN-KIGO        TO  SPA-MAE-KIGO
               END-IF
      *
               END-IF
           END-IF
      *
      *    番号全角チェック
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-GMN-NUM     NOT =   SPACE )
               MOVE    SPA-GMN-NUM         TO  WRK-MAE-INPUT
               MOVE    40                  TO  WRK-MAX
               IF      SPA-GMN-NUM     NOT =   SPA-OLD-NUM
      *            変更ありは半角変換
                   PERFORM 890-HALF-ZENC-SEC
                   MOVE    WRK-ATO-INPUT       TO  SPA-GMN-NUM
                   IF      FLG-ERR             =   1
                       MOVE    "0049"              TO  SPA-ERRCD
                       MOVE    14                  TO  SPA-GMN-CUR
                   END-IF
               ELSE
      *            変更なしは混在チェックのみ（半角ＯＫ）
                   PERFORM 890-HALF-ZEN-SEC
                   MOVE    WRK-ATO-INPUT       TO  SPA-GMN-NUM
                   IF      FLG-ERR             =   1
                       MOVE    "0049"              TO  SPA-ERRCD
                       MOVE    14                  TO  SPA-GMN-CUR
                   END-IF
               END-IF
      *        半角の時、変更はＯＫとする
      *        IF      FLG-ERR             =   2
      *            IF      SPA-GMN-NUM     NOT =   SPA-OLD-NUM
      *                MOVE    "0049"              TO  SPA-ERRCD
      *                MOVE    14                  TO  SPA-GMN-CUR
      *            END-IF
      *        END-IF
      *        空白チェック
               IF     (SPA-ERRCD           =   SPACE  )
                  AND (SPA-GMN-HKNNUM      >=  "001"  AND
                                           <=  "099"  )
                   IF      SPA-GMN-NUM     NOT =   SPA-MAE-NUM
                       MOVE    ZERO            TO  SPA-NUM-KEIFLG
                   END-IF
                   IF      SPA-NUM-KEIFLG      =   ZERO
                       IF      KANACHK-RC3     NOT =   ZERO
                           MOVE    "K049"              TO  SPA-ERRCD
                           MOVE    14                  TO  SPA-GMN-CUR
                       END-IF
                       MOVE    1                   TO  SPA-NUM-KEIFLG
                   END-IF
                   MOVE    SPA-GMN-NUM         TO  SPA-MAE-NUM
               END-IF
           END-IF
      *
      *R02.4
      *    番号枝番　チェック
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-GMN-EDABAN NOT =   SPACE )
               PERFORM 10011-EDABAN-CHK-SEC
           END-IF
           .
       1001-KIGO-NUM-CHK-EXT.
           EXIT.
      *
      *R02.4
      ****************************************************************
      *    番号枝番チェック処理
      ****************************************************************
       10011-EDABAN-CHK-SEC     SECTION.
      *
      *    数値２桁
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-GMN-EDABAN     TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )
              OR  (SNUM-MINKBN     NOT =   SPACE )
              OR  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "1040"              TO  SPA-ERRCD
           ELSE
               MOVE    SNUM-OUTEDT(14:2)   TO  SPA-GMN-EDABAN
           END-IF
      *
      *    後期高齢者は入力不可
           IF     (SPA-ERRCD           =   SPACE  )
              AND (SPA-GMN-EDABAN NOT =   SPACE  )
               IF     (SPA-GMN-HKNNUM      =   "039"
                                           OR  "040" )
                   MOVE    "1041"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE  )
              AND (SPA-GMN-KIGO        =   SPACE  )
              AND (SPA-GMN-NUM         =   SPACE  )
              AND (SPA-GMN-EDABAN  NOT =   SPACE  )
      *        枝番のみはエラー
               MOVE    "1042"              TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    72                  TO  SPA-GMN-CUR
           END-IF
           .
       10011-EDABAN-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    資格取得日チェック処理
      ****************************************************************
       1001-SKKGETYMD-CHK-SEC     SECTION.
      *
           IF      SPA-GMN-SKKGETYMD   =   SPACE
               MOVE    SPACE               TO  SPA-NAI-SKKGETYMD
           ELSE
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN-SKKGETYMD   TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY         TO  SPA-GMN-SKKGETYMD
                   MOVE    SGDAY-SDAY          TO  SPA-NAI-SKKGETYMD
               ELSE
                   MOVE    "0050"              TO  SPA-ERRCD
                   MOVE    15                  TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
      *
       1001-SKKGETYMD-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    保険適用開始日チェック処理
      ****************************************************************
       1001-HTEKSTYMD-CHK-SEC       SECTION.
      *
      *    システム日付設定
           IF      SPA-GMN-HTEKSTYMD   =   SPACE
               IF    ((SPA-MCP-WIDGET      =   "P02_HTEKSTYMD") OR
                      (SPA-MCP-WIDGET      =   "P02_HTEKEDYMD"))  AND
                     ( SPA-GMN-HKNNUM  NOT =   SPACE    )
                   CONTINUE
               ELSE
                   IF     (SPA-HKNKOH-FLG      =   1    )  AND
                          (SPA-GMN-HKNNUM  NOT =   SPACE)
                       CONTINUE
                   ELSE
                       GO      TO      1001-HTEKSTYMD-CHK-EXT
                   END-IF
               END-IF
      *        システム日付を設定
               MOVE    SPA-SYSYMD          TO  SPA-NAI-HTEKSTYMD
               MOVE    SPA-SYSYMDWH        TO  SPA-GMN-HTEKSTYMD
      *ver.4.7 　すべてシステム日付
      *        後期高齢者
      *        IF      SPA-GMN-HKNNUM      =   "039"
      *                                    OR  "040"
      *            PERFORM 10011-TEKSTYMD-SET39-SEC
      *            IF      (FLG-KOH-CHK        =   1   )
      *                OR  (SPA-SYSYMD         <   "20080401" )
      *                OR  (SPA-BYMD-75-2      <=  "20080401" )
      *                ７５歳到達日がＨ２０．４以前の時
      *                MOVE    "20080401"      TO  SPA-NAI-HTEKSTYMD
      *                MOVE    "20080401"      TO  SPA-GMN-HTEKSTYMD
      *            END-IF
      ******   END-IF
           END-IF
      *
      *!   IF      SPA-GMN-HTEKSTYMD   =   SPACE
      *        MOVE    SPACE               TO  SPA-NAI-HTEKSTYMD
      *        GO      TO      1001-HTEKSTYMD-CHK-EXT
      *!   ELSE
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN-HTEKSTYMD   TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY         TO  SPA-GMN-HTEKSTYMD
                   MOVE    SGDAY-SDAY          TO  SPA-NAI-HTEKSTYMD
               ELSE
                   MOVE    "0051"              TO  SPA-ERRCD
                   MOVE    16                  TO  SPA-GMN-CUR
               END-IF
      *!   END-IF
      *    有効開始入力時に老人補助区分の再編集
           IF      (SPA-NAI-HTEKSTYMD  >=  "20021001" )  AND
                   (SPA-BYMD-70        >   ZERO       )   AND
                   (SPA-NAI-HTEKSTYMD  >=  SPA-BYMD-70)
               PERFORM 310110-HOJKBN-HENSYU-SEC
           END-IF
      *
      *    継続が特別の時、Ｈ１５．４以降であること
           IF      (SPA-NAI-HTEKSTYMD  NOT =   SPACE )  AND
                   (SPA-GMN-CONTKBN    =   "3"       )
               IF      SPA-NAI-HTEKSTYMD   <   "20030401"
                   MOVE    "1049"              TO  SPA-ERRCD
                   MOVE    16                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      *    後期高齢者の時、開始日が７５歳以上は警告
           IF      SPA-GMN-HKNNUM      =   "039"
                                       OR  "040"
               PERFORM 310110-HOJKBN-HENSYU-SEC
               PERFORM 10011-TEKSTYMD-SET39-SEC
               IF     (SPA-NAI-HTEKSTYMD   NOT =   SPACE )  AND
                      (SPA-NAI-HTEKSTYMD       <   SPA-BYMD-75-2)
                 AND  (FLG-KOH-CHK             =   ZERO    )
                   IF      SPA-ROU-KEIFLG      =   ZERO
                       MOVE    "K002"              TO  SPA-ERRCD
                       MOVE    16                  TO  SPA-GMN-CUR
                       MOVE    1                   TO  SPA-ROU-KEIFLG
                   END-IF
               END-IF
           ELSE
      *        主保険で
               IF     (SPA-NAI-PTHKN-SBTKBN    =   "1"   )  AND
                      (SPA-NAI-HTEKSTYMD   NOT =   SPACE )
                 AND  (SPA-BYMD-75-2           >   ZERO  )
                   IF     (SPA-NAI-HTEKSTYMD       >   "20080401" ) AND
                          (SPA-NAI-HTEKSTYMD       >=  SPA-BYMD-75-2)
      *            船員で本人以外
                       IF     (SPA-GMN-HKNNUM          =   "002" )
                          AND (SPA-GMN-HONKZKKBN       =   "1"   )
                          AND (SPA-GMN-HOJOKBN         >=  "A"  AND
                                                       <=  "I"   )
                           CONTINUE
                       ELSE
      *!!!
      ***                   MOVE    "0122"         TO  SPA-ERRCD
      *                保険開始日が７５歳以上は警告（非該当対応）
                           IF      SPA-ROU-KEIFLG      =   ZERO
                               MOVE    "K122"      TO  SPA-ERRCD
                               MOVE    16          TO  SPA-GMN-CUR
                               MOVE    1           TO  SPA-ROU-KEIFLG
                           END-IF
      *!!!
                       END-IF
                   END-IF
               END-IF
           END-IF
           .
       1001-HTEKSTYMD-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    後期高齢者保険適用開始了日設定処理
      ****************************************************************
       10011-TEKSTYMD-SET39-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-KOH-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-PTKOH-MAX )  OR
                              (FLG-KOH-CHK     =   1     )
               IF     (SPA-PTKOH-SAKJOKBN (IDX)    =   SPACE ) AND
                      (SPA-PTKOH-KOHNUM (IDX)      =   "027" ) AND
                      (SPA-PTKOH-TEKEDYMD (IDX)    =   "20080331")
                   MOVE    1                   TO  FLG-KOH-CHK
               END-IF
           END-PERFORM
           .
       10011-TEKSTYMD-SET39-EXT.
           EXIT.
      *
      ****************************************************************
      *    保険適用終了日チェック処理
      ****************************************************************
       1001-HTEKEDYMD-CHK-SEC     SECTION.
      *
           IF     (SPA-GMN-HTEKSTYMD   =   SPACE )  AND
                  (SPA-GMN-HTEKEDYMD   =   SPACE )
               GO      TO      1001-HTEKEDYMD-CHK-EXT
           END-IF
      *    空白で入力でない時、処理をしない
           IF     (SPA-HKNKOH-FLG      =   ZERO     )   AND
                  (SPA-GMN-HTEKEDYMD   =   SPACE    )   AND
                  (SPA-MCP-WIDGET  NOT =   "P02_HTEKEDYMD")
              GO      TO      1001-HTEKEDYMD-CHK-EXT
           END-IF
      *
           IF      SPA-GMN-HTEKEDYMD   =   SPACE
      *        継続の時、自動で発生する
               IF      SPA-GMN-CONTKBN     =   "1"  OR  "2"
                                                    OR  "3"
                   PERFORM 41010-KEIKBN-TEKEDYMD-SEC
                   GO      TO      1001-HTEKEDYMD-CHK-EXT
               END-IF
           END-IF
      *
           IF     (SPA-GMN-HTEKEDYMD   =   SPACE ) OR
                  (SPA-GMN-HTEKEDYMD(1:7)  =   "9999999")
               MOVE    "9999999  "         TO  SPA-GMN-HTEKEDYMD
               MOVE    ALL "9"             TO  SPA-NAI-HTEKEDYMD
      *        継続の時、エラーとする
               IF      SPA-GMN-CONTKBN     =   "1"  OR  "2"
                                                    OR  "3"
                   MOVE    "0053"              TO  SPA-ERRCD
                   MOVE    17                  TO  SPA-GMN-CUR
                   GO      TO      1001-HTEKEDYMD-CHK-EXT
               END-IF
           ELSE
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN-HTEKEDYMD   TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY         TO  SPA-GMN-HTEKEDYMD
                   MOVE    SGDAY-SDAY          TO  SPA-NAI-HTEKEDYMD
               ELSE
                   MOVE    "0052"              TO  SPA-ERRCD
                   MOVE    17                  TO  SPA-GMN-CUR
                   GO      TO      1001-HTEKEDYMD-CHK-EXT
               END-IF
           END-IF
      *
      *    継続・任継入力時の終了日チェック
      *          開始より(継)..５年、(任)..２年
           IF     (SPA-GMN-CONTKBN     =   "1"  OR  "2" )  AND
                  (SPA-NAI-HTEKSTYMD   NOT =   SPACE    )
               INITIALIZE                  WRK-DAY-AREA
               MOVE    SPA-NAI-HTEKSTYMD   TO  WRK-DAY-YMD
               IF      SPA-GMN-CONTKBN     =   "1"
                   MOVE    5               TO  WRK-DAY-ZOGEN
               ELSE
      *************MOVE    2               TO  WRK-DAY-ZOGEN
                   MOVE    5               TO  WRK-DAY-ZOGEN
               END-IF
               MOVE    "3"                 TO  WRK-DAY-PTN
               PERFORM 41010-DAY-ZOGEN-GET-SEC
               IF     (WRK-SYMD        NOT =   SPACE  )  AND
                      (SPA-NAI-HTEKEDYMD   >=  WRK-SYMD)
                   MOVE    "0053"          TO  SPA-ERRCD
                   MOVE    17              TO  SPA-GMN-CUR
                   GO      TO      1001-HTEKEDYMD-CHK-EXT
               END-IF
           END-IF
      *
      *    継続の時、Ｈ１５．３までとする
      *??  IF       SPA-GMN-CONTKBN    =   "1"
      *        IF     (SPA-GMN-HKNNUM      =   "002" )  AND
      *               (SPA-GMN-HOJOKBN     =   "2"   )
      *            CONTINUE
      *        ELSE
      *            船員以外
      *            IF      SPA-NAI-HTEKEDYMD   >=  "20030401"
      *                MOVE    "1050"              TO  SPA-ERRCD
      *                MOVE    17                  TO  SPA-GMN-CUR
      *            END-IF
      *        END-IF
      *****END-IF
      *    Ｈ１５．４から
           IF     (SPA-GMN-CONTKBN     =   "2"   OR  "3" )  AND
                  (SPA-NAI-HTEKSTYMD   >=  "20030401"    )
               IF      SPA-GMN-CONTKBN    =   "2"
                   MOVE    2                   TO  WRK-DAY-ZOGEN
                   MOVE    "3"                 TO  WRK-DAY-PTN
      *            任意継続で改正前に５５歳以上の場合、５年後とする
                   IF     (SPA-NAI-BIRTHDAY    <   "19480401" )  AND
                          (SPA-NAI-HTEKSTYMD   =   "20030401" )
                       MOVE    5                   TO  WRK-DAY-ZOGEN
                   END-IF
               ELSE
                   MOVE    6                   TO  WRK-DAY-ZOGEN
                   MOVE    "2"                 TO  WRK-DAY-PTN
               END-IF
               MOVE    SPA-NAI-HTEKSTYMD   TO  WRK-DAY-YMD
               PERFORM 41010-DAY-ZOGEN-GET-SEC
               IF     (WRK-SYMD        NOT =   SPACE  )  AND
                      (SPA-NAI-HTEKEDYMD   >=  WRK-SYMD)
                   MOVE    "0053"          TO  SPA-ERRCD
                   MOVE    17              TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
      *
       1001-HTEKEDYMD-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    被保険者名チェック処理
      ****************************************************************
       1001-HIHKNJANAME-CHK-SEC     SECTION.
      *
      *    被保険者名入力以外
           IF      SPA-MCP-WIDGET      =   "P02_HIHKNJANAME"
               MOVE    1               TO  SPA-NAI-HIHKNFLG
           END-IF
      *
      *    新規時
           IF      (SPA-NAI-HKNSTR         =   ZERO  ) OR
                   (SPA-PTHKN-SINKIKBN (SPA-NAI-HKNSTR)
                                           =   "1"    )
               MOVE    1                   TO  FLG-HONKZKKBN
           END-IF
      *
      *    新規の時、氏名または、世帯主を編集
           IF     (SPA-GMN-HKNNUM      NOT =   SPACE  )
           AND    (SPA-NAI-HIHKNFLG        =   ZERO   )
      *
               EVALUATE    SPA-GMN-HONKZKKBN
               WHEN    "1"
      *        本人の場合、氏名を編集する
                   IF     (SPA-GMN-HIHKNJANAME =   SPACE     )  OR
                         ((SPA-HEN-FLG         =   1         ) AND
                          (SPA-HZN-NAME        =   SPA-GMN-HIHKNJANAME))
                      OR  (FLG-HONKZKKBN          =   1     )
                       MOVE    SPA-GMN-NAME    TO  SPA-GMN-HIHKNJANAME
                   END-IF
               WHEN    "2"
      *            家族の場合、世帯主名か氏名
                   IF     (SPA-GMN-HIHKNJANAME    =   SPACE )  OR
                         ((SPA-GMN-HIHKNJANAME    =   SPA-GMN-NAME ) AND
                          (FLG-HONKZKKBN          =   1     ))
                       PERFORM 10011-HIHKNJANAME-HENSYU-SEC
                   END-IF
               END-EVALUATE
           END-IF
      *    記号全角チェック
           IF      SPA-GMN-HIHKNJANAME    NOT =   SPACE
               MOVE    SPA-GMN-HIHKNJANAME TO  WRK-MAE-INPUT
               MOVE    50                  TO  WRK-MAX
      *        拡張漢字登録
               PERFORM 891-HALF-ZEN2-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-HIHKNJANAME
               IF      FLG-ERR         NOT =   ZERO
                   MOVE    "0055"              TO  SPA-ERRCD
                   MOVE    18                  TO  SPA-GMN-CUR
                   GO      TO      1001-HIHKNJANAME-CHK-EXT
               END-IF
           END-IF
      *
      *    被保険者名入力で、家族の場合世帯主名を編集する
           IF     (SPA-GMN-HIHKNJANAME NOT =   SPACE     )  AND
                  (SPA-MCP-WIDGET          =   "P02_HIHKNJANAME") AND
                  (SPA-GMN-HONKZKKBN       =   "2"      )
               IF      (SPA-GMN-HKNNUM         =   "060" 
                                               OR  "067"
                                               OR  "068"
                                               OR  "069" )
                   OR  (SPA-NAI-SETAIKBN-FLG   NOT =   "1"  )
                   CONTINUE
               ELSE
                   MOVE    SPA-GMN-HIHKNJANAME     TO  SPA-GMN-SETAINUSI
               END-IF
           END-IF
           .
       1001-HIHKNJANAME-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    保険名家族編集処理
      ****************************************************************
       10011-HIHKNJANAME-HENSYU-SEC     SECTION.
      *
      *    姓の判定
      *    MOVE    ZERO                TO  IDX-S1
      *    PERFORM VARYING     IDX-S2  FROM    1   BY  2
      *            UNTIL       IDX-S2  >   50
      *        IF      SPA-GMN-NAME(IDX-S2:2)     =   "　"
      *            MOVE    50              TO  IDX-S2
      *        ELSE
      *            ADD     2               TO  IDX-S1
      *        END-IF
      *    END-PERFORM
           INITIALIZE                  ORCSSTRINGAREA
           MOVE    "search"            TO  ORCSSTR-COMMAND
           MOVE    SPA-GMN-NAME        TO  ORCSSTR-STRING1
           MOVE    "　"                TO  ORCSSTR-STRING2
           CALL    "orcsstring"        USING
                                       ORCSSTRINGAREA
           IF      ORCSSTR-NUM1        =   ZERO
               MOVE    SPA-GMN-NAME        TO  WRK-SEINAME
           ELSE
               MOVE    ORCSSTR-NUM1        TO  IDX-S1
               MOVE    ORCSSTR-NUM2        TO  IDX-S2
               INITIALIZE                  ORCSSTRINGAREA
               MOVE    "substr"            TO  ORCSSTR-COMMAND
               MOVE    SPA-GMN-NAME        TO  ORCSSTR-STRING1
               MOVE    1                   TO  ORCSSTR-NUM1
               COMPUTE ORCSSTR-NUM2        =   IDX-S1
                                           -   1
               CALL    "orcsstring"        USING
                                           ORCSSTRINGAREA
               MOVE    ORCSSTR-STRING2     TO  WRK-SEINAME
           END-IF
      *
      *    国保、退職国保の場合
           IF     (SPA-GMN-HKNNUM      =   "060"
                                       OR  "067"
                                       OR  "068"
                                       OR  "069"  )
      *           標準設定の時のみ家族でも姓名編集
             AND  (SPA-NAI-HIHKNJAKBN-FLG  =   "0"  )
               IF      SPA-GMN-HIHKNJANAME     =   SPACE
                   MOVE    SPA-GMN-NAME    TO  SPA-GMN-HIHKNJANAME
               END-IF
               IF     (SPA-GMN-SETAINUSI   =   SPACE )  OR
                      (SPA-GMN-SETAINUSI   =   SPA-GMN-NAME)
                   IF      SPA-NAI-SETAIKBN-FLG   =   "1"
      *            世帯主名が空白で氏名と同じ時、姓だけ
      ************      MOVE    SPA-GMN-NAME(1:IDX-S1)
                        MOVE    WRK-SEINAME
                                                 TO  SPA-GMN-SETAINUSI
                   END-IF
               END-IF
           ELSE
      *        国保以外
               IF     (SPA-GMN-SETAINUSI   NOT =   SPACE)   AND
                      (SPA-GMN-SETAINUSI   NOT =   SPA-GMN-NAME)
                   MOVE    SPA-GMN-SETAINUSI   TO  SPA-GMN-HIHKNJANAME
               ELSE
      *****        MOVE    SPA-GMN-NAME(1:IDX-S1)
                   MOVE    WRK-SEINAME
                                                 TO  SPA-GMN-HIHKNJANAME
      *            世帯主名が空白で氏名と同じ時、姓だけ
                   IF      SPA-NAI-SETAIKBN-FLG   =   "1"
      *****            MOVE    SPA-GMN-NAME(1:IDX-S1)
                       MOVE    WRK-SEINAME
                                                 TO  SPA-GMN-SETAINUSI
                   END-IF
               END-IF
           END-IF
           .
      *
       10011-HIHKNJANAME-HENSYU-EXT.
           EXIT.
      *
      ****************************************************************
      *    保険確認日チェック処理
      ****************************************************************
       1001-HKAKUNINYMD-CHK-SEC     SECTION.
      *
           IF     (SPA-GMN-HKNNUM      =   SPACE )  AND
                  (SPA-GMN-HTEKSTYMD   =   SPACE )  AND
                  (SPA-GMN-HKAKUNINYMD =   SPACE )
               GO      TO      1001-HKAKUNINYMD-CHK-EXT
           END-IF
      *
           IF      SPA-GMN-HKAKUNINYMD =   SPACE
               MOVE    SPA-SYSYMDWH        TO  SPA-GMN-HKAKUNINYMD
               MOVE    SPA-SYSYMD          TO  SPA-NAI-HKAKUNINYMD
           ELSE
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN-HKAKUNINYMD TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY         TO  SPA-GMN-HKAKUNINYMD
                   MOVE    SGDAY-SDAY          TO  SPA-NAI-HKAKUNINYMD
               ELSE
                   MOVE    "0056"              TO  SPA-ERRCD
                   MOVE    19                  TO  SPA-GMN-CUR
                   GO      TO      1001-HKAKUNINYMD-CHK-EXT
               END-IF
           END-IF
      *
      *    更新の時、変更時のみチェック
           IF     (SPA-NAI-HKAKUNINYMD     >   SPA-SYSYMD )
             AND  (SPA-NAI-HKAKUNINYMD NOT =   SPA-MAE-HKAKUNINYMD)
               IF      SPA-HKN-KAKUINFLG       =   ZERO
                   MOVE    "0057"              TO  SPA-ERRCD
                   MOVE    19                  TO  SPA-GMN-CUR
                   MOVE    1                   TO  SPA-HKN-KAKUINFLG
               END-IF
           END-IF
      *
           .
      *
       1001-HKAKUNINYMD-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    関連チェック処理
      ****************************************************************
       200-KANREN-CHK-SEC     SECTION.
      *    保険入力なし
           IF      (SPA-GMN-HKNJANUM   =   SPACE  )  AND
                   (SPA-GMN-HKNNUM     =   SPACE  )  AND
                   (SPA-GMN-HONKZKKBN  =   SPACE  )  AND
                   (SPA-GMN-HOJOKBN    =   SPACE  )  AND
                   (SPA-GMN-CONTKBN    =   SPACE  )  AND
                   (SPA-GMN-KIGO       =   SPACE  )  AND
                   (SPA-GMN-NUM        =   SPACE  )  AND
                   (SPA-GMN-EDABAN     =   SPACE  )  AND
                   (SPA-GMN-SKKGETYMD  =   SPACE  )  AND
                   (SPA-GMN-HTEKSTYMD  =   SPACE  )  AND
                   (SPA-GMN-HTEKEDYMD  =   SPACE  )  AND
                   (SPA-GMN-HIHKNJANAME    =   SPACE  )  AND
                   (SPA-GMN-HKAKUNINYMD    =   SPACE  )
               GO      TO      200-KANREN-CHK-EXT
           END-IF
      *
      *    労災保険の時、以下処理なし
           IF      SPA-GMN-HKNNUM      =   "971"
                                       OR  "973"
               GO      TO      200-KANREN-CHK-EXT
           END-IF
      *    保険入力なし
           IF      (SPA-GMN-HKNNUM     NOT =   SPACE  )  AND
                   (SPA-GMN-HONKZKKBN  NOT =   SPACE  )  AND
                   (SPA-GMN-HTEKSTYMD  NOT =   SPACE  )  AND
                   (SPA-GMN-HTEKEDYMD  NOT =   SPACE  )  AND
                   (SPA-GMN-HKAKUNINYMD  NOT =   SPACE)
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
      *
      *    すべての項目が入力済みの時、以下のチェックを行う
           IF      FLG-KAKUTEI         =   ZERO
               GO      TO      200-KANREN-CHK-EXT
           END-IF
      *
      *    適応期間範囲チェック
      *****IF      SPA-NAI-HTEKSTYMD   >=  SPA-NAI-HTEKEDYMD
           IF      SPA-NAI-HTEKSTYMD   >   SPA-NAI-HTEKEDYMD
               MOVE    "0058"          TO  SPA-ERRCD
               MOVE    17              TO  SPA-GMN-CUR
           END-IF
      *    保険を確認
           IF      SPA-ERRCD           =   SPACE
               PERFORM 2002-HNKKIKAN-CHK-SEC
           END-IF
      *    本人家族区分整合性チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 2003-HONKAKBN-SEICHK-SEC
           END-IF
      *    補助区分整合性チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 2003-HOJOKBN-SEICHK-SEC
           END-IF
      *    補助区分変更チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 20039-HOJOKBN-HENCHK-SEC
           END-IF
      *    保険期間重複チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 2004-KANREN-HKN-CHECK-SEC
           END-IF
      *
      *!!  保険者異動日チェック
      *    IF     (SPA-ERRCD           =   SPACE )  AND
      *           (SPA-PIDCD           =   SPACE )
      *        IF      SPA-HKNJA-IDOYMD    =   SPACE
      *            CONTINUE
      *        ELSE
      *            PERFORM 2005-IDOYMD-CHECK-SEC
      *        END-IF
      *!!  END-IF
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
           IF      SPA-GMN-HKNNUM      =   SPACE
               MOVE    "0059"          TO  SPA-ERRCD
               MOVE    9               TO  SPA-GMN-CUR
           END-IF
      *
           IF      SPA-GMN-HONKZKKBN   =   SPACE
               IF      SPA-ERRCD       =   SPACE
                   MOVE    "0045"          TO  SPA-ERRCD
                   MOVE    10              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *    適用開始日
           IF      SPA-GMN-HTEKSTYMD   =   SPACE
               IF      SPA-ERRCD       =   SPACE
                   MOVE    "0062"          TO  SPA-ERRCD
                   MOVE    16              TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       2001-HISSU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険確認チェック処理
      *****************************************************************
       2002-HNKKIKAN-CHK-SEC           SECTION.
      *
           MOVE    SPACE               TO  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    SPA-GMN-HKNNUM      TO  HKN-HKNNUM
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 921-HKNMST-NEXT-SEC
           ELSE
               INITIALIZE              HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
           MOVE    SPACE               TO  WRK-MAE-TEKEDYMD
           MOVE    SPACE               TO  WRK-ATO-TEKSTYMD
           PERFORM
               UNTIL   FLG-HKNNUM      =   1
      *        支払区分のないものは対象外とする
             IF      HKN-PAYKBN        =   SPACE
               CONTINUE
             ELSE
               IF     (SPA-NAI-HTEKSTYMD   >=  HKN-TEKSTYMD )  AND
                      (SPA-NAI-HTEKEDYMD   <=  HKN-TEKEDYMD )
                   MOVE    1               TO  FLG-CHK
                   MOVE    1               TO  FLG-HKNNUM
               ELSE
                   IF      SPA-NAI-HTEKSTYMD   >=  HKN-TEKSTYMD
                       MOVE    HKN-TEKEDYMD    TO  WRK-MAE-TEKEDYMD
                   ELSE
                       MOVE    HKN-TEKSTYMD    TO  WRK-ATO-TEKSTYMD
                   END-IF
               END-IF
             END-IF
      *
               IF      FLG-HKNNUM          =   ZERO
                   MOVE    "tbl_hknnum"        TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 921-HKNMST-NEXT-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    対象があった時
           IF      FLG-CHK             =   1
               GO      TO      2002-HNKKIKAN-CHK-EXT
           END-IF
      *    保険の終了時
           IF     (WRK-MAE-TEKEDYMD    NOT =   SPACE )  AND
                  (WRK-ATO-TEKSTYMD    =   SPACE )
               MOVE    WRK-MAE-TEKEDYMD    TO  SPA-END-TEKEDYMD
               MOVE    WRK-ATO-TEKSTYMD    TO  SPA-STR-TEKSTYMD
               MOVE    "0201"              TO  SPA-PIDCD
               MOVE    16                  TO  SPA-GMN-CUR
               MOVE    SPA-NAI-HKNSTR      TO  SPA-HKN-IDX1
               GO      TO      2002-HNKKIKAN-CHK-EXT
           END-IF
      *    Ｈ１５．４改正対応
      *    保険の期間変更時
           IF     (WRK-MAE-TEKEDYMD    =   "20030331" )  AND
                  (WRK-ATO-TEKSTYMD    =   "20030401" )
               IF      SPA-GMN-CONTKBN     =   SPACE
                   MOVE    WRK-MAE-TEKEDYMD    TO  SPA-END-TEKEDYMD
                   MOVE    WRK-ATO-TEKSTYMD    TO  SPA-STR-TEKSTYMD
                   MOVE    "0202"              TO  SPA-PIDCD
                   MOVE    16                  TO  SPA-GMN-CUR
                   MOVE    SPA-NAI-HKNSTR      TO  SPA-HKN-IDX1
                   GO      TO      2002-HNKKIKAN-CHK-EXT
               END-IF
      *        継続はＨ１５．３．３１までとする
               IF      SPA-GMN-CONTKBN     =   "1"
                   MOVE    WRK-MAE-TEKEDYMD    TO  SPA-END-TEKEDYMD
                   MOVE    SPACE               TO  SPA-STR-TEKSTYMD
                   MOVE    "0203"              TO  SPA-PIDCD
                   MOVE    16                  TO  SPA-GMN-CUR
                   MOVE    SPA-NAI-HKNSTR      TO  SPA-HKN-IDX1
                   GO      TO      2002-HNKKIKAN-CHK-EXT
               END-IF
      *        任継
               IF      SPA-GMN-CONTKBN     =   "2"
                   MOVE    WRK-MAE-TEKEDYMD    TO  SPA-END-TEKEDYMD
                   MOVE    WRK-ATO-TEKSTYMD    TO  SPA-STR-TEKSTYMD
                   MOVE    "0204"              TO  SPA-PIDCD
                   MOVE    16                  TO  SPA-GMN-CUR
                   MOVE    SPA-NAI-HKNSTR      TO  SPA-HKN-IDX1
                   GO      TO      2002-HNKKIKAN-CHK-EXT
               END-IF
           END-IF
           IF      FLG-CHK             =   ZERO
      *        マスタの範囲エラー
               MOVE    "0063"          TO  SPA-ERRCD
               MOVE    16              TO  SPA-GMN-CUR
           END-IF
           .
       2002-HNKKIKAN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    補助区分整合性チェック処理
      *****************************************************************
       2003-HOJOKBN-SEICHK-SEC         SECTION.
      *
      *!!!
      *Ｈ２２．９　高齢者非該当判定
           PERFORM 310111-HIGAITOU-CHK-SEC
      *!!!
      *    国保・自費の時必須
      *    前の補助区分が空白、老人公費ありはそのままとする
      *T   IF      (SPA-GMN-HKNNUM     =   "060"   )   AND
           IF      (SPA-NAI-HTEKSTYMD  >=  SPA-BYMD-70)
               IF     (SPA-MAE-HOJOKBN     =   SPACE )
               AND    (SPA-NAI-HKNSTR  NOT =   ZERO  )
               AND    (SPA-PTHKN-SINKIKBN (SPA-NAI-HKNSTR) = SPACE)
                   MOVE    ZERO            TO  FLG-CHK
                   PERFORM VARYING     IDX     FROM    1   BY  1
                           UNTIL      (IDX     >   SPA-PTKOH-MAX )  OR
                                      (FLG-CHK     =   1         )
                       IF    (SPA-PTKOH-SAKJOKBN (IDX) =   SPACE )  AND
                             (SPA-PTKOH-KOHNUM   (IDX) =   "027" )
                           MOVE    1              TO  FLG-CHK
                       END-IF
                   END-PERFORM
                   IF      FLG-CHK         =   1
      *            高齢者非該当以外
                     OR   (FLG-RJNCHK      =   1    )
                       GO      TO      2003-HOJOKBN-SEICHK-EXT
                   END-IF
               END-IF
           END-IF
      *
           IF      (SPA-GMN-HKNNUM         =   "060"
                                           OR  "068" )   OR
                   (SPA-NAI-PTHKN-SBTKBN   =   "8"  )
               IF      SPA-GMN-HOJOKBN     =   SPACE
                   MOVE    "0060"          TO  SPA-ERRCD
                   MOVE    11              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *!!  治験
      *    IF      (SPA-GMN-HKNNUM         >=  "900"
      *                                    OR  "919" )   AND
      *            (SPA-NAI-PTHKN-SBTKBN   =   "9"  )
      *        IF      SPA-GMN-HOJOKBN     =   SPACE
      *            MOVE    "0060"          TO  SPA-ERRCD
      *            MOVE    11              TO  SPA-GMN-CUR
      *        END-IF
      *!!! END-IF
      *
      *    Ｈ１４．１０から
      *    ７０歳以上は必須
           IF     (SPA-NAI-HTEKSTYMD   >=  "20021001" )  AND
                  (SPA-NAI-HTEKSTYMD   >=  SPA-BYMD-70)  AND
                  (SPA-NAI-BIRTHDAY    >=  "19321001" )  AND
                  (SPA-BYMD-70         >   ZERO      )   AND
                  (SPA-GMN-HKNNUM(1:1) NOT =   "9"    )
      *!!
             AND  (FLG-RJNCHK         =   0  )
               IF      SPA-GMN-HOJOKBN     =   SPACE
                   MOVE    "0128"          TO  SPA-ERRCD
                   MOVE    11              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 20031-HOJOKBN-YMD-SEC
           END-IF
      *
           .
       2003-HOJOKBN-SEICHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    補助区分と開始日の整合性チェック処理
      *****************************************************************
       20031-HOJOKBN-YMD-SEC         SECTION.
      *
      *    国保の前期高齢者の補助区分判定
           IF    ( SPA-GMN-HKNNUM      =   "060"  OR  "068"  )  AND
                 ( SPA-NAI-BIRTHDAY    >   "19320930"        )  AND
                 ((SPA-NAI-HTEKSTYMD   >=  "20021001"   ) AND
                  (SPA-NAI-HTEKSTYMD   >=  SPA-BYMD-70  )    )
               IF      (SPA-GMN-HOJOKBN    =   "3"  OR  "6")
      *            Ｈ１８．１０ 改正 チェックなし
                   IF      SPA-NAI-HTEKSTYMD   >=  "20061001"
                       CONTINUE
                   ELSE
                       MOVE    "1066"          TO  SPA-ERRCD
                       MOVE    11              TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *    船員家族
           IF     (SPA-GMN-HONKZKKBN   =   "2"       ) AND
                  (SPA-GMN-HKNNUM      =   "002"     )
      *        老人
      *        Ｈ１８．１０ 改正
               IF      SPA-NAI-HTEKSTYMD   >=  "20061001"
                   IF      (SPA-GMN-HOJO-MAX    =   8      )
                       IF     (SPA-GMN-HOJOKBN     =   "7"
                                                   OR  "9")
                           CONTINUE
                       ELSE
                           MOVE    "1066"          TO  SPA-ERRCD
                           MOVE    11              TO  SPA-GMN-CUR
                       END-IF
      *H26.5
      *                H26.5.1以降のS19.4.2生以降の高齢者負担割合
                       IF      FLG-RJN-02          =   1
                           IF     (SPA-GMN-HOJOKBN     =   "7"
                                                       OR  "8")
                               MOVE    SPACE           TO  SPA-ERRCD
                               MOVE    ZERO            TO  SPA-GMN-CUR
                           ELSE
                               MOVE    "1066"          TO  SPA-ERRCD
                               MOVE    11              TO  SPA-GMN-CUR
                           END-IF
                       END-IF
                   END-IF
               ELSE
                   IF      (SPA-NAI-HTEKSTYMD  NOT =   SPACE )
                      AND  (SPA-GMN-HOJO-MAX       =   8     )
                       IF     (SPA-GMN-HOJOKBN     =   "8"
                                                   OR  "9")
                           CONTINUE
                       ELSE
                           MOVE    "1066"          TO  SPA-ERRCD
                           MOVE    11              TO  SPA-GMN-CUR
                       END-IF
                   END-IF
               END-IF
           END-IF
      *    共済家族
           IF     (SPA-GMN-HONKZKKBN   =   "2"       ) AND
                  (SPA-GMN-HKNNUM      =   "031" OR
                                           "032" OR
                                           "033" OR
                                           "034"     )
      *        老人
      *        Ｈ１８．１０ 改正
               IF      SPA-NAI-HTEKSTYMD   >=  "20061001"
                   IF      (SPA-GMN-HOJO-MAX    =   4      )  AND
                           (SPA-GMN-HOJOKBN     =   "B"   OR
                                                    "H"   )
                       MOVE    "1066"          TO  SPA-ERRCD
                       MOVE    11              TO  SPA-GMN-CUR
                   END-IF
      *H26.5
      *            H26.5.1以降のS19.4.2生以降の高齢者負担割合
                   IF      FLG-RJN-02          =   1
                       IF      (SPA-GMN-HOJO-MAX    =   4      )  AND
                               (SPA-GMN-HOJOKBN     =   "E"   OR
                                                        "H"   )
                           MOVE    "1066"          TO  SPA-ERRCD
                           MOVE    11              TO  SPA-GMN-CUR
                       ELSE
                           MOVE    SPACE           TO  SPA-ERRCD
                           MOVE    ZERO            TO  SPA-GMN-CUR
                       END-IF
                   END-IF
               ELSE
                   IF      (SPA-GMN-HOJO-MAX    =   4      )  AND
                           (SPA-GMN-HOJOKBN     =   "B"   OR
                                                    "E"   )
                       MOVE    "1066"          TO  SPA-ERRCD
                       MOVE    11              TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *!!
      *    高齢者非該当はエラーなし
           IF      (FLG-RJNCHK         =   1   )  AND
                   (SPA-GMN-HOJOKBN    =   SPACE)
                MOVE    SPACE              TO  SPA-ERRCD
           END-IF
           .
       20031-HOJOKBN-YMD-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    補助区分の変更処理
      *****************************************************************
       20039-HOJOKBN-HENCHK-SEC         SECTION.
      *
           IF     (SPA-NAI-HKNSTR      >   ZERO )  AND
                  (SPA-PTHKN-SINKIKBN (SPA-NAI-HKNSTR)
                                       =   SPACE )
               IF      (SPA-GMN-HOJOKBN    NOT =   SPA-OLD-HOJOKBN)
                  AND  (SPA-HOJOKBN-KEIFLG     =   ZERO      )
                   MOVE    "K130"              TO  SPA-ERRCD
                   MOVE    11                  TO  SPA-GMN-CUR
                   MOVE    1                   TO  SPA-HOJOKBN-KEIFLG
               END-IF
           END-IF
           .
       20039-HOJOKBN-HENCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    本人家族区分整合性チェック処理
      *****************************************************************
       2003-HONKAKBN-SEICHK-SEC         SECTION.
      *
      *    本人のみ入力可能チェック
           IF     ( SPA-NAI-PTHKN-SBTKBN   =   "4" OR "8" OR "9")  OR
                  ( SPA-GMN-HKNNUM         =   "007"       )
               IF      SPA-GMN-HONKZKKBN   NOT =   "1"
                   MOVE    "0066"          TO  SPA-ERRCD
                   MOVE    10              TO  SPA-GMN-CUR
               END-IF
      *****ELSE
      *
      ******** １５歳未満の時、家族とする
      *        IF      SPA-NAI-NENREI-YY   <=  15
      *            IF      SPA-GMN-HONKZKKBN   =   "1"
      *                MOVE    "0067"          TO  SPA-ERRCD
      *                MOVE    10              TO  SPA-GMN-CUR
      *            END-IF
      *********END-IF
           END-IF
      *
      *    船員入力可能チェック
           IF      (SPA-GMN-HKNNUM         =   "002"  )  AND
                   (SPA-GMN-HOJOKBN    NOT =   SPACE  )
               IF     (SPA-GMN-HONKZKKBN   NOT =   "1" )  AND
                      (SPA-GMN-HOJO-MAX        =    3  )
                   MOVE    "0066"          TO  SPA-ERRCD
                   MOVE    10              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
      *    共済入力可能チェック
           IF      (SPA-GMN-HKNNUM         =   "031" OR
                                               "032" OR
                                               "033" OR
                                               "034" )  AND
                   (SPA-GMN-HOJOKBN    NOT =   SPACE  )
               IF     (SPA-GMN-HONKZKKBN   NOT =   "1" )  AND
                      (SPA-GMN-HOJO-MAX        =    1  )
                   MOVE    "0066"          TO  SPA-ERRCD
                   MOVE    10              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
      *    変更チェック
           IF     (SPA-NAI-HKNSTR      >   ZERO )  AND
                  (SPA-PTHKN-SINKIKBN (SPA-NAI-HKNSTR)
                                       =   SPACE ) AND
                  (SPA-ERRCD           =   SPACE )
               IF      (SPA-GMN-HONKZKKBN  NOT =   SPA-OLD-HONKZKKBN)
                  AND  (SPA-HONKZK-KEIFLG  =   ZERO      )
                   MOVE    "K129"              TO  SPA-ERRCD
                   MOVE    10                  TO  SPA-GMN-CUR
                   MOVE    1                   TO  SPA-HONKZK-KEIFLG
               END-IF
           END-IF
      *
           .
       2003-HOJOKBN-SEICHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険期間エラーチェック処理
      *****************************************************************
       2004-KANREN-HKN-CHECK-SEC    SECTION.
      *
           MOVE    ZERO                TO  WRK-IDX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX             >   SPA-PTHKN-MAX )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               MOVE    ZERO            TO  FLG-CHK
      *        現在表示分、削除分はチェックしない
               IF     (IDX             =   SPA-NAI-HKNSTR ) OR
                      (SPA-PTHKN-SAKJOKBN(IDX)     =   "1" )
                   CONTINUE
               ELSE
      *            範囲の重複
                   IF      (SPA-PTHKN-TEKEDYMD (IDX)
                                           <   SPA-NAI-HTEKSTYMD)  OR
                           (SPA-PTHKN-TEKSTYMD (IDX)
                                           >   SPA-NAI-HTEKEDYMD)
                       CONTINUE
                   ELSE
                       MOVE    1            TO  FLG-CHK
                   END-IF
               END-IF
      *        労災は対象外
               IF     (SPA-GMN-HKNNUM          =   "971"  OR  "973")  OR
                      (SPA-PTHKN-HKNNUM (IDX)  =   "971"  OR  "973")
                   MOVE    ZERO            TO  FLG-CHK
               END-IF
      *        公害保険は対象外
               IF     (SPA-GMN-HKNNUM          =   "975"  )  OR
                      (SPA-PTHKN-HKNNUM (IDX)  =   "975"  )
                   MOVE    ZERO            TO  FLG-CHK
               END-IF
      *
               IF      FLG-CHK         =   1
                   PERFORM 20041-KIKAN-CHK1-SEC
               END-IF
           END-PERFORM
      *
      *    新規入力分で、開始日付と終了日付が重複している時、自動変換
      *H30.7
           MOVE    ZERO                TO  SPA-WRK-IDX
           IF    ( WRK-IDX             >   ZERO     )  AND
                 ((SPA-NAI-HKNSTR      =   ZERO      ) OR
                  (SPA-PTHKN-SINKIKBN (SPA-NAI-HKNSTR)
                                       =   "1"      ) )
               IF      (SPA-PTHKN-TEKEDYMD (WRK-IDX)
                                           >=  SPA-NAI-HTEKSTYMD)  AND
                       (SPA-PTHKN-TEKSTYMD (WRK-IDX)
                                           <   SPA-NAI-HTEKSTYMD)
                   MOVE    SPA-PTHKN-HKNNUM-MEI(WRK-IDX)
                                           TO  SPA-PIDMSG
                   MOVE    "0105"          TO  SPA-PIDCD
                   MOVE    SPACE           TO  SPA-ERRCD
      *H30.7
                   MOVE    WRK-IDX         TO  SPA-WRK-IDX
               END-IF
           END-IF
      *
            .
       2004-KANREN-HKN-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    重複期間チェック処理
      *****************************************************************
       20041-KIKAN-CHK1-SEC       SECTION.
      *
      *    自費・治験
           IF     (SPA-NAI-PTHKN-SBTKBN    =   "8" OR "9") OR
                  (SPA-PTHKN-HKN-SBTKBN (IDX)
                                           =   "8" OR "9" )
               IF     (SPA-NAI-PTHKN-SBTKBN    =   "8" OR "9" )
                 AND  (SPA-GMN-HKNNUM          =   SPA-PTHKN-HKNNUM
                                                                (IDX))
                   MOVE    "0064"              TO  SPA-ERRCD
                   MOVE    IDX                 TO  WRK-IDX
                   MOVE    16                  TO  SPA-GMN-CUR
               END-IF
               GO      TO      20041-KIKAN-CHK1-EXT
           END-IF
      *
      *    自費・治験以外
      *    継続（同時発生あり）　重複可
           IF     (SPA-GMN-CONTKBN         =   "1" ) OR
                  (SPA-PTHKN-CONTKBN (IDX) =   "1" )
               GO      TO      20041-KIKAN-CHK1-EXT
           END-IF
      *!!!!
      *H30.9
      *    自官は重複可とする
           IF      (SPA-GMN-HKNNUM         =   "007")
               OR  (SPA-PTHKN-HKNNUM(IDX)  =   "007")
      *        重複エラー
               IF     (SPA-PTHKN-HKNNUM  (IDX) =   SPA-GMN-HKNNUM)
                   MOVE    "0064"          TO  SPA-ERRCD
                   MOVE    IDX             TO  WRK-IDX
                   MOVE    16              TO  SPA-GMN-CUR
               END-IF
      *        自官・国公以外重複エラー
               IF     (SPA-PTHKN-HKNNUM  (IDX) =   "007")
                AND   (SPA-GMN-HKNNUM      NOT =   "031")
                   MOVE    "0064"          TO  SPA-ERRCD
                   MOVE    IDX             TO  WRK-IDX
                   MOVE    16              TO  SPA-GMN-CUR
               END-IF
      *        自官・国公以外重複エラー
               IF     (SPA-PTHKN-HKNNUM  (IDX) NOT =   "031")
                AND   (SPA-GMN-HKNNUM      =   "007")
                   MOVE    "0064"          TO  SPA-ERRCD
                   MOVE    IDX             TO  WRK-IDX
                   MOVE    16              TO  SPA-GMN-CUR
               END-IF
      *        どちらかが家族はエラー
               IF      SPA-ERRCD           =   SPACE
                   IF     (SPA-PTHKN-HONKZKKBN (IDX) =   "2" )
                       OR (SPA-GMN-HONKZKKBN         =   "2" )
                       MOVE    "0064"          TO  SPA-ERRCD
                       MOVE    IDX             TO  WRK-IDX
                       MOVE    16              TO  SPA-GMN-CUR
                   END-IF
               END-IF
               GO      TO      20041-KIKAN-CHK1-EXT
           END-IF
      *!!!!
      *    船員
           IF      (SPA-GMN-HKNNUM         =   "002") AND
                   (SPA-GMN-HOJOKBN    NOT =   SPACE )
      *        補助区分が同じ時、重複エラー
               IF     (SPA-PTHKN-HKNNUM  (IDX) =   "002") AND
                      (SPA-PTHKN-HOJOKBN (IDX) =   SPA-GMN-HOJOKBN)
                   MOVE    "0064"          TO  SPA-ERRCD
                   MOVE    IDX             TO  WRK-IDX
                   MOVE    16              TO  SPA-GMN-CUR
               END-IF
      *H26.7
      *        家族の時、補助区分判定なし
               IF     (SPA-PTHKN-HKNNUM  (IDX) =   "002") AND
                     ((SPA-GMN-HONKZKKBN       =   "2"  ) AND
                      (SPA-PTHKN-HONKZKKBN (IDX)
                                               =   SPA-GMN-HONKZKKBN))
                   MOVE    "0064"          TO  SPA-ERRCD
                   MOVE    IDX             TO  WRK-IDX
                   MOVE    16              TO  SPA-GMN-CUR
               END-IF
      *H30.7
      *        前期高齢者補助区分重複対象
               IF     (SPA-GMN-HOJOKBN     =   SPACE
                                           OR  "7"
                                           OR  "8"
                                           OR  "9" )
                 AND  (SPA-ERRCD           =   SPACE)
                   CONTINUE
               ELSE
                   GO      TO      20041-KIKAN-CHK1-EXT
               END-IF
           END-IF
      *    共済の下船３月
           IF      (SPA-GMN-HKNNUM         =   "031"  OR
                                               "032"  OR
                                               "033"  OR
                                               "034" )  AND
      *???         (SPA-GMN-HOJOKBN    NOT =   SPACE )
                   (SPA-GMN-HOJOKBN    NOT =   SPACE
                                           AND "7"
                                           AND "8"
                                           AND "9" )
      *        補助区分が同じ時、重複エラー
               IF     (SPA-PTHKN-HKNNUM  (IDX) =   SPA-GMN-HKNNUM) AND
                      (SPA-PTHKN-HOJOKBN (IDX) =   SPA-GMN-HOJOKBN)
                   MOVE    "0064"          TO  SPA-ERRCD
                   MOVE    IDX             TO  WRK-IDX
                   MOVE    16              TO  SPA-GMN-CUR
               END-IF
               GO      TO      20041-KIKAN-CHK1-EXT
           END-IF
      *    船員、共済の下船３月
           IF      (SPA-PTHKN-HKNNUM(IDX)  =   "002"
                                           OR  "031"
                                           OR  "032"
                                           OR  "033"
                                           OR  "034" )
      *        前期高齢者の補助区分対象外
               IF     (SPA-PTHKN-HOJOKBN(IDX)  =   SPACE
                                               OR  "7"
                                               OR  "8"
                                               OR  "9"  )
                   CONTINUE
               ELSE
                   GO      TO      20041-KIKAN-CHK1-EXT
               END-IF
           END-IF
      *
      *    以外の時、重複とする
           MOVE    "0064"              TO  SPA-ERRCD
           MOVE    IDX                 TO  WRK-IDX
           MOVE    16                  TO  SPA-GMN-CUR
           .
       20041-KIKAN-CHK1-EXT.
           EXIT.
      *****************************************************************
      *    継続適用終了日　処理
      *****************************************************************
       41010-KEIKBN-TEKEDYMD-SEC       SECTION.
      *
           INITIALIZE                      WRK-DAY-AREA
           MOVE    SPA-NAI-HTEKSTYMD   TO  WRK-DAY-YMD
           EVALUATE    SPA-GMN-CONTKBN
               WHEN    "1"
                   MOVE    5               TO  WRK-DAY-ZOGEN
                   MOVE    "3"             TO  WRK-DAY-PTN
               WHEN    "2"
                   MOVE    2               TO  WRK-DAY-ZOGEN
      ************ MOVE    5               TO  WRK-DAY-ZOGEN
                   MOVE    "3"             TO  WRK-DAY-PTN
               WHEN    "3"
                   MOVE    6               TO  WRK-DAY-ZOGEN
                   MOVE    "2"             TO  WRK-DAY-PTN
           END-EVALUATE
           PERFORM 41010-DAY-ZOGEN-GET-SEC
           IF      WRK-SYMD            =   SPACE
               MOVE    16              TO  SPA-GMN-CUR
               GO      TO              41010-KEIKBN-TEKEDYMD-EXT
           END-IF
           INITIALIZE                  WRK-DAY-AREA
           MOVE    WRK-SYMD            TO  WRK-DAY-YMD
           MOVE    -1                  TO  WRK-DAY-ZOGEN
           MOVE    "1"                 TO  WRK-DAY-PTN
           PERFORM 41010-DAY-ZOGEN-GET-SEC
           IF      WRK-SYMD            =   SPACE
               MOVE    16              TO  SPA-GMN-CUR
               GO      TO              41010-KEIKBN-TEKEDYMD-EXT
           END-IF
           MOVE    WRK-SYMD            TO  SPA-NAI-HTEKEDYMD
           PERFORM 410112-SEIYMDHEN-SEC
           MOVE    WRK-HENYMD          TO  SPA-GMN-HTEKEDYMD
           IF      WRK-HENYMD          =   SPACE
               MOVE    "0024"          TO  SPA-ERRCD
               MOVE    16              TO  SPA-GMN-CUR
           END-IF
      *
           .
       41010-KEIKBN-TEKEDYMD-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者異動日チェック処理
      *****************************************************************
       2005-IDOYMD-CHECK-SEC         SECTION.
      *
           IF      SPA-HKNJA-KEIFLG    =   "1"
               GO      TO      2005-IDOYMD-CHECK-EXT
           END-IF
      *
           IF      (SPA-HKNJA-IDOYMD   <=  SPA-NAI-HTEKSTYMD )  OR
                   (SPA-HKNJA-IDOYMD   <   SPA-NAI-HTEKEDYMD )
               CONTINUE
           ELSE
               MOVE    SPACE           TO  SPA-HKNJA-KEIFLG
               GO      TO      2005-IDOYMD-CHECK-EXT
           END-IF
      *
           MOVE    "K003"              TO  SPA-ERRCD
           MOVE    "1"                 TO  SPA-HKNJA-KEIFLG
           MOVE    8                   TO  SPA-GMN-CUR
      *
           .
       2005-IDOYMD-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険重複期間処理
      *****************************************************************
       300-HKNKIKAN-SYORI-SEC         SECTION.
      *
           MOVE    ZERO                TO  WRK-IDX
      *H30.7
           MOVE    SPA-WRK-IDX         TO  WRK-IDX
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX             >   SPA-PTHKN-MAX)  OR
                              (WRK-IDX     NOT =   ZERO )
               MOVE    ZERO            TO  FLG-CHK
      *        現在表示分、削除分はチェックしない
               IF     (IDX             =   SPA-NAI-HKNSTR ) OR
                      (SPA-PTHKN-SAKJOKBN(IDX)     =   "1" )
                   CONTINUE
               ELSE
      *            範囲の重複
                   IF      (SPA-PTHKN-TEKEDYMD (IDX)
                                           <   SPA-NAI-HTEKSTYMD)  OR
                           (SPA-PTHKN-TEKSTYMD (IDX)
                                           >   SPA-NAI-HTEKEDYMD)
                       CONTINUE
                   ELSE
                       MOVE    1            TO  FLG-CHK
                   END-IF
               END-IF
      *        労災は対象外
               IF     (SPA-GMN-HKNNUM          =   "971" OR "973" )  OR
                      (SPA-PTHKN-HKNNUM (IDX)  =   "971" OR "973" )
                   MOVE    ZERO            TO  FLG-CHK
               END-IF
      *        公害保険は対象外
               IF     (SPA-GMN-HKNNUM          =   "975"  )  OR
                      (SPA-PTHKN-HKNNUM (IDX)  =   "975"  )
                   MOVE    ZERO            TO  FLG-CHK
               END-IF
      *H30.7
      *!!!!    未使用となるが、対応のみ行う
               IF      FLG-CHK         =   1
      *            重複期間チェック
                   PERFORM 20041-KIKAN-CHK1-SEC
      *            エラー　クリア
                   MOVE    SPACE           TO  SPA-ERRCD
               END-IF
      *
      *******  IF      FLG-CHK         =   1
      *            自費・治験
      *            IF     (SPA-NAI-PTHKN-SBTKBN    =   "8" OR "9") OR
      *                   (SPA-PTHKN-HKN-SBTKBN (IDX)
      *                                            =   "8" OR "9")
      *                IF     (SPA-NAI-PTHKN-SBTKBN    =   "8" OR "9" )
      *                  AND  (SPA-GMN-HKNNUM      =   SPA-PTHKN-HKNNUM
      *                                                     (IDX))
      *                    MOVE    IDX             TO  WRK-IDX
      *                END-IF
      *            ELSE
      *            自費・治験以外
      *                継続（同時発生あり）　重複可
      *                IF     (SPA-GMN-CONTKBN         =   "1" ) OR
      *                       (SPA-PTHKN-CONTKBN (IDX) =   "1" )
      *                    CONTINUE
      *                ELSE
      *                    船員
      *                    IF      (SPA-GMN-HKNNUM       =   "002") AND
      *                            (SPA-GMN-HOJOKBN  NOT =   SPACE
      *                                                  AND  "7"
      *                                                  AND  "8" 
      *                                                  AND  "9" )
      *                        IF     (SPA-PTHKN-HKNNUM(IDX)
      *                                                  =   "002")
      *                          AND  (SPA-PTHKN-HOJOKBN (IDX)
      *                                            =   SPA-GMN-HOJOKBN)
      *                            MOVE    IDX             TO  WRK-IDX
      *                        END-IF
      *                    ELSE
      ****                     IF    ((SPA-PTHKN-HKNNUM(IDX)
      *                                                =   "002") AND
      *                               (SPA-PTHKN-HOJOKBN (IDX)
      ****                                         NOT =   SPACE )) OR
      *                        IF    ((SPA-PTHKN-HKNNUM(IDX)
      *                                                =   "031" OR
      *                                                    "032" OR 
      *                                                    "033" OR 
      *                                                    "034" )  AND
      *                               (SPA-PTHKN-HOJOKBN (IDX)
      *                                                =   "2" OR "B"
      *                                                        OR "E" ))
      *                            CONTINUE
      *                        ELSE
      *                            MOVE    IDX             TO  WRK-IDX
      *                        END-IF
      *                    END-IF
      *                END-IF
      *            END-IF
      *********END-IF
           END-PERFORM
      *
      *    現在表示分の開始日付−１日
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    SPA-NAI-HTEKSTYMD   TO  LNK-DAY6-KIJUN
           MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    -1                  TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING
                                           STS-AREA-DAY
                                           LNK-DAY6-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY6-KEISAN     TO  WRK-SYMD
           ELSE
               GO      TO      300-HKNKIKAN-SYORI-EXT
           END-IF
           PERFORM 410112-SEIYMDHEN-SEC
      *    期間変更
           IF      (SPA-PTHKN-TEKEDYMD (WRK-IDX)
                                       >=  SPA-NAI-HTEKSTYMD)  AND
                   (SPA-PTHKN-TEKSTYMD (WRK-IDX)
                                       <   SPA-NAI-HTEKSTYMD)
               MOVE    WRK-SYMD        TO  SPA-PTHKN-TEKEDYMD (WRK-IDX)
               MOVE    WRK-HENYMD      TO  SPA-PTHKN-TEKEDYMDW(WRK-IDX)
               MOVE    18              TO  SPA-GMN-CUR
           END-IF
      *H30.7
           MOVE    ZERO                TO  SPA-WRK-IDX
      *
           .
       300-HKNKIKAN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    老人２割切替え処理（Ｈ１８．１０改正）
      *****************************************************************
       330-RJNHKNKIKAN-SYORI-SEC         SECTION.
      *
      *    置換え前の保険を終了させる
           MOVE    SPA-HKN-IDX1        TO  IDX
           MOVE    SPA-PTHKN-TEKEDYMD(IDX)
                                       TO  WRK-TEKEDYMD
           MOVE    SPA-PTHKN-TEKEDYMDW(IDX)
                                       TO  WRK-TEKEDYMDW
      *    Ｈ１８．９．３０まで有効
           MOVE    "20060930"          TO  WRK-SYMD
           PERFORM 410112-SEIYMDHEN-SEC
           MOVE    WRK-SYMD            TO  SPA-PTHKN-TEKEDYMD(IDX)
           MOVE    WRK-HENYMD          TO  SPA-PTHKN-TEKEDYMDW(IDX)
           MOVE    SPA-SYSYMD          TO  SPA-PTHKN-KAKUNINYMD (IDX)
           MOVE    SPA-SYSYMDWH        TO  SPA-PTHKN-KAKUNINYMDW(IDX)
      *
      *    保険をＨ１８．１０．１からとする
           INITIALIZE                  SPA-GMN-HKN-AREA
           MOVE    ZERO                TO  SPA-NAI-HKNSTR
           MOVE    SPA-HKN-IDX1        TO  IDX
      *
           MOVE    SPA-PTHKN-HKNNUM (IDX)  TO  SPA-GMN-HKNNUM
           MOVE    ZERO                TO  SPA-NAI-HKNID
      *
           MOVE    SPA-PTHKN-HKNJANUM(IDX) TO  SPA-GMN-HKNJANUM
           MOVE    SPA-PTHKN-HONKZKKBN (IDX)
                                       TO  SPA-GMN-HONKZKKBN
           MOVE    SPA-PTHKN-CONTKBN (IDX)
                                       TO  SPA-GMN-CONTKBN
           MOVE    SPA-PTHKN-HIHKNJANAME (IDX)
                                       TO  SPA-GMN-HIHKNJANAME
           MOVE    SPA-PTHKN-KIGO    (IDX) TO  SPA-GMN-KIGO
           MOVE    SPA-PTHKN-NUM     (IDX) TO  SPA-GMN-NUM
      *
           MOVE    SPA-PTHKN-EDABAN (IDX) TO  SPA-GMN-EDABAN
      *
           MOVE    SPA-PTHKN-SKKGETYMD  (IDX)
                                       TO  SPA-NAI-SKKGETYMD
           MOVE    "20061001"          TO  SPA-NAI-HTEKSTYMD
           MOVE    WRK-TEKEDYMD        TO  SPA-NAI-HTEKEDYMD
           MOVE    SPA-SYSYMD          TO  SPA-NAI-HKAKUNINYMD
      *
           MOVE    SPA-PTHKN-SKKGETYMDW (IDX)
                                       TO  SPA-GMN-SKKGETYMD
           MOVE    "20061001"          TO  WRK-SYMD
           PERFORM 410112-SEIYMDHEN-SEC
           MOVE    WRK-HENYMD          TO  SPA-GMN-HTEKSTYMD
           MOVE    WRK-TEKEDYMDW       TO  SPA-GMN-HTEKEDYMD
           MOVE    SPA-SYSYMDWH        TO  SPA-GMN-HKAKUNINYMD
      *
      *    保険情報
           PERFORM VARYING     IDH     FROM    1   BY  1
                   UNTIL       IDH     >   SPA-GMN-HKNMAX
               IF      SPA-GMN-HKNNUML (IDH)   =   SPA-GMN-HKNNUM
                   MOVE    SPA-GMN-HKNNUMLST (IDH)
                                               TO  SPA-GMN-HKNNUM-G
      *            識別
                   MOVE    SPA-NAI-HKNSBTKBN (IDH)
                                               TO  SPA-NAI-PTHKN-SBTKBN
               END-IF
           END-PERFORM
      *    船員の時、補助区分を３割に設定する
           IF      SPA-GMN-HKNNUM      =   "002"
               EVALUATE    SPA-PTHKN-HOJOKBN (IDX)
                   WHEN    "D"
                       MOVE    "G"             TO  SPA-GMN-HOJOKBN
                   WHEN    "E"
                       MOVE    "H"             TO  SPA-GMN-HOJOKBN
                   WHEN    "F"
                       MOVE    "I"             TO  SPA-GMN-HOJOKBN
                   WHEN    SPACE
                       MOVE    "7"             TO  SPA-GMN-HOJOKBN
               END-EVALUATE
           END-IF
      *    共済の下船３月
           IF     (SPA-GMN-HKNNUM      =   "031"  OR
                                           "032"  OR
                                           "033"  OR
                                           "034" )
               EVALUATE    SPA-PTHKN-HOJOKBN (IDX)
                   WHEN    "E"
                       MOVE    "H"             TO  SPA-GMN-HOJOKBN
                   WHEN    SPACE
                       MOVE    "7"             TO  SPA-GMN-HOJOKBN
               END-EVALUATE
           END-IF
      *    国保
           IF      SPA-GMN-HKNNUM      =   "060"  OR
                                           "068"
               EVALUATE    SPA-PTHKN-HOJOKBN (IDX)
                   WHEN    "2"
                       MOVE    "3"             TO  SPA-GMN-HOJOKBN
               END-EVALUATE
           END-IF
      *    その他
           IF      SPA-GMN-HOJOKBN     =   SPACE
               EVALUATE    SPA-PTHKN-HOJOKBN (IDX)
                   WHEN    "8"
                       MOVE    "7"             TO  SPA-GMN-HOJOKBN
               END-EVALUATE
           END-IF
      *
      *    補助・本人家族区分等
           PERFORM 31011-HKN-HEN-SEC
      *
      *    保険者番号　検索チェック
           INITIALIZE                  HKNJAINF-REC
           MOVE    SPA-HOSPNUM          TO  HKNJA-HOSPNUM
           MOVE    SPA-GMN-HKNJANUM    TO  HKNJA-HKNJANUM
      *
           MOVE    HKNJAINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknjainf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-HKNJAINF-READ-SEC
           ELSE
               INITIALIZE                  HKNJAINF-REC
               MOVE    1               TO  FLG-HKNJAINF
           END-IF
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-HKNJAINF        =   ZERO
               MOVE    HKNJA-HKNJANAME     TO  SPA-HKNJA-HKNJANAME
               MOVE    HKNJA-ADRS          TO  SPA-HKNJA-ADRS
               MOVE    HKNJA-BANTI         TO  SPA-HKNJA-BANTI
               MOVE    HKNJA-TEL           TO  SPA-HKNJA-TEL
           ELSE
               MOVE    SPACE               TO  SPA-HKNJA-HKNJANAME
               MOVE    SPACE               TO  SPA-HKNJA-ADRS
               MOVE    SPACE               TO  SPA-HKNJA-BANTI
               MOVE    SPACE               TO  SPA-HKNJA-TEL
           END-IF
      *
           .
       330-RJNHKNKIKAN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人保険自動切り替え処理
      *****************************************************************
       310-RJNHENKAN-SYORI-SEC         SECTION.
      *
      *    置換え前の保険を終了させる
           MOVE    SPA-HKN-IDX1        TO  IDX
           MOVE    SPA-PTHKN-TEKEDYMD(IDX)
                                       TO  WRK-TEKEDYMD
           MOVE    SPA-PTHKN-TEKEDYMDW(IDX)
                                       TO  WRK-TEKEDYMDW
      *
      *    ７０歳を迎える月−１日
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    SPA-BYMD-70         TO  LNK-DAY6-KIJUN
           MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    -1                  TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING
                                           STS-AREA-DAY
                                           LNK-DAY6-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY6-KEISAN     TO  WRK-SYMD
           ELSE
               MOVE    "20020930"          TO  WRK-SYMD
           END-IF
           PERFORM 410112-SEIYMDHEN-SEC
           MOVE    WRK-SYMD            TO  SPA-PTHKN-TEKEDYMD(IDX)
           MOVE    WRK-HENYMD          TO  SPA-PTHKN-TEKEDYMDW(IDX)
           MOVE    SPA-SYSYMD          TO  SPA-PTHKN-KAKUNINYMD (IDX)
           MOVE    SPA-SYSYMDWH        TO  SPA-PTHKN-KAKUNINYMDW(IDX)
      *
      *    保険をＨ１４．１０．１からとする
           INITIALIZE                  SPA-GMN-HKN-AREA
           MOVE    ZERO                TO  SPA-NAI-HKNSTR
           MOVE    SPA-HKN-IDX1        TO  IDX
      *
      *
           MOVE    SPA-PTHKN-HKNNUM (IDX)  TO  SPA-GMN-HKNNUM
           MOVE    ZERO                TO  SPA-NAI-HKNID
      *
           MOVE    SPA-PTHKN-HKNJANUM(IDX) TO  SPA-GMN-HKNJANUM
           MOVE    SPA-PTHKN-HONKZKKBN (IDX)
                                       TO  SPA-GMN-HONKZKKBN
           MOVE    SPA-PTHKN-CONTKBN (IDX)
                                       TO  SPA-GMN-CONTKBN
           MOVE    SPA-PTHKN-HIHKNJANAME (IDX)
                                       TO  SPA-GMN-HIHKNJANAME
           MOVE    SPA-PTHKN-KIGO    (IDX) TO  SPA-GMN-KIGO
           MOVE    SPA-PTHKN-NUM     (IDX) TO  SPA-GMN-NUM
      *R02.4
           MOVE    SPA-PTHKN-EDABAN (IDX)  TO  SPA-GMN-EDABAN
      *
           MOVE    SPA-PTHKN-SKKGETYMD  (IDX)
                                       TO  SPA-NAI-SKKGETYMD
           MOVE    SPA-BYMD-70         TO  SPA-NAI-HTEKSTYMD
           MOVE    WRK-TEKEDYMD        TO  SPA-NAI-HTEKEDYMD
      *****MOVE    ALL "9"             TO  SPA-NAI-HTEKEDYMD
           MOVE    SPA-SYSYMD          TO  SPA-NAI-HKAKUNINYMD
      *
           MOVE    SPA-PTHKN-SKKGETYMDW (IDX)
                                       TO  SPA-GMN-SKKGETYMD
           MOVE    SPA-BYMD-70         TO  WRK-SYMD
           PERFORM 410112-SEIYMDHEN-SEC
           MOVE    WRK-HENYMD          TO  SPA-GMN-HTEKSTYMD
      *****MOVE    "9999999"           TO  SPA-GMN-HTEKEDYMD
           MOVE    WRK-TEKEDYMDW       TO  SPA-GMN-HTEKEDYMD
           MOVE    SPA-SYSYMDWH        TO  SPA-GMN-HKAKUNINYMD
      *
      *    保険情報
           PERFORM VARYING     IDH     FROM    1   BY  1
                   UNTIL       IDH     >   SPA-GMN-HKNMAX
               IF      SPA-GMN-HKNNUML (IDH)   =   SPA-GMN-HKNNUM
                   MOVE    SPA-GMN-HKNNUMLST (IDH)
                                               TO  SPA-GMN-HKNNUM-G
      *            識別
                   MOVE    SPA-NAI-HKNSBTKBN (IDH)
                                               TO  SPA-NAI-PTHKN-SBTKBN
               END-IF
           END-PERFORM
      *H26.5
      *    老人2割を初期とする
           IF      (SPA-NAI-BIRTHDAY       >=  "19440402")
               AND (SPA-NAI-HTEKSTYMD      >=  "20140501")
               MOVE    1                   TO  FLG-RJN-02
           ELSE
               MOVE    ZERO                TO  FLG-RJN-02
           END-IF
      *
      *
      *    船員の時、補助区分を設定する
           IF      SPA-GMN-HKNNUM      =   "002"
               EVALUATE    SPA-PTHKN-HOJOKBN (IDX)
                   WHEN    "1"
                       MOVE    "A"             TO  SPA-GMN-HOJOKBN
                       IF      FLG-RJN-02      =   1
                           MOVE    "D"             TO  SPA-GMN-HOJOKBN
                       END-IF
                   WHEN    "2"
                       MOVE    "B"             TO  SPA-GMN-HOJOKBN
                       IF      FLG-RJN-02      =   1
                           MOVE    "E"             TO  SPA-GMN-HOJOKBN
                       END-IF
                   WHEN    "3"
                       MOVE    "C"             TO  SPA-GMN-HOJOKBN
                       IF      FLG-RJN-02      =   1
                           MOVE    "F"             TO  SPA-GMN-HOJOKBN
                       END-IF
                   WHEN    SPACE
                       MOVE    "9"             TO  SPA-GMN-HOJOKBN
                       IF      FLG-RJN-02      =   1
                           MOVE    "8"             TO  SPA-GMN-HOJOKBN
                       END-IF
               END-EVALUATE
           END-IF
      *    共済の下船３月
           IF     (SPA-GMN-HKNNUM      =   "031"  OR
                                           "032"  OR
                                           "033"  OR
                                           "034" )
               EVALUATE    SPA-PTHKN-HOJOKBN (IDX)
                   WHEN    "2"
                       MOVE    "B"             TO  SPA-GMN-HOJOKBN
                       IF      FLG-RJN-02      =   1
                           MOVE    "E"             TO  SPA-GMN-HOJOKBN
                       END-IF
                   WHEN    SPACE
                       MOVE    "9"             TO  SPA-GMN-HOJOKBN
                       IF      FLG-RJN-02      =   1
                           MOVE    "8"             TO  SPA-GMN-HOJOKBN
                       END-IF
               END-EVALUATE
           END-IF
      *    補助・本人家族区分等
           PERFORM 31011-HKN-HEN-SEC
      *
      *    保険者番号　検索チェック
           INITIALIZE                  HKNJAINF-REC
           MOVE    SPA-HOSPNUM         TO  HKNJA-HOSPNUM
           MOVE    SPA-GMN-HKNJANUM    TO  HKNJA-HKNJANUM
      *
           MOVE    HKNJAINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknjainf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-HKNJAINF-READ-SEC
           ELSE
               INITIALIZE                  HKNJAINF-REC
               MOVE    1               TO  FLG-HKNJAINF
           END-IF
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-HKNJAINF        =   ZERO
               MOVE    HKNJA-HKNJANAME     TO  SPA-HKNJA-HKNJANAME
               MOVE    HKNJA-ADRS          TO  SPA-HKNJA-ADRS
               MOVE    HKNJA-BANTI         TO  SPA-HKNJA-BANTI
               MOVE    HKNJA-TEL           TO  SPA-HKNJA-TEL
           ELSE
               MOVE    SPACE               TO  SPA-HKNJA-HKNJANAME
               MOVE    SPACE               TO  SPA-HKNJA-ADRS
               MOVE    SPACE               TO  SPA-HKNJA-BANTI
               MOVE    SPACE               TO  SPA-HKNJA-TEL
           END-IF
      *
           .
       310-RJNHENKAN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険自動切り替え処理
      *****************************************************************
       320-HKNCHANGE-SYORI-SEC         SECTION.
      *
      *    置換え前の保険を終了させる
           IF      SPA-HKN-IDX1        =   ZERO
               MOVE    1                   TO  IDX
           ELSE
               MOVE    SPA-HKN-IDX1        TO  IDX
           END-IF
      *
           MOVE    SPA-END-TEKEDYMD    TO  SPA-PTHKN-TEKEDYMD(IDX)
           MOVE    SPA-END-TEKEDYMD    TO  WRK-SYMD
           PERFORM 410112-SEIYMDHEN-SEC
           MOVE    WRK-HENYMD          TO  SPA-PTHKN-TEKEDYMDW(IDX)
           MOVE    SPA-SYSYMD          TO  SPA-PTHKN-KAKUNINYMD (IDX)
           MOVE    SPA-SYSYMDWH        TO  SPA-PTHKN-KAKUNINYMDW(IDX)
      *    終了日変更のみ画面編集
           IF     (SPA-STR-TEKSTYMD    =   SPACE  ) OR
                  (SPA-PIDCD           =   "0201" OR
                                           "0203" )
               MOVE    SPA-PTHKN-TEKEDYMDW(IDX)
                                           TO  SPA-GMN-HTEKEDYMD
               MOVE    SPA-PTHKN-TEKEDYMD (IDX)
                                           TO  SPA-NAI-HTEKEDYMD
               MOVE    SPA-SYSYMDWH        TO  SPA-GMN-HKAKUNINYMD
               MOVE    SPA-SYSYMDW         TO  SPA-NAI-HKAKUNINYMD
               GO      TO      320-HKNCHANGE-SYORI-EXT
           END-IF
      *
      *    新規で保険を追加する
           MOVE    ZERO                TO  SPA-NAI-HKNSTR
           MOVE    ZERO                TO  SPA-NAI-HKNID
           MOVE    SPACE               TO  SPA-NAI-SAKJOKBN
           INITIALIZE                  SPA-MAE-HKN-AREA
      *
      *    開始日
           MOVE    SPA-STR-TEKSTYMD    TO  SPA-NAI-HTEKSTYMD
           MOVE    SPA-STR-TEKSTYMD    TO  WRK-SYMD
           PERFORM 410112-SEIYMDHEN-SEC
           MOVE    WRK-HENYMD          TO  SPA-GMN-HTEKSTYMD
      *    確認日
           MOVE    SPA-SYSYMDWH        TO  SPA-GMN-HKAKUNINYMD
           MOVE    SPA-SYSYMD          TO  SPA-NAI-HKAKUNINYMD
      *    補助区分
           PERFORM 310110-HOJKBN-HENSYU-SEC
      *    終了日変更なし
           EVALUATE    SPA-GMN-CONTKBN
               WHEN    SPACE
                   CONTINUE
               WHEN    "2"
                   CONTINUE
           END-EVALUATE
      *
           .
       320-HKNCHANGE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    半角空白を全角空白へ変換処理
      *****************************************************************
       890-HALF-ZEN-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           MOVE    WRK-MAX             TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               MOVE    1                   TO  FLG-ERR
               MOVE    KANACHK-MAE-INPUT       TO  WRK-ATO-INPUT
               IF      KANACHK-SYUBETU     =   1
                   MOVE    2                   TO  FLG-ERR
               END-IF
           ELSE
               MOVE    KANACHK-OUT-INPUT   TO  WRK-ATO-INPUT
           END-IF
      *
           .
       890-HALF-ZEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    半角を全角へ変換処理
      *****************************************************************
       890-HALF-ZENC-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           MOVE    WRK-MAX             TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-MAE-INPUT
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               MOVE    1                   TO  FLG-ERR
               MOVE    KANACHK-MAE-INPUT   TO  WRK-ATO-INPUT
           ELSE
               MOVE    KANACHK-OUT-INPUT   TO  WRK-ATO-INPUT
           END-IF
      *
           .
       890-HALF-ZENC-EXT.
           EXIT.
      *****************************************************************
      *    半角空白を全角空白へ変換処理
      *****************************************************************
       891-HALF-ZEN2-SEC         SECTION.
      *
           IF     SPA-ENCODING         =   2
      *        拡張漢字対応
               PERFORM 8910-HALF-ZENC2-SEC
           ELSE
      *        拡張漢字不可
               PERFORM 890-HALF-ZEN-SEC
           END-IF
      *
           .
       891-HALF-ZEN2-EXT.
           EXIT.
      *****************************************************************
      *    半角空白を全角空白へ変換処理（外字あり）
      *****************************************************************
       8910-HALF-ZENC2-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
      *
           INITIALIZE                  ORCSKANACHK2AREA
           MOVE    "1"                 TO  KANACHK2-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK2-MAE-INPUT
           MOVE    WRK-MAX             TO  KANACHK2-MAX-CNT
           CALL    "ORCSKANACHK2"      USING
                                       ORCSKANACHK2AREA
           IF     (KANACHK2-RC     NOT =   ZERO  )  OR
                  (KANACHK2-SYUBETU    =   1     )
               MOVE    1                   TO  FLG-ERR
               MOVE    KANACHK2-MAE-INPUT  TO  WRK-ATO-INPUT
               IF      KANACHK2-SYUBETU        =   1
                   MOVE    2                   TO  FLG-ERR
               END-IF
           ELSE
               MOVE    KANACHK2-OUT-INPUT   TO  WRK-ATO-INPUT
           END-IF
      *
           .
       8910-HALF-ZENC2-EXT.
           EXIT.
      *****************************************************************
      *    全角空白チェック処理
      *****************************************************************
       891-KUHAKU-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
           MOVE    ZERO                TO  WRK-CNT
           INSPECT    WRK-MAE-INPUT    TALLYING   WRK-CNT
                                       FOR   ALL  "　"
      *
           IF      WRK-CNT             >   ZERO
               MOVE    1                   TO  FLG-ERR
           END-IF
           .
       891-KUHAKU-CHK-EXT.
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
      *    日付取得　処理
      *****************************************************************
       41010-DAY-ZOGEN-GET-SEC      SECTION.
      *
           MOVE    SPACE            TO  STS-AREA-DAY
           INITIALIZE                   STS-AREA-DAY
           INITIALIZE                   LNK-DAY6-AREA
           MOVE    "61"             TO  LNK-DAY6-IRAI
           MOVE    WRK-DAY-YMD      TO  LNK-DAY6-KIJUN
           MOVE    WRK-DAY-PTN      TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-DAY-ZOGEN    TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"    USING   STS-AREA-DAY
                                        LNK-DAY6-AREA
           IF  STS-DAY-RC1          =   ZERO
               MOVE    LNK-DAY6-KEISAN  TO  WRK-SYMD
      *H24.3
      *        開始日が２月２９日で年加算は翌日とする
               IF     (WRK-DAY-YMD(5:4)    =   "0229" )
                 AND  (WRK-DAY-PTN         =   "3"    )
                 AND  (WRK-DAY-ZOGEN       >   ZERO   )
                 AND  (WRK-SMMDD           =   "0228" )
                   MOVE    "0301"          TO  WRK-SMMDD
               END-IF
           ELSE
               MOVE    SPACE            TO  WRK-SYMD
               MOVE    "0069"           TO  SPA-ERRCD
           END-IF
      *
           .
       41010-DAY-ZOGEN-GET-EXT.
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
               MOVE    ZERO                TO  FLG-HKNNUM
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1                   TO  FLG-HKNNUM
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
