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
       PROGRAM-ID.                 ORCSC50201404.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 診療行為チェック、算定（手術）
      *                      Ｈ２６年４月改正対応
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     MCC-多々納   02/04/16  労災用追加
      * 01.00.02     MCC-多々納   02/06/12  時間外特例追加
      * 01.00.04     MCC-多々納   02/07/09  労災加算の入力を可能とする
      * 01.00.05     NACL-多々納  02/10/03  労災加算なしの追加
      * 01.00.06     NACL-多々納  02/11/22  労災の機能回復指導加算の修正
      * 01.00.07     NACL-多々納  03/06/06  フィルム追加
      * 01.00.07     NACL-多々納  03/06/06  人工心肺不使用加算の計算修正
      * 01.00.08     NACL-多々納  04/03/16  Ｈ１６年４月改正追加
      * 01.00.09     NACL-多々納  05/12/14  MONFUNC 対応
      * 03.05.00     NACL-多々納  07/04/XX  グループ診療対応
      * 04.02.00     NACL-多々納  08/03/XX  Ｈ２０年４月改正追加
      * 04.02.00     NACL-多々納  08/06/24  通則加算を通則加算対象の所定点数としない
      * 04.05.00     NACL-多々納  10/03/XX  Ｈ２２年４月改正追加
      * 04.06.00     NACL-多々納  10/07/01  残量廃棄自動算定対応
      * 04.08.00     NACL-多々納  13/05/XX  労災レセ電用点数登録対応
      * 04.08.00     NACL-多々納  13/06/XX  自己血輸血６歳未満対応
      * 04.07.00     NACL-多々納  14/03/XX  Ｈ２６年４月改正追加
      * 04.07.00     NACL-多々納  15/03/XX  Ｈ２７年４月改正追加
      *****************************************************************
      *
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
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
           03  FLG-ERR             PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-ARIOK           PIC 9(01).
           03  FLG-MADOKU          PIC 9(01).
           03  FLG-SEIBETU         PIC 9(01).
           03  FLG-ADD             PIC 9(01).
      *    施設基準不適合
           03  FLG-KIJUNN          PIC 9(01).
      *    手技料計算フラグ
           03  FLG-SYUGI           PIC 9(01).
      *    労災加算あり
           03  FLG-ROU-ARI         PIC 9(01).
           03  FLG-ROUSAI          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDXS                PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-SA              PIC 9(04).
           03  IDX-ZS              PIC 9(04).
           03  IDX-ZS2             PIC 9(04).
           03  IDX-T               PIC 9(04).
           03  IDX-SANSO           PIC 9(04).
      *
           03  IDX-Y               PIC 9(04).
           03  IDX-Y2              PIC 9(02).
      *
           03  IDX-MAX             PIC 9(04).
           03  HZN-IDX             PIC 9(04).
           03  IDX-Z2              PIC 9(04).
      *
           03  IDX-SYU             PIC 9(04).
           03  IDX-SYU2            PIC 9(04).
           03  IDX-NEN             PIC 9(04).
           03  IDX-TUS             PIC 9(04).
      *
           03  IDX-KSN             PIC 9(04).
      *    施設基準用
           03  IDS                 PIC 9(04).
      *
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-MOJI            PIC X(01). 
           03  WRK-SRYCD           PIC X(09). 
           03  WRK-SRYCD1          PIC X(09). 
           03  WRK-SRYCD2          PIC X(09). 
           03  WRK-SRYCD3          PIC X(09). 
           03  WRK-TIMEFLG         PIC 9(01). 
           03  WRK-RSI-SRYCD           PIC X(09). 
      *
           03  WRK-SRYKA               PIC X(02). 
      *
           03  WRK-TENSUKEI        PIC 9(10)V99.
           03  WRK-TENSUKEI-9      PIC S9(10).
           03  WRK-TENSUKEI-G      PIC S9(10).
           03  WRK-TENSUKEI-K      PIC S9(10).
           03  WRK-TENGAKEI-G      PIC S9(10).
           03  WRK-TENGAKEI-K      PIC S9(10).
      *
           03  WRK-TENGAKEI-G2     PIC S9(10).
           03  WRK-TENSUKEI-G2     PIC S9(10).
           03  WRK-GENGAKU-2       PIC S9(10).
      *
           03  WRK-GOK-TENSUKEI        PIC S9(10).
           03  WRK-GOK-TENGAKEI        PIC S9(10).
           03  WRK-GRSI-TENSUKEI       PIC S9(10).
           03  WRK-GRSI-TENSUKEI2      PIC S9(10).
      *
           03  WRK-SUM-AREA.
      *    点数計
               05  WRK-SUM-TENSUKEI        PIC S9(08)V99.
      *    点数計（通則加算対象外計）
               05  WRK-SUM-TENGAKEI        PIC S9(08)V99.
      *    点数計（人工心肺不使用加算計）
               05  WRK-JIN-TENSUKEI        PIC S9(08)V99.
      *    労災点数
               05  WRK-RSI-TENSUKEI        PIC S9(08)V99.
               05  WRK-RSI-TENSUKEI2       PIC S9(08)V99.
      *
           03  WRK-SYOKEIS         PIC 9(08)V9999.
           03  WRK-SYOKEIS2        PIC 9(08)V99.
           03  WRK-SYOKEI          PIC 9(08).
      *    フィルム点数計
           03  WRK-FIRTEN          PIC 9(08).
           03  WRK-FIRTEN-KEI      PIC 9(08).
           03  WRK-FIRTEN-ADD      PIC 9(08).
           03  WRK-KIZKEI          PIC 9(08).
           03  WRK-KIZKEI-ADD      PIC 9(08).
           03  WRK-TENSUKEIS       PIC 9(08)V99.
      *
           03  WRK-ERRCD           PIC X(04).
      *
           03  WRK-SCNT            PIC 9(03).
      *
           03  WRK-INPUTCD         PIC X(22).
      *
           03  WRK-SURYO           PIC 9(07)V9(05).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(07).
               05  WRK-SURYO-S2    PIC 9(05). 
      *
      *    自己血輸血用
           03  WRK-TAIJYU              PIC 9(09).
      *
           03  WRK-TAIJYU-2            PIC 9(05)V9.
           03  WRK-KEISAN              PIC 9(05)V9(03).
      *   きざみ値行為再計算用
           03  WRK-KZMKGN              PIC 9(14)V9(01).
           03  WRK-KZMJGN              PIC 9(14)V9(01).
           03  WRK-KZM                 PIC 9(14)V9(01).
           03  WRK-KEI-TEN             PIC 9(08)V9(04).
           03  WRK-KEI-TEN1            PIC 9(08)V9(04).
           03  WRK-KEI-TEN2            PIC 9(08).
      *
      *    保存用
       01  HOZ-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //HOZ-//.
      *
      *    酸素コードテーブル
           COPY     "CMSANSOTBL.INC".
      *
      *自己血輸血（６歳未満）
       01  CONS-JIKOYK1            PIC X(09)  VALUE    "150286410".
       01  CONS-JIKOYK2            PIC X(09)  VALUE    "150286510".
       01  CONS-JIKOYK3            PIC X(09)  VALUE    "150327710".
       01  CONS-JIKOYK4            PIC X(09)  VALUE    "150327810".
      *
       01  CONS-JIKOCOM            PIC X(09)  VALUE    "840000082".
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ（施設基準）
           COPY    "CPSK1006.INC".
      *    システム管理マスタ（診療科）
           COPY    "CPSK1005.INC".
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
      *    COPY    "CPTENSU.INC".
      *
      *    算定履歴
      *01  SANTEI-REC.
      *    COPY    "CPSANTEI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *
      *    診療行為算定編集パラメタ
           COPY    "CPORCSC90TEN2.INC".
      *
      *    診療行為算定歴チェックパラメタ
           COPY    "CPORCSC91.INC".
      *
      *    診療行為算定歴チェックパラメタ
           COPY    "CPORCSC92.INC".
      *    画面表示編集パラメタ
           COPY    "CPORCSC94.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
      *
      *    診療行為算定用共通パラメタ
           COPY    "CPORCSC00.INC".
      *
       01  WRK-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-//.
      *
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
           MOVE    ZERO                TO  IDX-ZS
           MOVE    ZERO                TO  IDX-ZS2
           MOVE    ZERO                TO  IDX-SANSO
           INITIALIZE                  WRK-AREA
           MOVE    ZERO                TO  FLG-KIJUNN
           MOVE    ZERO                TO  WRK-SCNT
     *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE  )   OR
                              (SPA-ERRCD       NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *
               ADD     1                   TO  IDX-MAX
      *
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1)     =   "."
                                                        OR "*"
                   MOVE    WRK-GMN-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
               ELSE
                   MOVE    WRK-COM-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
                   IF      WRK-COM-INPUTCD(IDX-Z)(1:3) =   "059"
                       MOVE    "7"                     TO  WRK-MOJI
                   END-IF
               END-IF
      *        労災加算振替え
               IF      WRK-COM-INPUTCD(IDX-Z)  =   "099509901"
                   MOVE    "1"                 TO  WRK-MOJI
               END-IF
      *
               EVALUATE    WRK-MOJI
                   WHEN    "0"
                   WHEN    "8"
                   WHEN    "."
                   WHEN    "*"
                       CONTINUE
      *
                   WHEN    OTHER
      *                編集チェックへ
                       PERFORM 10011-CHK-SEC
               END-EVALUATE
           END-PERFORM
      *
      *    酸素の補正率を自動発生する
           IF     (SPA-ERRCD           =   SPACE)   AND
                  (IDX-SANSO           >   ZERO )
               MOVE    SPA-ERRCD          TO  WRK-ERRCD
               MOVE    SPACE              TO  SPA-ERRCD
               PERFORM 10011-SANSO-ADD-SEC
               MOVE    WRK-ERRCD          TO  SPA-ERRCD
           END-IF
      *
           IF      IDX-ZS              >   ZERO
      *        乳幼児、時間外の自動加算を追加する
               MOVE    SPA-ERRCD          TO  WRK-ERRCD
               MOVE    SPACE              TO  SPA-ERRCD
               PERFORM 1002-JIDO-KASAN-SEC
               MOVE    WRK-ERRCD          TO  SPA-ERRCD
           END-IF
      *
           .
       1001-NAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細別内容チェック、自動算定編集処理
      *****************************************************************
       10011-CHK-SEC                  SECTION.
      *
      *    各チェック処理
           EVALUATE    WRK-MOJI
               WHEN    "1"
      *            手技料 加算チェック
                   IF      WRK-COM-DATAKBN (IDX-Z) =   "2"
                       PERFORM 1005-KASAN-CHK-SEC
                   END-IF
      *            手技料
                   IF      WRK-COM-DATAKBN (IDX-Z) =   "1"
                       ADD     1               TO  WRK-SCNT
                       MOVE    IDX-Z           TO  IDX-ZS
                       MOVE    IDX-Z           TO  IDX-ZS2
                   END-IF
      *            施設基準不適合チェック
                   IF      WRK-COM-KIJUNTEIGENKBN(IDX-Z)
                                               NOT =   ZERO
                       PERFORM 100111-KIJUNN-CHK-SEC
                   END-IF
      *            労災の時、労災加算の算定
                   IF      SPA-HKNKBN          =   1
      ***********    AND  (SPA-RSI-HKNKBN  NOT =   "5" )
                     AND  (SPA-RSI-JUNKYO  NOT =   "1" )
                       PERFORM 100113-RSIHKN-SYORI-SEC
                   END-IF
      *            自己血輸血（６歳未満）のコメント
                   IF     (WRK-COM-INPUTCD (IDX-Z) =   CONS-JIKOYK1
                                                   OR  CONS-JIKOYK2
                                                   OR  CONS-JIKOYK3
                                                   OR  CONS-JIKOYK4)
                       PERFORM 100114-JIKOYUKETU-COM-SEC
                   END-IF
      *        薬剤
               WHEN    "6"
      *            麻毒区分
                   IF      WRK-COM-MADOKUKBN (IDX-Z)   =   1
                       MOVE   1                TO  FLG-MADOKU
                   END-IF
      *            生物学的製剤区分
                   IF      WRK-COM-SEIBUTUKBN(IDX-Z)   =   1
                       MOVE   1                TO  FLG-SEIBETU
                   END-IF
      *            残量廃棄
                   PERFORM 1006-HAIKI-SYORI-SEC
      *        器材
               WHEN    "7"
      *DDDD        IF      WRK-COM-DATAKBN (IDX-Z) =   "3"
      *                フィルムの時エラー
      *                MOVE    "0047"          TO  SPA-ERRCD
      *                MOVE    "1"             TO  WRK-COM-CUR(IDX-Z)
      *DDD         END-IF
      *
      *            酸素区分判定
                   IF      WRK-COM-SANSOKBN (IDX-Z)    =   1
      *                酸素の時、酸素補正率を自動加算
      *****************IF      WRK-COM-INPUTCD (IDX-Z) =   "770000000"
                       MOVE    WRK-COM-INPUTCD (IDX-Z) TO  WRK-SRYCD
                       PERFORM 100115-SANSO-CHK-SEC
                       IF      FLG-SANSO-OK        =   1
                           MOVE    IDX-Z       TO  IDX-SANSO
      ******************   PERFORM 10011-SANSO-ADD-SEC
                       ELSE
                           IF      IDX-SANSO        =   ZERO
                               MOVE    "0038"       TO  SPA-ERRCD
                               MOVE    "1"          TO  WRK-COM-CUR
                                                              (IDX-Z)
                           END-IF
                       END-IF
                   END-IF
           END-EVALUATE
      *
           .
       10011-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    酸素判定処理
      *****************************************************************
       100115-SANSO-CHK-SEC           SECTION.
      *
      *
           SET     TBL-SANSO            TO  1
           SEARCH      TBL-SANSO-OCC   VARYING   TBL-SANSO
                   AT  END
                       MOVE    ZERO            TO  FLG-SANSO-OK
                   WHEN    WRK-SRYCD       =   TBL-SANSO-SRYCD
                                                           (TBL-SANSO)
                       MOVE    1               TO  FLG-SANSO-OK
           END-SEARCH 
      *
           .
       100115-SANSO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    残量廃棄コメントの有無
      *****************************************************************
       1006-HAIKI-SYORI-SEC           SECTION.
      *
      *    アンプルの時、小数以下判定
           IF     (WRK-COM-TANICD (IDX-Z)  =   "014" OR "022" OR "046" )
           AND    (LNK-ORCSC-HAIKIKBN-FLG  =   "2"            )
      *
               MOVE    WRK-COM-SURYO(IDX-Z)    TO  WRK-SURYO
               IF     (WRK-SURYO-S2                >   ZERO    )  AND
                      (WRK-COM-AUTOFLG2 (IDX-Z)    =   SPACE   )
                   PERFORM 10011-HAIKI-ADD-SEC
               END-IF
           END-IF
           .
       1006-HAIKI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    破棄コメント自動発生成処理
      *****************************************************************
       10011-HAIKI-ADD-SEC             SECTION.
      *
           MOVE    "099309901"         TO  WRK-SRYCD
           IF      WRK-COM-INPUTCD (IDX-Z + 1) =   WRK-SRYCD
               CONTINUE
           ELSE
               MOVE    IDX-Z               TO  IDX-S
               MOVE    ZERO                TO  FLG-ARI
               PERFORM 1002-KASAN-SET-SEC
               MOVE    IDX-S               TO  IDX-Z
           END-IF
           .
       10011-HAIKI-ADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    施設基準不適合チェック加算処理
      *****************************************************************
       100111-KIJUNN-CHK-SEC           SECTION.
      *
      *H27.4
           IF     (SPA-SRYYMD          >=  "20150401" )
             AND  (WRK-COM-KIJUNTEIGENKBN (IDX-Z)  =   2  OR  3 )
      *        施設基準不適応
               PERFORM 1001111-SSTKIJUN-CHK-SEC
      *        施設基準適応
               IF      FLG-ERR             =   ZERO
                   MOVE    1                   TO  FLG-KIJUNN
               ELSE
      *        施設基準不適応
                   MOVE    2                   TO  FLG-KIJUNN
      *            施設基準不適合（手術）
                   MOVE    "150382490"         TO  WRK-SRYCD
                   IF      WRK-COM-INPUTCD (IDX-Z + 1) =   WRK-SRYCD
                       COMPUTE IDX-S               =   IDX-Z  +  1
                       MOVE    1                   TO  FLG-ARI
                   ELSE
                       MOVE    IDX-Z               TO  IDX-S
                       MOVE    ZERO                TO  FLG-ARI
                   END-IF
                   IF      FLG-ARI         =   1
                       MOVE    IDX-S           TO  IDX-T
                   END-IF
                   PERFORM 1002-KASAN-SET-SEC
                   IF      FLG-ADD         =   1
                       MOVE    IDX-S           TO  IDX-ZS2
                       MOVE    2               TO  WRK-COM-SANFLG
                                                          (IDX-S)
                   END-IF
               END-IF
           END-IF
      *
      *    加算の時、対象の手技が有ること
           IF      WRK-COM-KIJUNTEIGENKBN (IDX-Z)  =   1
               MOVE    2               TO  WRK-COM-SANFLG (IDX-Z)
               IF      FLG-KIJUNN          =   ZERO
                   MOVE    "0173"          TO  SPA-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
               END-IF
           END-IF
           .
       100111-KIJUNN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    施設基準不適応チェック処理
      *****************************************************************
       1001111-SSTKIJUN-CHK-SEC        SECTION.
      *
           MOVE    WRK-COM-KIJUNTEIGENCD (IDX-Z)
                                       TO  IDY
           MOVE    ZERO                TO  FLG-ERR
      *H26.4
           MOVE    ZERO                TO  IDS
      *    施設基準情報検索
           MOVE    SPACE               TO  SYS-1006-REC
           MOVE    "1006"              TO  SYS-1006-KANRICD
           EVALUATE    TRUE
               WHEN    IDY             <=  500
                   MOVE    "1"                 TO  SYS-1006-KBNCD
                   MOVE    ZERO                TO  IDS
               WHEN    IDY             <=  1000
                   MOVE    "2"                 TO  SYS-1006-KBNCD
                   MOVE    500                 TO  IDS
               WHEN    IDY             <=  3500
                   MOVE    "7"                 TO  SYS-1006-KBNCD
                   MOVE    3000                TO  IDS
               WHEN    IDY             <=  8500
                   MOVE    "17"                TO  SYS-1006-KBNCD
                   MOVE    8000                TO  IDS
           END-EVALUATE
           MOVE    SPA-SRYYMD          TO  SYS-1006-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1006-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1006-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1006-REC        TO  SYSKANRI-REC
      *    システム管理検索
           PERFORM 9001-SYSKANRI-KEY-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1006-REC
           ELSE
               INITIALIZE                  SYS-1006-REC
           END-IF
      *
      *    基準不適合逓減対象施設基準
           IF      SYS-1006-SSTKIJUN (IDY - IDS)   =   1
               CONTINUE
           ELSE
               MOVE    1                   TO  FLG-ERR
           END-IF
      *
           .
       1001111-SSTKIJUN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    施設規準自動算定位置処理
      *****************************************************************
       1001112-KIJUNN-ICHI-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDX-TUS
      *
           PERFORM VARYING     IDX1    FROM    IDX-S   BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE     )   OR
                             ((WRK-GMN-INPUTCD (IDX1)  =   SPACE ) AND
                              (WRK-COM-INPUTCD (IDX1)  =   SPACE)) OR
                              (FLG-ARI                 =   1     )
      *
               IF      WRK-COM-INPUTCD (IDX1)  =   WRK-SRYCD  OR
                                                   WRK-SRYCD2
                   MOVE    1                   TO  FLG-ARI
                   MOVE    IDX1                TO  IDX-TUS
               END-IF
      *
      *        次の手術をまで対象とする
               IF     (IDX1                >   IDX-S     )   AND
                      (WRK-COM-DATAKBN (IDX1)  =   "1"   )   AND
                      (WRK-COM-INPUTCD (IDX1)(1:1) =   "1" )
                   MOVE    SPA-MAX-LINE            TO  IDX1
               ELSE
      *        最後の通則加算対象位置
                   IF     (WRK-COM-INPUTCD (IDX1)(1:1) =   "1" )  AND
                          (WRK-COM-TUSOKUGAIKBN(IDX1)  NOT =   "1" )
                       MOVE    IDX1            TO  IDX-TUS
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      IDX-TUS             =   ZERO
               MOVE    IDX-Z           TO  IDX-TUS
           END-IF
      *
           .
       1001112-KIJUNN-ICHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    酸素補正率自動加算処理
      *****************************************************************
       10011-SANSO-ADD-SEC                  SECTION.
      *
           MOVE    "770020070"         TO  WRK-SRYCD
           MOVE    ZERO                TO  FLG-ARI
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1    >   SPA-MAX-LINE  )  OR
                              (WRK-COM-INPUTCD(IDX1)   =   SPACE AND
                               WRK-GMN-INPUTCD(IDX1)   =   SPACE )
               IF       WRK-COM-INPUTCD (IDX1) =   WRK-SRYCD
                   MOVE    IDX1                TO  IDX-T
                   MOVE    1                   TO  FLG-ARI
               END-IF
           END-PERFORM
           IF      FLG-ARI             =   ZERO
               MOVE    IDX-SANSO           TO  IDX-S
           END-IF
      *    酸素補正率自動加算
           PERFORM 1002-KASAN-SET-SEC
      *
           .
       10011-SANSO-ADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災加算の算定処理
      *****************************************************************
       100113-RSIHKN-SYORI-SEC        SECTION.
      *
      *    初診時ブラッシング料は初診算定時のみの算定とする
           IF     (IDX-ZS                  >   ZERO       )  AND
                  (WRK-COM-INPUTCD(IDX-ZS) =   "101400030"
                                           OR  "101500150" )
      *@@      IF      LNK-ORCSC-SYOSIN2       =   1
      *            CONTINUE
      *        ELSE
      *            MOVE    "8014"              TO  SPA-ERRCD
      *            MOVE    "1"                 TO  WRK-COM-CUR (IDX-ZS)
      *@@      END-IF
      *        アフターケアの時算定不可
               IF      SPA-RSI-HKNKBN          =   "3"
                   MOVE    "8018"              TO  SPA-ERRCD
                   MOVE    "1"                 TO  WRK-COM-CUR (IDX-ZS)
               END-IF
           END-IF
      *
           .
       100113-RSIHKN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災自動加算追加処理
      *****************************************************************
       10013-RISIKASAN-SET-SEC        SECTION.
      *
           MOVE    ZERO                TO  WRK-SCNT
           MOVE    SPACE               TO  WRK-RSI-SRYCD
           MOVE    ZERO                TO  FLG-ROUSAI
           MOVE    ZERO                TO  FLG-ROU-ARI
           MOVE    ZERO                TO  IDX-KSN
           MOVE    ZERO                TO  IDX-SYU
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE     )   OR
                             ((WRK-GMN-INPUTCD (IDX1)  =   SPACE ) AND
                              (WRK-COM-INPUTCD (IDX1)  =   SPACE ))
                         OR   (SPA-ERRCD           NOT =   SPACE )
                         OR   (SPA-RSI-SISIKBN         =   ZERO  )
               MOVE    ZERO                TO  FLG-ROUSAI
               IF      (WRK-COM-INPUTCD(IDX1)(1:1) =   "1"  )  AND
                       (WRK-COM-INPUTCD(IDX1)(1:3) NOT =   "101")  AND
                       (WRK-COM-DATAKBN(IDX1)      =   "1"  )   AND
                       (WRK-COM-CDKBN-KBN (IDX1)   =   "K"  )
                   ADD     1                       TO  WRK-SCNT
      *        労災用　区分番号
                   MOVE    SPACE                   TO  WRK-SRYCD
                   EVALUATE    WRK-COM-CDKBN-KBNNUM (IDX1)
                   WHEN    "000"  THRU  "002"
      *            筋骨格系・四肢・体幹手術
                   WHEN    "023"  THRU  "144"
      *            神経系・頭蓋骨（？？これはどうかはわからない）
                   WHEN    "145"  THRU  "198"
      *以下は？？？WHEN    "001"
                       EVALUATE    SPA-RSI-SISIKBN
                           WHEN    1
      *                        １．５倍
                               MOVE    "101500020"      TO  WRK-SRYCD
                           WHEN    2
      *                        ２倍
                               MOVE    "101500010"      TO  WRK-SRYCD
                       END-EVALUATE
                       MOVE    1                    TO  FLG-ROUSAI
                       MOVE    1                    TO  FLG-ROU-ARI
                   END-EVALUATE
                   IF      WRK-SCNT            >   1
                       IF     ((FLG-ROUSAI          =   1  )  AND
                               (WRK-RSI-SRYCD       =   SPACE))  OR
                              ((FLG-ROUSAI          =   ZERO) AND
                               (WRK-RSI-SRYCD   NOT =   SPACE))
                           MOVE    "8026"          TO  SPA-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR(IDX1)
                       END-IF
                   END-IF
                   MOVE    WRK-SRYCD            TO  WRK-RSI-SRYCD
                   MOVE    IDX1            TO  IDX-SYU
               END-IF
               IF       WRK-COM-INPUTCD (IDX1) =   "101500010"  OR
                                                   "101500020"  OR
                                                   "099509901"
      *D           IF      FLG-ROU-ARI     =   ZERO
      *                MOVE    "8004"          TO  SPA-ERRCD
      *                MOVE    "1"             TO  WRK-COM-CUR (IDX1)
      *                MOVE    SPACE           TO  WRK-COM-CUR (IDX1)
      *D           ELSE
                       IF      IDX-KSN         >   ZERO
                           MOVE    "8004"      TO  SPA-ERRCD
                           MOVE    "1"         TO  WRK-COM-CUR (IDX1)
                       ELSE
                           MOVE    IDX1        TO  IDX-KSN
                       END-IF
      *D           END-IF
               END-IF
           END-PERFORM
      *    自動発生の有無
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (IDX-KSN             =   ZERO  )  AND
                  (WRK-RSI-SRYCD   NOT =   SPACE )
               MOVE    ZERO                TO  IDX-T
               PERFORM VARYING     IDX1    FROM    IDX-SYU BY  1
                       UNTIL      (IDX1        >   SPA-MAX-LINE )   OR
                                 ((WRK-GMN-INPUTCD (IDX1)  =   SPACE )
                              AND (WRK-COM-INPUTCD (IDX1)  =   SPACE ))
                   IF      (WRK-COM-INPUTCD(IDX1)(1:1) =   "1"  )  AND
                           (WRK-COM-INPUTCD(IDX1)(1:3) NOT =   "101")
                        MOVE    IDX1               TO  IDX-T
                   END-IF
               END-PERFORM
               MOVE    ZERO                TO  FLG-ARI
               IF      IDX-T                   =   ZERO
                   MOVE    IDX-SYU             TO  IDX-T
               END-IF
               MOVE    IDX-T               TO  IDX-S
      *        加算料追加処理
               MOVE    WRK-RSI-SRYCD       TO  WRK-SRYCD
               PERFORM 1002-KASAN-SET-SEC
               IF      FLG-ADD             =   1
                   MOVE    "2"             TO  WRK-COM-AUTOKBN (IDX-S)
               END-IF
           END-IF
      *
           MOVE    SPA-ERRCD           TO  WRK-ERRCD
           .
       10013-RISIKASAN-SET-EXT.
           EXIT.
      *
      *H25.5
      *****************************************************************
      *    自己血輸血（６歳未満）のコメント追加処理
      *****************************************************************
       100114-JIKOYUKETU-COM-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-ARI
           PERFORM VARYING     IDX1    FROM    IDX-Z   BY  1
                   UNTIL      (IDX1    >   SPA-MAX-LINE  )  OR
                             ((WRK-COM-INPUTCD(IDX1)   =   SPACE ) AND
                              (WRK-GMN-INPUTCD(IDX1)   =   SPACE ))
               IF       WRK-COM-INPUTCD (IDX1) =   CONS-JIKOCOM
                   MOVE    1                   TO  FLG-ARI
               END-IF
           END-PERFORM
      *
           IF      FLG-ARI             =   ZERO
               MOVE    CONS-JIKOCOM        TO  WRK-SRYCD
               MOVE    IDX-Z               TO  IDX-S
               MOVE    ZERO                TO  FLG-ARI
               PERFORM 1002-KASAN-SET-SEC
               MOVE    IDX-S               TO  IDX-Z
               IF      FLG-ADD             =   1
      *            継続コメントとする
                   MOVE    "1"             TO  WRK-COM-INPUTCONT (IDX-Z)
                   INITIALIZE                  ORCSC94AREA
                   MOVE    "1"                 TO  LNK-SC94-KBN
                   MOVE    LNK-ORCSC-PG        TO  LNK-SC94-PG
                   MOVE    IDX-Z               TO  LNK-SC94-IDX
                   CALL    "ORCSC94"           USING
                                               ORCSC94AREA
                                               WRK-SCR-REC
                                               SPA-AREA
               END-IF
           END-IF
           .
       100114-JIKOYUKETU-COM-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動加算追加処理
      *****************************************************************
       1002-JIDO-KASAN-SEC        SECTION.
      *
           MOVE    ZERO                TO  IDX-SYU
           MOVE    ZERO                TO  IDX-NEN
           MOVE    ZERO                TO  IDX-TUS
      *
           PERFORM VARYING     IDX1    FROM    1    BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE     ) OR
                              (WRK-GMN-INPUTCD (IDX1)  =   SPACE  AND
                               WRK-COM-INPUTCD (IDX1)  =   SPACE )
      *        主たる手術を判定対象とする
               IF     (WRK-COM-DATAKBN (IDX1)  =   "1"   )   AND
                      (WRK-COM-INPUTCD (IDX1)(1:1) =   "1" )
                   IF      IDX-SYU         =   ZERO
                       MOVE    IDX1            TO  IDX-SYU
                  END-IF
               END-IF
      *        最後の通則加算対象位置
               IF     (WRK-COM-INPUTCD (IDX1)(1:1) =   "1" )  AND
                      (WRK-COM-TUSOKUGAIKBN(IDX1)  NOT =   "1" )
      *H27.4
      *            ％減算で前手技が通則外は対象外
                   IF     (WRK-COM-DATAKBN (IDX1)  =   "2" )
                      AND (WRK-COM-SANFLG  (IDX1)  =   2   )
                      AND (WRK-COM-DATAKBN (IDX1 - 1)      =   "1" )
                      AND (WRK-COM-TUSOKUGAIKBN(IDX1 - 1)  =   "1" )
                       CONTINUE
                   ELSE
                       MOVE    IDX1            TO  IDX-NEN
                   END-IF
               END-IF
           END-PERFORM
      *
      *****MOVE    IDX-ZS2             TO  IDX-S
           MOVE    IDX-SYU             TO  IDX-ZS
      *
      *    乳幼児加算（６歳未満で手術の時）
           IF     (LNK-ORCSC-NENREI-YY <   6  )  AND
                  (WRK-COM-SRYSYUKBN (IDX-ZS) =   "500" )
               PERFORM 10011-YOUJIKASAN-SET-SEC
           END-IF
      *
      *    年齢加算処理
           PERFORM 10013-NENREI-SET-SEC
      *
      *    外来で診察料の無い場合の時間加算
           IF      (SPA-NYUGAIKBN          =   "2"  )  AND
                   (LNK-ORCSC-PG           =   "K02")  AND
                   (LNK-ORCSC-SYORIFLG     =   ZERO )  AND
                   (SPA-SRYYMD             =   SPA-SYSYMD) AND
                   (SPA-HKNCOMBINUM    NOT =   "9999") AND
                   (LNK-ORCSC-SINSATU-ARI  =   ZERO )  AND
                   (SPA-GAI-JIKANKBN   NOT =   ZERO )  AND
                   (WRK-COM-TIMEKSNKBN(IDX-ZS)
                                       NOT =   ZERO )
               IF      (SPA-GAI-JIKANKBN       =   8 )
                   CONTINUE
               ELSE
                   PERFORM 10019-JIKANKASAN-CHK-SEC
               END-IF
           END-IF
      *    時間加算
           IF     ((LNK-ORCSC-TIMEFLG          NOT =   ZERO )  OR
                   (WRK-COM-TIMEFLG (IDX-ZS)   NOT =   ZERO ))  AND
                   (WRK-COM-TIMEKSNKBN(IDX-ZS) NOT =   ZERO )
               PERFORM 10012-JIKANKASAN-SET-SEC
           END-IF
      *    労災加算
           IF      (SPA-HKNKBN         =   1   )  AND
      ************ (SPA-RSI-HKNKBN NOT =   "5" )  AND
                   (SPA-RSI-JUNKYO NOT =   "1" )  AND
                   (SPA-ERRCD          =   SPACE ) AND
                   (WRK-ERRCD          =   SPACE ) 
               PERFORM 10013-RISIKASAN-SET-SEC
           END-IF
      *
           .
       1002-JIDO-KASAN-EXT.
           EXIT.
      *****************************************************************
      *    乳幼児加算料追加処理
      *****************************************************************
       10011-YOUJIKASAN-SET-SEC        SECTION.
      *
      *    通則年齢区分があるか、労災コードの場合のみ
           IF     (WRK-COM-TSUSOKUAGE (IDX-ZS) =   "1" )  OR
                 ((WRK-COM-INPUTCD (IDX-ZS)(1:3)   =  "101") AND
      *        初診時ブラッシング料は対象外
                  (WRK-COM-INPUTCD(IDX-ZS) NOT =   "101400030"
                                               AND "101500150" ))
               CONTINUE
           ELSE
               GO      TO      10011-YOUJIKASAN-SET-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-SRYCD
      *
      *    極低出生体重児加算区分
           IF      WRK-COM-CHPMESUKSN (IDX-ZS)  =   1
               PERFORM 100112-TEITAIJYU-CHK-SEC
               IF     (FLG-ADD             =   1 )  OR
                      (FLG-ARI             =   1 )
                   GO      TO      10011-YOUJIKASAN-SET-EXT
               END-IF
           END-IF
      *
           IF     (LNK-ORCSC-NENREI-YY <   3  )
      *    ３歳児未満の加算
               MOVE    "150000290"         TO  WRK-SRYCD
           ELSE
      *    ３歳から６歳未満の加算
               MOVE    "150342890"         TO  WRK-SRYCD
           END-IF
      *    加算料追加処理
           PERFORM 100111-KASAN-CHK-SEC
           IF      FLG-ARI             =   ZERO
      *        最後の通則年齢加算位置
               MOVE    IDX-NEN             TO  IDX-S
           END-IF
      *    加算料追加処理
           PERFORM 1002-KASAN-SET-SEC
      *
           IF      FLG-ADD             =   1
      *        自動追加分（入力時に追加）
               MOVE    "2"                 TO  WRK-COM-AUTOKBN (IDX-S)
               MOVE    1                   TO  WRK-COM-SANFLG (IDX-S)
           END-IF
      *
           .
       10011-YOUJIKASAN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    極低出生体重児加算区分処理
      *****************************************************************
       100112-TEITAIJYU-CHK-SEC        SECTION.
      *
      *    低体重児加算
           MOVE    "150306890"         TO  WRK-SRYCD
           PERFORM 100111-KASAN-CHK-SEC
      *    入力なし
           IF      FLG-ARI             =   ZERO
      *        新生児加算
               MOVE    "150000190"         TO  WRK-SRYCD
      *        加算料追加処理
               PERFORM 100111-KASAN-CHK-SEC
               IF      FLG-ARI             =   ZERO
      *            最後の通則年齢加算位置
                   MOVE    IDX-NEN             TO  IDX-S
               END-IF
      *        加算料追加処理
               PERFORM 1002-KASAN-SET-SEC
               IF      FLG-ADD             =   1
      *            自動追加分（入力時に追加）
                   MOVE    "2"             TO  WRK-COM-AUTOKBN (IDX-S)
                   MOVE    1               TO  WRK-COM-SANFLG (IDX-S)
               END-IF
           ELSE
               MOVE    1               TO  WRK-COM-SANFLG (IDX-T)
           END-IF
           .
       100112-TEITAIJYU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動加算　存在チェック加処理
      *****************************************************************
       100111-KASAN-CHK-SEC        SECTION.
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
       100111-KASAN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来で診察料の無い場合の時間加算処理
      *****************************************************************
       10019-JIKANKASAN-CHK-SEC        SECTION.
      *
      *    時間外の有無
           IF     (WRK-GMN-INPUTCD (IDX-ZS)(1:1)       NUMERIC  ) AND
                  (WRK-GMN-INPUTCD (IDX-ZS)(2:1)       =   SPACE) AND
                  (WRK-GMN-INPUTCD (IDX-ZS)(3:)    NOT =   SPACE)
               CONTINUE
           ELSE
      *        時間外設定
               MOVE    WRK-GMN-INPUTCD (IDX-ZS)    TO  WRK-INPUTCD
               MOVE    SPACE                       TO  WRK-GMN-INPUTCD
                                                         (IDX-ZS)
               MOVE    SPA-GAI-JIKANKBN            TO  WRK-GMN-INPUTCD
                                                         (IDX-ZS)(1:1)
               MOVE    WRK-INPUTCD                 TO  WRK-GMN-INPUTCD
                                                         (IDX-ZS)(3:)
               MOVE    SPA-GAI-JIKANKBN            TO  WRK-COM-TIMEFLG
                                                         (IDX-ZS)(1:1)
           END-IF
      *
           .
       10019-JIKANKASAN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    時間加算加算料追加処理
      *****************************************************************
       10012-JIKANKASAN-SET-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYCD
           MOVE    SPACE               TO  WRK-SRYCD1
           MOVE    SPACE               TO  WRK-SRYCD2
           MOVE    SPACE               TO  WRK-SRYCD3
      *
           MOVE    LNK-ORCSC-TIMEFLG   TO  WRK-TIMEFLG
           IF      WRK-TIMEFLG         =   ZERO
               MOVE    WRK-COM-TIMEFLG(IDX-ZS)     TO  WRK-TIMEFLG
           END-IF
      *
      *    入院の時、手技料の時間外フラグを優先する
           IF     (SPA-NYUGAIKBN       =   "1"  )  AND
                  (WRK-COM-TIMEFLG(IDX-ZS) NOT =   ZERO )
      *        時間外区分の無い場合はクリアする
               IF     (WRK-GMN-INPUTCD(IDX-ZS)(1:1)    NUMERIC) AND
                      (WRK-GMN-INPUTCD(IDX-ZS)(2:1)    =   SPACE )  AND
                      (WRK-GMN-INPUTCD(IDX-ZS)(3:) NOT =   SPACE )
                   MOVE    WRK-COM-TIMEFLG(IDX-ZS) TO  WRK-TIMEFLG
               ELSE
                   IF      LNK-ORCSC-TIMEFLG       =   ZERO
                       MOVE    ZERO                TO  WRK-TIMEFLG
                       MOVE    ZERO                TO  WRK-COM-TIMEFLG
                                                                (IDX-ZS)
                   END-IF
               END-IF
           END-IF
      *
      *
      *    時間加算
           EVALUATE    WRK-TIMEFLG
      *      /* 時間外 */
               WHEN    1
                   MOVE    "150000490"     TO  WRK-SRYCD2
      *            時間外１
                   MOVE    "150371390"     TO  WRK-SRYCD1
      *     /* 休日 */
               WHEN    2
                   MOVE    "150000590"     TO  WRK-SRYCD2
                   MOVE    "150371290"     TO  WRK-SRYCD1
      *        /* 深夜 */
               WHEN    3
                   MOVE    "150000690"     TO  WRK-SRYCD2
                   MOVE    "150371490"     TO  WRK-SRYCD1
      *        /* 時間外特例 */
               WHEN    4
                   MOVE    "150000790"     TO  WRK-SRYCD2
                   MOVE    "150371590"     TO  WRK-SRYCD1
           END-EVALUATE
      *    時間外加算なし
           IF      WRK-SRYCD2           =    SPACE
               GO      TO      10012-JIKANKASAN-SET-EXT
           END-IF
      *
      *    時間自動加算　存在チェック
           MOVE    ZERO                TO  FLG-ARI
           MOVE    ZERO                TO  IDX-T
           MOVE    ZERO                TO  IDX-TUS
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE     ) OR
                              (WRK-GMN-INPUTCD (IDX1)  =   SPACE  AND
                               WRK-COM-INPUTCD (IDX1)  =   SPACE )
                           OR (SPA-ERRCD       NOT =   SPACE  )
               IF     (WRK-COM-DATAKBN    (IDX1)       =   "2"  )  AND
                      (WRK-COM-TIMEKSNKBN (IDX1)   NOT =   ZERO )
                    MOVE    1              TO  FLG-ARI
                    IF      IDX-T          =   ZERO
                        MOVE    IDX1           TO  IDX-T
                    ELSE
                        MOVE    "0043"         TO  SPA-ERRCD
                        MOVE    "1"            TO  WRK-COM-CUR (IDX1)
                    END-IF
               END-IF
      *        最後の通加算対象位置
               IF     (WRK-COM-INPUTCD (IDX1)(1:1) =   "1" )  AND
                      (WRK-COM-TUSOKUGAIKBN (IDX1) =   "0" )
      *H27.4
      *            ％減算で前手技が通則外は対象外
                   IF     (WRK-COM-DATAKBN (IDX1)  =   "2" )
                      AND (WRK-COM-SANFLG  (IDX1)  =   2   )
                      AND (WRK-COM-DATAKBN (IDX1 - 1)      =   "1" )
                      AND (WRK-COM-TUSOKUGAIKBN(IDX1 - 1)  =   "1" )
                       CONTINUE
                   ELSE
                       MOVE    IDX1            TO  IDX-TUS
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      10012-JIKANKASAN-SET-EXT
           END-IF
      *
           IF      FLG-ARI             =   1
      *        時間外加算ありの時は、追加処理なし
               IF      WRK-COM-INPUTCD (IDX-T) =   WRK-SRYCD1
                                               OR  WRK-SRYCD2
      *            追加処理なし
                   MOVE    2               TO  FLG-ARI
               END-IF
           END-IF
      *
           IF      IDX-T               =   ZERO
               IF      IDX-TUS             =   ZERO
                   MOVE    IDX-ZS              TO  IDX-S
               ELSE
                   MOVE    IDX-TUS             TO  IDX-S
               END-IF
           ELSE
               MOVE    IDX-T               TO  IDX-S
           END-IF
      *    加算料追加処理
           IF      FLG-ARI             =   2
               MOVE    1               TO  WRK-COM-SANFLG (IDX-S)
           ELSE
      *        時間外加算
               PERFORM 100125-TIMEKSN-HENSYU-SEC
           END-IF
      *
      *    入院の時、手技料に時間外フラグを設定する
           IF     (SPA-NYUGAIKBN       =   "1"  )  AND
                  (WRK-COM-TIMEFLG(IDX-ZS) =   ZERO )
               MOVE    WRK-TIMEFLG         TO  WRK-COM-TIMEFLG(IDX-ZS)
           END-IF
      *
           .
       10012-JIKANKASAN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    時間外加算算定チェック処理
      *****************************************************************
       100125-TIMEKSN-HENSYU-SEC            SECTION.
      *
      *    システム管理検索
           PERFORM 1001251-SYS1005-HEN-SEC
      *
           MOVE    ZERO                TO  FLG-ADD
      *    時間外加算１のチェック
           IF      WRK-SRYCD1      NOT =    SPACE
               MOVE    WRK-SRYCD1      TO  WRK-SRYCD
               PERFORM 1002-KASAN-SET-SEC
               IF      FLG-ADD             =   1
                   MOVE    "2"             TO  WRK-COM-AUTOKBN (IDX-S)
                   MOVE    1               TO  WRK-COM-SANFLG (IDX-S)
               END-IF
           END-IF
           IF      FLG-ADD             =   ZERO
               MOVE    WRK-SRYCD2      TO  WRK-SRYCD
               PERFORM 1002-KASAN-SET-SEC
               IF      FLG-ADD             =   1
                   MOVE    "2"             TO  WRK-COM-AUTOKBN (IDX-S)
                   MOVE    1               TO  WRK-COM-SANFLG (IDX-S)
               END-IF
           END-IF
      *
           .
       100125-TIMEKSN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    手術時間外加算等１算定チェック処理
      *****************************************************************
       1001251-SYS1005-HEN-SEC            SECTION.
      *
           IF      WRK-COM-SRYKA (IDX-ZS)  =   SPACE
               MOVE    SPA-SRYKA               TO  WRK-SRYKA
           ELSE
               MOVE    WRK-COM-SRYKA (IDX-ZS)  TO  WRK-SRYKA
           END-IF
      *
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                      SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    WRK-SRYKA           TO  SYS-1005-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1005-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  SYSKANRI-REC
      *    システム管理検索
           PERFORM 9001-SYSKANRI-KEY-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1005-REC
           ELSE
               INITIALIZE                      SYS-1005-REC
           END-IF
           IF      SYS-1005-JIKANKSN1-50   =   "1"
               CONTINUE
           ELSE
               MOVE    SPACE           TO  WRK-SRYCD1
           END-IF
           .
       1001251-SYS1005-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    年齢加算追加処理（共通）
      *****************************************************************
       10013-NENREI-SET-SEC            SECTION.
      *
      *    手技料の直下とする
           MOVE    IDX-ZS              TO  IDX-S
      *    自己血輸血（６歳未満）の時、継続コメントの下
           IF     (WRK-COM-INPUTCD(IDX-ZS) =   CONS-JIKOYK1
                                           OR  CONS-JIKOYK2
                                           OR  CONS-JIKOYK3
                                           OR  CONS-JIKOYK4)
               PERFORM VARYING     IDXS    FROM    IDX-ZS  BY  1
                       UNTIL      (IDXS        >   SPA-MAX-LINE )
                            OR    (WRK-COM-INPUTCD (IDXS)  =   SPACE)
                   IF      (WRK-COM-INPUTCD (IDXS)(1:1)    =   "8")
                       OR  (WRK-COM-INPUTCD (IDXS)(1:3)    =   "008")
                       IF      WRK-COM-INPUTCONT (IDXS)    =   "1"
                           MOVE    IDXS            TO  IDX-S
                       END-IF
                   ELSE
                       IF      IDXS                >   IDX-ZS
                           MOVE    SPA-MAX-LINE        TO  IDXS
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2        >   4       )   OR
                              (WRK-COM-AGEKSNSRYCD(IDX-ZS IDX2)
                                               =   SPACE  OR
                                                   ZERO )
      *        加算追加
               MOVE    WRK-COM-AGEKSNSRYCD (IDX-ZS IDX2)  TO  WRK-SRYCD
      *        加算料チェック処理
               PERFORM 100111-KASAN-CHK-SEC
      *        加算料追加処理
               PERFORM 1002-KASAN-SET-SEC
               IF      FLG-ADD            =   1
                   MOVE    "2"            TO  WRK-COM-AUTOKBN (IDX-S)
                   MOVE    1              TO  WRK-COM-SANFLG  (IDX-S)
                   MOVE    4              TO  IDX2
               END-IF
           END-PERFORM
      *
           .
       10013-NENREI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    加算料追加処理
      *****************************************************************
       1002-KASAN-SET-SEC              SECTION.
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
           MOVE    WRK-SCR-REC         TO  HOZ-SCR-REC
           INITIALIZE                  ORCSC92AREA
           IF      FLG-ARI             =   ZERO
               MOVE    "1"                 TO  LNK-SC92-KBN
           ELSE
               MOVE    SPACE               TO  WRK-COM-CHKFLG (IDX-S)
           END-IF
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC92-SYORIFLG
           MOVE    IDX-S               TO  LNK-SC92-GMN-IDX
           MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
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
               MOVE    SPACE               TO  SPA-ERRCD
               MOVE    HOZ-SCR-REC         TO  WRK-SCR-REC
               IF      FLG-ARI             =   ZERO
                   COMPUTE IDX-S           =   IDX-S
                                           -   1
               END-IF
               GO      TO      1002-KASAN-SET-EXT
           END-IF
      *
           MOVE    WRK-COM-SRYKBN (IDX-S - 1)
                                       TO  WRK-COM-SRYKBN(IDX-S)
           MOVE    SPACE               TO  WRK-GMN-SRYKBN(IDX-S)
           MOVE    WRK-COM-SRYSYUKBN (IDX-S - 1)
                                       TO  WRK-COM-SRYSYUKBN(IDX-S)
      *
      *    自動追加分（入力時に追加）
           MOVE    "2"                 TO  WRK-COM-AUTOKBN (IDX-S)
      *
           ADD     1                   TO  WRK-GMN-MAX
      *
           MOVE    1                   TO  FLG-ADD
           ADD     1                   TO  IDX-Z
           .
       1002-KASAN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    加算料チェック処理
      *****************************************************************
       1005-KASAN-CHK-SEC                  SECTION.
      *
      *    加算データの時、前に手技料があること
           MOVE    ZERO                TO  FLG-ARI
           MOVE    ZERO                TO  IDX-SYU2
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL       IDX1    >=  IDX-Z
               IF     (WRK-COM-INPUTCD(IDX1)(1:1)  =   "1" )  AND
                      (WRK-COM-DATAKBN(IDX1)   NOT =   "2" )
                   IF      WRK-COM-CHUKSNCD (IDX1)     =
                                               WRK-COM-CHUKSNCD(IDX-Z)
                       MOVE    1               TO  FLG-ARIOK
                   END-IF
                   MOVE    1               TO  FLG-ARI
                   MOVE    IDX1            TO  IDX-SYU2
                   MOVE    IDX-Z           TO  IDX1
               END-IF
           END-PERFORM
      *
           IF      FLG-ARI             =   ZERO
               MOVE    "0013"          TO  SPA-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR(IDX-Z)
           END-IF
      *    低体重児加算チェック
           IF      SPA-ERRCD           =   SPACE
               IF      WRK-COM-INPUTCD(IDX-Z)   =   "150306890" 
                   IF      WRK-COM-CHPMESUKSN(IDX-Z) =   ZERO
                       MOVE    "2068"          TO  SPA-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR(IDX-Z)
                   END-IF
               END-IF
           END-IF
      *    画像等手術支援加算
           IF     (SPA-ERRCD           =   SPACE )
              AND (IDX-SYU2        NOT =   ZERO  )
              AND (WRK-COM-GAZOOPESUP(IDX-Z)
                                   NOT =   ZERO  )
      *        手技の画像等手術支援加算との判定
               EVALUATE    WRK-COM-GAZOOPESUP(IDX-SYU2)
                   WHEN    1
                       IF      WRK-COM-GAZOOPESUP(IDX-Z)   =   1
                           CONTINUE
                       ELSE
                           MOVE    "0214"          TO  SPA-ERRCD
                       END-IF
                   WHEN    2
                       IF      WRK-COM-GAZOOPESUP(IDX-Z)   =   2
                           CONTINUE
                       ELSE
                           MOVE    "0214"          TO  SPA-ERRCD
                       END-IF
                   WHEN    3
                       IF      WRK-COM-GAZOOPESUP(IDX-Z)   =   1
                                                           OR  2
                           CONTINUE
                       ELSE
                           MOVE    "0214"          TO  SPA-ERRCD
                       END-IF
                   WHEN    4
                       IF      WRK-COM-GAZOOPESUP(IDX-Z)   =   4
                           CONTINUE
                       ELSE
                           MOVE    "0214"          TO  SPA-ERRCD
                       END-IF
                   WHEN    5
                       IF      WRK-COM-GAZOOPESUP(IDX-Z)   =   1
                                                           OR  4
                           CONTINUE
                       ELSE
                           MOVE    "0214"          TO  SPA-ERRCD
                       END-IF
                   WHEN    0
                       MOVE    "0213"          TO  SPA-ERRCD
               END-EVALUATE
               IF      SPA-ERRCD           NOT =   SPACE
                   MOVE    "1"             TO  WRK-COM-CUR(IDX-Z)
               END-IF
           END-IF
      *    副鼻腔手術用内視鏡加算
           IF     (SPA-ERRCD           =   SPACE )
              AND (IDX-SYU2        NOT =   ZERO  )
              AND (WRK-COM-YOBI1 (IDX-Z)(2:1)
                                   NOT =   ZERO  )
      *        手技料の加算と不一致はエラー
               IF      WRK-COM-YOBI1(IDX-SYU2)(2:1)
                                   NOT =   WRK-COM-YOBI1 (IDX-Z)(2:1)
                   MOVE    "0511"          TO  SPA-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR(IDX-Z)
               END-IF
           END-IF
      *    副鼻腔切除機器加算
           IF     (SPA-ERRCD           =   SPACE )
              AND (IDX-SYU2        NOT =   ZERO  )
              AND (WRK-COM-YOBI1 (IDX-Z)(3:1)
                                   NOT =   ZERO  )
      *        手技料の加算と不一致はエラー
               IF      WRK-COM-YOBI1(IDX-SYU2)(3:1)
                                   NOT =   WRK-COM-YOBI1 (IDX-Z)(3:1)
                   MOVE    "0512"          TO  SPA-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR(IDX-Z)
               END-IF
           END-IF
      *    脊髄誘発電位測定加算
           IF     (SPA-ERRCD           =   SPACE )
              AND (IDX-SYU2        NOT =   ZERO  )
              AND (WRK-COM-YOBI1 (IDX-Z)(5:1)
                                   NOT =   ZERO  )
               PERFORM 10052-KASAN-CHK05-SEC
           END-IF
      *
      *    通則加算は通則加算対象外とする（告示１で判定）
           IF      (SPA-ERRCD                  =   SPACE )  AND
                   (WRK-COM-TENSIKIBETU (IDX-Z)    =   3    )  AND
      *********    (WRK-COM-KOKUJISKBKBN2(IDX-Z)   =   "9"  )  AND
                   (WRK-COM-KOKUJISKBKBN1(IDX-Z)   =   "9"  )  AND
                   (WRK-COM-TUSOKUGAIKBN (IDX-Z)   =   "0" )
               MOVE    "1"                 TO  WRK-COM-TUSOKUGAIKBN
                                                               (IDX-Z)
           END-IF
      *
           .
       1005-KASAN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    脊髄誘発電位測定加算チェック処理
      *****************************************************************
       10052-KASAN-CHK05-SEC                  SECTION.
      *
      *    手技との判定
           EVALUATE    WRK-COM-YOBI1(IDX-SYU2)(5:1)
               WHEN    "1"
                   IF      WRK-COM-YOBI1(IDX-Z)(5:1)   =   "1"
                       CONTINUE
                   ELSE
                       MOVE    "0513"          TO  SPA-ERRCD
                   END-IF
               WHEN    "2"
                   IF       WRK-COM-YOBI1(IDX-Z)(5:1)  =   "2"
                       CONTINUE
                   ELSE
                       MOVE    "0513"          TO  SPA-ERRCD
                   END-IF
               WHEN    "0"
                   MOVE    "0514"          TO  SPA-ERRCD
           END-EVALUATE
           IF      SPA-ERRCD           NOT =   SPACE
               MOVE    "1"             TO  WRK-COM-CUR(IDX-Z)
           END-IF
      *
           .
        10052-KASAN-CHK05-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数計算編集処理
      *****************************************************************
       1002-TENSU-KEI-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDX-MAX
           MOVE    ZERO                TO  IDX-ZS
           MOVE    ZERO                TO  IDX-SANSO
           MOVE    ZERO                TO  WRK-GRSI-TENSUKEI
           MOVE    ZERO                TO  WRK-RSI-TENSUKEI2
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  ORCSC90AREA
           MOVE    ZERO                TO  WRK-SUM-AREA
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE     ) OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *
               ADD     1                   TO  IDX-MAX
      *
               MOVE    SPACE               TO  WRK-GMN-SURYO (IDX-Z)
               MOVE    SPACE               TO  WRK-GMN-TENSU (IDX-Z)
               MOVE    SPACE               TO  WRK-GMN-KAISU (IDX-Z)
               MOVE    ZERO                TO  WRK-GMN-KEI   (IDX-Z)
      *
      *        手技毎
               INITIALIZE                   WRK-COM-PLUS-AREA(IDX-Z)
      *
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1)     =   "."
                                                       OR "*"
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
                   WHEN    "*"
                       CONTINUE
      *
                   WHEN    OTHER
      *                編集チェックへ
                       PERFORM 10021-KEISAN-SEC
               END-EVALUATE
           END-PERFORM
      *
      *    最終行
      *    手技料の計算
           IF      IDX-ZS              >   ZERO
               MOVE    2               TO  FLG-SYUGI
               COMPUTE IDX-Z           =   IDX-MAX    +  1
               PERFORM 100212-TENSUKEI-SEC
           END-IF
      *    合計点数計算・編集（１剤毎の合計点数のみとする）
           COMPUTE LNK-SC90-TENSU      =   WRK-GOK-TENSUKEI
                                       +   WRK-GOK-TENGAKEI
                                       +   WRK-GRSI-TENSUKEI
      *
           MOVE    "2"                 TO  LNK-SC90-KBN
           MOVE    ZERO                TO  LNK-SC90-IDX
           MOVE    1                   TO  LNK-SC90-IDX-STR
           MOVE    IDX-MAX             TO  LNK-SC90-IDX-END
           CALL    "ORCSC90TEN2"       USING
                                       ORCSC90AREA
                                       WRK-SCR-REC
      *
      ***  COMPUTE LNK-SC90-TENSUKEI   =   LNK-SC90-TENSUKEI
      *                                +   LNK-SC90-TENGAKEI
      *                                +   WRK-GRSI-TENSUKEI
      *    各点数編集
      *    MOVE    LNK-SC90-TENSUKEI   TO  WRK-COM-SYUTEN1 (IDX-MAX)
      *    COMPUTE WRK-COM-YKZTEN (IDX-MAX)
      *                                =   LNK-SC90-ZAITEN
      *                                +   LNK-SC90-GAITEN
      *    MOVE    LNK-SC90-KIZTEN     TO  WRK-COM-KIZAITEN (IDX-MAX)
      *
      *    点数計　計算
      *    COMPUTE WRK-COM-TENSU (IDX-MAX) =   LNK-SC90-TENSUKEI
      *                                    +   LNK-SC90-ZAITEN
      *                                    +   LNK-SC90-GAITEN
      *                                    +   LNK-SC90-KIZTEN
      *    計
           COMPUTE WRK-COM-KEI (IDX-MAX)   =   WRK-COM-TENSU  (IDX-MAX)
                                           *   LNK-ORCSC-KAISU
      *
      *    セット用合計点数
           MOVE    WRK-COM-KEI  (IDX-MAX)  TO  LNK-ORCSC-ZAIKEI
           MOVE    WRK-COM-TENSU(IDX-MAX)  TO  LNK-ORCSC-ZAITEN
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
      *    手技料判定
           IF      (WRK-MOJI                   =   "1"  )  AND
                   (WRK-COM-DATAKBN (IDX-Z)    =   "1"  )
               IF      IDX-ZS              >   ZERO
      *            手技料の計算
                   MOVE    1               TO  FLG-SYUGI
                   PERFORM 100212-TENSUKEI-SEC
               END-IF
               MOVE    IDX-Z               TO  IDX-ZS
           END-IF
      *
      *    手技料集計
           IF       WRK-MOJI                   =   "1"
               PERFORM 100212-SYUGI-KEI-SEC
           END-IF
      *    回数
           MOVE    LNK-ORCSC-KAISU     TO  WRK-COM-KAISU (IDX-Z)
      *
           .
       10021-KEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    手技料計算処理
      *****************************************************************
       100212-SYUGI-KEI-SEC            SECTION.
      *    点数
      *        ％加算・減算は集計しない
           IF      WRK-COM-TENSIKIBETU (IDX-Z)   =   5
                                                 OR  6
               GO      TO      100212-SYUGI-KEI-EXT
           END-IF
      *H25.5
      *    自己血輸血（６歳未満）対応
           IF     (WRK-COM-INPUTCD (IDX-Z) =   CONS-JIKOYK1
                                           OR  CONS-JIKOYK2
                                           OR  CONS-JIKOYK3
                                           OR  CONS-JIKOYK4)
               PERFORM 1002122-JIKOYUKETU-KEI-SEC
           END-IF
      *
      *    労災の手技の時、
           IF      WRK-COM-INPUTCD(IDX-Z)(1:3) =   "101"
               IF      WRK-COM-INPUTCD(IDX-ZS) =   "101400030"
                                               OR  "101500150"
      *        初診時ブラッシング料は時間外加算のみ
                   ADD     WRK-COM-KISOTEN (IDX-Z) TO  WRK-RSI-TENSUKEI2
               ELSE
                   ADD     WRK-COM-KISOTEN (IDX-Z) TO  WRK-RSI-TENSUKEI
               END-IF
           ELSE
      *        人工心肺不使用加算は施設基準逓減の対象外とする
               IF      WRK-COM-INPUTCD (IDX-Z) =   "150302770"
                                               OR  "150302870"
                   ADD     WRK-COM-KISOTEN (IDX-Z) TO  WRK-JIN-TENSUKEI
                   GO      TO      100212-SYUGI-KEI-EXT
               END-IF
      *        通則加算対象外区分判定
               IF      WRK-COM-TUSOKUGAIKBN (IDX-Z)  =   "1"
                   ADD     WRK-COM-KISOTEN (IDX-Z) TO  WRK-SUM-TENGAKEI
               ELSE
                   ADD     WRK-COM-KISOTEN(IDX-Z)  TO  WRK-SUM-TENSUKEI
               END-IF
           END-IF
      *
           .
       100212-SYUGI-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自己血輸血（６歳未満）の計算処理
      *****************************************************************
       1002122-JIKOYUKETU-KEI-SEC             SECTION.
      *
      *    手技の下の体重コメント
           MOVE    ZERO                TO  WRK-TAIJYU
           PERFORM VARYING     IDXS    FROM    IDX-Z   BY  1
                       UNTIL      (IDXS        >   SPA-MAX-LINE )
                            OR    (WRK-COM-INPUTCD (IDXS)  =   SPACE)
               IF      WRK-COM-INPUTCD(IDXS)   =   CONS-JIKOCOM
      *            体重決定
                   INITIALIZE                      ORCSNUMAREA
                   MOVE    WRK-COM-ATAI1 (IDXS)    TO  SNUM-INX
                   CALL    "ORCSNUM"           USING
                                               ORCSNUMAREA
                   IF     (SNUM-RC             =   ZERO  )
                       MOVE    SNUM-OUTNUM         TO  WRK-TAIJYU
                   END-IF
                   MOVE    SPA-MAX-LINE        TO  IDXS
               END-IF
           END-PERFORM
      *
           IF      WRK-TAIJYU          =   ZERO
               MOVE    ZERO            TO  WRK-TAIJYU-2
           ELSE
               COMPUTE WRK-KEISAN      =  (WRK-TAIJYU   /   1000 )
                                       +   0.05
               MOVE    WRK-KEISAN      TO  WRK-TAIJYU-2
           END-IF
           MOVE    WRK-COM-SURYO (IDX-Z)   TO  WRK-SURYO
           MOVE    ZERO                    TO  WRK-KEI-TEN2
      *    体重×下限値
           COMPUTE WRK-KZMKGN          =   WRK-TAIJYU-2
                                       *   WRK-COM-KZMKGN(IDX-Z)
      *    体重×上限値
           IF      WRK-COM-KZMJGN(IDX-Z)   =    99999999
               MOVE    WRK-COM-KZMJGN(IDX-Z)    TO  WRK-KZMJGN
           ELSE
               COMPUTE WRK-KZMJGN          =   WRK-TAIJYU-2
                                           *   WRK-COM-KZMJGN(IDX-Z)
           END-IF
      *    体重×きざみ値
           COMPUTE WRK-KZM             =   WRK-TAIJYU-2
                                       *   WRK-COM-KZM (IDX-Z)
      *    きざみ値判定
           EVALUATE    TRUE
      *        輸血量≦（体重×下限値−体重×きざみ値）
               WHEN    WRK-SURYO       <=   (WRK-KZMKGN
                                        -    WRK-KZM     )
                   IF      WRK-COM-KZMERR (IDX-Z)  =   2  OR  3
                       MOVE    "0021"          TO  SPA-ERRCD
                   ELSE
                       MOVE    WRK-COM-TEN (IDX-Z) TO  WRK-KEI-TEN2
                   END-IF
      *      （体重×下限値−体重×きざみ値）＜輸血量≦（体重×下限値）
               WHEN   ((WRK-KZMKGN   -   WRK-KZM )  <  WRK-SURYO )  AND
                      ( WRK-SURYO                   <= WRK-KZMKGN)
                   MOVE    WRK-COM-TEN (IDX-Z)     TO  WRK-KEI-TEN2
      *
      *       （体重×下限値）＜輸血量≦（体重×上限値）
               WHEN     (WRK-KZMKGN            <   WRK-SURYO )   AND
                        (WRK-KZMJGN            >=  WRK-SURYO )
      *            点数計算式１
                   COMPUTE WRK-KEI-TEN1    =  (WRK-SURYO
                                           -   WRK-KZMKGN)
                                           /   WRK-KZM
                                           +   0.9999
                   MOVE    WRK-KEI-TEN1    TO  WRK-KEI-TEN2
                   COMPUTE WRK-KEI-TEN     =   WRK-COM-TEN (IDX-Z)
                                           + ( WRK-KEI-TEN2
                                           *   WRK-COM-KZMTEN (IDX-Z))
                   COMPUTE WRK-KEI-TEN2    =   WRK-KEI-TEN    +  0.5
      *
      *            （体重×上限値）＜データ
               WHEN    WRK-KZMJGN          <   WRK-SURYO
                   IF      WRK-COM-KZMERR (IDX-T)  =   0  OR  2
      *                点数計算式１
                       COMPUTE WRK-KEI-TEN1    =  (WRK-SURYO
                                               -   WRK-KZMKGN)
                                               /   WRK-KZM
                                               +   0.9999
                       MOVE    WRK-KEI-TEN1    TO  WRK-KEI-TEN2
                       COMPUTE WRK-KEI-TEN     =   WRK-COM-TEN (IDX-Z)
                                               + ( WRK-KEI-TEN2
                                               *   WRK-COM-KZMTEN
                                                               (IDX-Z))
                       COMPUTE WRK-KEI-TEN2    =   WRK-KEI-TEN    +  0.5
                   ELSE
      *            点数計算式２
                       COMPUTE WRK-KEI-TEN1    =  (WRK-KZMJGN
                                               -   WRK-KZMKGN)
                                               /   WRK-COM-KZM  (IDX-Z)
                                               +   0.9999
                       MOVE    WRK-KEI-TEN1    TO  WRK-KEI-TEN2
                       COMPUTE WRK-KEI-TEN     =   WRK-COM-TEN (IDX-Z)
                                               + ( WRK-KEI-TEN2
                                               *   WRK-COM-KZMTEN
                                                               (IDX-Z))
                       COMPUTE WRK-KEI-TEN2    =   WRK-KEI-TEN  +  0.5
                   END-IF
           END-EVALUATE
      *
           IF     (SPA-ERRCD           =   SPACE )
               MOVE    WRK-KEI-TEN2    TO  WRK-COM-KISOTEN  (IDX-Z)
           END-IF
           .
       1002122-JIKOYUKETU-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    手技料の計算処理
      *****************************************************************
       100212-TENSUKEI-SEC             SECTION.
      *
      *    集計済点数編集
      *!   MOVE    WRK-SUM-TENGAKEI    TO  LNK-SC90-TENGAKEI
      *!   MOVE    WRK-SUM-TENSUKEI    TO  LNK-SC90-TENSUKEI
      *
           MOVE    IDX-Z               TO  HZN-IDX
           PERFORM 1006-KASAN-SET-SEC
      *    手技料の合計を編集する
           MOVE    WRK-SUM-TENSUKEI    TO  WRK-COM-SYUTEN2 (IDX-ZS)
      *
      *    労災の外来管理加算読み替え処理
           IF      SPA-HKNKBN          =   1
      *D     AND  (SPA-RSI-HKNKBN  NOT =   "5" )
             AND  (SPA-RSI-JUNKYO  NOT =   "1" )
               IF     (WRK-COM-RSIGIRI (IDX-ZS)
                                           =   1  )  AND
                      (WRK-COM-SYUTEN2 (IDX-ZS)
                                           <   LNK-ORCSC-GAIRAI-TEN)
                   MOVE     LNK-ORCSC-GAIRAI-TEN
                                           TO  WRK-SUM-TENSUKEI
               END-IF
           END-IF
      *
      *    人工心肺不使用加算を通則分に加算する
           COMPUTE WRK-SUM-TENSUKEI    =   WRK-SUM-TENSUKEI
                                       +   WRK-JIN-TENSUKEI
      *    ２以上の手術の逓減計算
      *****PERFORM 1008-HEIYOU-KEI-SEC
      *
           ADD     WRK-SUM-TENSUKEI    TO  WRK-GOK-TENSUKEI
           ADD     WRK-SUM-TENGAKEI    TO  WRK-GOK-TENGAKEI
           ADD     WRK-RSI-TENSUKEI    TO  WRK-GRSI-TENSUKEI
      *
      ***  MOVE    ZERO                TO  LNK-SC90-TENSUKEI
      ***  MOVE    ZERO                TO  LNK-SC90-TENGAKEI
           MOVE    ZERO                TO  WRK-SUM-TENSUKEI
           MOVE    ZERO                TO  WRK-SUM-TENGAKEI
           MOVE    ZERO                TO  WRK-RSI-TENSUKEI
           MOVE    ZERO                TO  WRK-JIN-TENSUKEI
      *
      *    最後の手技料時に、以下の計算を行う
           IF      FLG-SYUGI           =   2
               PERFORM 1007-KASAN-SET-ALL-SEC
           END-IF
      *
           .
       100212-TENSUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    割合加算計算処理
      *****************************************************************
       1006-KASAN-SET-SEC              SECTION.
      *    自動加算以外の加算（時間外、年齢以外）
           MOVE    ZERO                TO  WRK-TENSUKEI-G
           MOVE    ZERO                TO  WRK-TENSUKEI-K
           MOVE    ZERO                TO  WRK-TENGAKEI-G
           MOVE    ZERO                TO  WRK-TENGAKEI-K
      *
           MOVE    ZERO                TO  WRK-TENGAKEI-G2
           MOVE    ZERO                TO  WRK-TENSUKEI-G2
           MOVE    ZERO                TO  WRK-GENGAKU-2
      *
           PERFORM VARYING     IDX     FROM    IDX-ZS  BY  1
                   UNTIL       IDX         >=  HZN-IDX
      *        時間外加算のものは、自動加算とする
               IF     (WRK-COM-DATAKBN    (IDX)        =   "2"  )  AND
                      (WRK-COM-TIMEKSNKBN (IDX)    NOT =   ZERO )  AND
                      (WRK-COM-TENSIKIBETU(IDX)        =   5    )
                   MOVE    1               TO  WRK-COM-SANFLG (IDX)
               END-IF
      *施設基準逓減廃止により、手技料に逓減
      *        ５０％減算は最初にする
               IF     (WRK-COM-DATAKBN    (IDX)    =   "2"  )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   6    )  AND
                      (WRK-COM-INPUTCD    (IDX)    =   "150268970")
                   MOVE    5               TO  WRK-COM-SANFLG (IDX)
                   COMPUTE WRK-TENSUKEI        =   WRK-COM-KISOTEN
                                                           (IDX-ZS)
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                  MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                  MOVE    WRK-TENSUKEI-9      TO  WRK-GENGAKU-2
      *           通則加算対象外区分判定
                  IF      WRK-COM-TUSOKUGAIKBN (IDX-ZS)  =   "1"
                       MOVE    WRK-TENSUKEI-9  TO  WRK-TENGAKEI-G2
                   ELSE
                       MOVE    WRK-TENSUKEI-9  TO  WRK-TENSUKEI-G2
                   END-IF
               END-IF
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    IDX-ZS  BY  1
                   UNTIL       IDX         >=  HZN-IDX
      *        診察料、加算、労災、時間外・乳幼児以外を対象とする
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   NOT =   "1"  )  OR
                      (WRK-COM-DATAKBN    (IDX)        =   "1"  )  OR
                      (WRK-COM-INPUTCD(IDX)(1:3)       =   "101")  OR
                      (WRK-COM-SANFLG     (IDX)    NOT =   ZERO )
                   CONTINUE
               ELSE
      *
      *        手技料の点数を所定点数とする
                   MOVE    ZERO                TO  WRK-TENSUKEI
      *        ％加算
      *             （２以上の逓減を減算後の点数に加算
                   IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                          (WRK-COM-TENSIKIBETU(IDX)    =   5   )  AND
                          (WRK-COM-SANFLG     (IDX)    =   ZERO)
                       COMPUTE WRK-TENSUKEI    =  (WRK-COM-KISOTEN
                                                               (IDX-ZS) 
                                               -   WRK-GENGAKU-2  )
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.5
                       MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
      *廃止            人工心肺不使用加算は施設基準逓減の対象外とする
      *                 IF      WRK-COM-INPUTCD (IDX) =   "150301970"
      *                     ADD     WRK-TENSUKEI-9  TO  WRK-JIN-TENSUKEI
      *                 ELSE
      *
      *                通則加算対象外区分判定
                       IF      WRK-COM-TUSOKUGAIKBN (IDX-ZS)  =   "1"
                           ADD     WRK-TENSUKEI-9  TO  WRK-TENGAKEI-K
                       ELSE
                           ADD     WRK-TENSUKEI-9  TO  WRK-TENSUKEI-K
                       END-IF
      *
      ***********      END-IF
                   END-IF
      *            ％減算
                   IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                          (WRK-COM-TENSIKIBETU(IDX)    =   6   )  AND
                          (WRK-COM-SANFLG     (IDX)    =   ZERO) 
                       COMPUTE WRK-TENSUKEI    =  (WRK-COM-KISOTEN
                                                               (IDX-ZS)
                                               -   WRK-GENGAKU-2  )
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                       MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
      *                通則加算対象外区分判定
                       IF      WRK-COM-TUSOKUGAIKBN (IDX-ZS)  =   "1"
                           ADD     WRK-TENSUKEI-9  TO  WRK-TENGAKEI-G
                       ELSE
                           ADD     WRK-TENSUKEI-9  TO  WRK-TENSUKEI-G
                       END-IF
                   END-IF
               END-IF
      *H27.4
      *        施設基準逓減は、直前の手技料
               IF     (WRK-COM-DATAKBN    (IDX)    =   "2"  )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   6    )  AND
                      (WRK-COM-SANFLG     (IDX)    =   2    )
                   COMPUTE WRK-TENSUKEI       =    WRK-COM-KISOTEN
                                                               (IDX-ZS) 
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
      *            他の注加算の対象外とする
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENGAKEI-G
               END-IF
      *
           END-PERFORM
           IF      WRK-COM-INPUTCD(IDX-ZS)(1:3)    =   "101"
      *        労災コードの時、加算のみ算定
               COMPUTE WRK-RSI-TENSUKEI    =   WRK-RSI-TENSUKEI
                                           +   WRK-TENSUKEI-K
                                           -   WRK-TENSUKEI-G2
           ELSE
               COMPUTE WRK-SUM-TENSUKEI    =   WRK-SUM-TENSUKEI
                                           +   WRK-TENSUKEI-K
                                           -   WRK-TENSUKEI-G
                                           -   WRK-TENSUKEI-G2
               COMPUTE WRK-SUM-TENGAKEI    =   WRK-SUM-TENGAKEI
                                           +   WRK-TENGAKEI-K
                                           -   WRK-TENGAKEI-G
                                           -   WRK-TENGAKEI-G2
           END-IF
      *
           .
       1006-KASAN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    割合加算計算処理（全体）
      *****************************************************************
       1007-KASAN-SET-ALL-SEC              SECTION.
      *
      *!!  MOVE    WRK-GOK-TENSUKEI    TO  LNK-SC90-TENSUKEI
      *!!  MOVE    WRK-GOK-TENGAKEI    TO  LNK-SC90-TENGAKEI
      *
      *    自動加算の加算（時間外、年齢）
           MOVE    ZERO                TO  WRK-TENSUKEI-K
           MOVE    ZERO                TO  WRK-TENSUKEI-G
      *
           PERFORM VARYING     IDX     FROM    1       BY  1
                   UNTIL       IDX         >=  HZN-IDX
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-INPUTCD(IDX)(1:3)   NOT =   "101" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   5   )  AND
                      (WRK-COM-SANFLG     (IDX)    =   1   )
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   COMPUTE WRK-TENSUKEI        =   WRK-GOK-TENSUKEI
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.5
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-K
               END-IF
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-INPUTCD(IDX)(1:3)   NOT =   "101" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   6   )  AND
                      (WRK-COM-SANFLG     (IDX)    =   1   )
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   COMPUTE WRK-TENSUKEI        =   WRK-GOK-TENSUKEI
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.5
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-G
               END-IF
           END-PERFORM
      *
           COMPUTE WRK-GOK-TENSUKEI    =   WRK-GOK-TENSUKEI
                                       +   WRK-TENSUKEI-K
                                       -   WRK-TENSUKEI-G
      *
      *    労災の加算
           IF      SPA-HKNKBN          =   1
      *****  AND  (SPA-RSI-HKNKBN  NOT =   "5" )
             AND  (SPA-RSI-JUNKYO  NOT =   "1" )
               CONTINUE
           ELSE
               GO      TO      1007-KASAN-SET-ALL-EXT
           END-IF
      *    労災コードの加算（時間外、年齢）
           MOVE    ZERO                TO  WRK-TENSUKEI-K
           MOVE    ZERO                TO  WRK-TENSUKEI-G
      *
           PERFORM VARYING     IDX     FROM    1       BY  1
                   UNTIL       IDX         >=  HZN-IDX
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-INPUTCD(IDX)(1:3)   NOT =   "101" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   5   )  AND
                      (WRK-COM-SANFLG     (IDX)    =   1   )
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   COMPUTE WRK-TENSUKEI        =   WRK-GRSI-TENSUKEI
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.5
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-K
               END-IF
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-INPUTCD(IDX)(1:3)   NOT =   "101" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   6   )  AND
                      (WRK-COM-SANFLG     (IDX)    =   1   )
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   COMPUTE WRK-TENSUKEI        =   WRK-GRSI-TENSUKEI
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.5
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-G
               END-IF
      *        初診時ブラッシング料の時間外加算
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-INPUTCD(IDX)(1:3)   NOT =   "101")  AND
                      (WRK-COM-TIMEKSNKBN (IDX)    NOT =   ZERO )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   5   )  AND
                      (WRK-COM-SANFLG     (IDX)    =   1   )  AND
                      (WRK-RSI-TENSUKEI2           >   ZERO)
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   COMPUTE WRK-TENSUKEI        =   WRK-RSI-TENSUKEI2
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.5
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-K
               END-IF
           END-PERFORM
           COMPUTE WRK-GRSI-TENSUKEI   =   WRK-GRSI-TENSUKEI
                                       +   WRK-TENSUKEI-K
                                       -   WRK-TENSUKEI-G
                                       +   WRK-RSI-TENSUKEI2
      *
      *    労災加算
           MOVE    ZERO                TO  WRK-TENSUKEI-K
           PERFORM VARYING     IDX     FROM    1       BY  1
                   UNTIL       IDX         >=  HZN-IDX
               IF     (WRK-COM-INPUTCD(IDX)(1:3)   =   "101" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   5     )
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   COMPUTE WRK-TENSUKEI        =   WRK-GOK-TENSUKEI
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.9
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-K
      *１回のみ
                   MOVE    HZN-IDX             TO  IDX
               END-IF
           END-PERFORM
      *
           COMPUTE WRK-GOK-TENSUKEI    =   WRK-GOK-TENSUKEI
                                       +   WRK-TENSUKEI-K
           .
       1007-KASAN-SET-ALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    全診療行為チェック処理
      *****************************************************************
       200-ALL-SYORI-SEC                SECTION.
      *
           .
       200-ALL-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動算定処理
      *****************************************************************
       300-AUTO-SYORI-SEC                SECTION.
      *
       300-AUTO-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       9001-SYSKANRI-KEY-SEC         SECTION.
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYSKANRI-REC
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    SPACE           TO  SYSKANRI-REC
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    SPACE           TO  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       9001-SYSKANRI-KEY-EXT.
           EXIT.
      *
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
       990-DBCLOSE-SEC           SECTION.
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
