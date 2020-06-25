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
       PROGRAM-ID.                 ORCSTENTEK.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院会計照会
      *  コンポーネント名  : 入院用点滴集計処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/01/10    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  03/03/20  薬剤点数計算修正
      *  01.00.02    NACL−多々納 03/04/02  中心静脈自動発生追加
      *  01.00.03    NACL−多々納 03/05/21  廃棄数量修正
      *  01.00.04    NACL−多々納 03/10/16  輸血判定追加
      *  01.00.05    NACL−多々納 05/05/13  332点滴（手術以外）
      *                                     352中心静脈（手術以外）を追加
      *  01.00.06    NACL-多々納  05/11/25  MONFUNC 対応
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *  04.01.00    NACL-多々納  07/10/02  薬剤コメント編集対応
      *  04.05.00    NACL-多々納  09/06/XX  数量小数５桁対応
      *  04.05.00    NACL-多々納  09/11/30  日毎の表示順対応
      *  04.07.00    NACL-多々納  11/11/XX  同日再入院対応
      *  04.07.00    NACL-多々納  14/10/16  診療科編集対応
      *  04.08.00    NACL-多々納  16/06/XX  中心静脈（植入型カテーテル）対応
      *  05.00.00    NACL-多々納  19/03/XX  点数識別区分＝4,5と桁数対応
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
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
           03  FLG-END             PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-TENSUPLUS       PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
      *
           03  FLG-ZAIARI          PIC 9(01).
           03  FLG-SEIBUTU         PIC 9(01).
           03  FLG-SEIBUTU-OK      PIC 9(01).
           03  FLG-MADOKU          PIC 9(01).
           03  FLG-MADOKU-OK       PIC 9(01).
           03  FLG-CHU-ARI         PIC 9(01).
           03  FLG-HAIKI           PIC 9(01).
           03  FLG-TENARI          PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-TENTEKI         PIC 9(01).
      *    輸血の有無
           03  FLG-YUKETU          PIC 9(01).
      *    薬剤の有無
           03  FLG-YAKU-ARI        PIC 9(01).
           03  FLG-COMMENT          PIC 9(01).
           03  FLG-COMMENT-OK      PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
           03  IDXA                PIC 9(04).
           03  IDXA2               PIC 9(04).
           03  IDXS                PIC 9(04).
           03  IDX-T2              PIC 9(04).
           03  IDX-T3              PIC 9(04).
      *
           03  IDX-T               PIC 9(04).
           03  IDX-H               PIC 9(04).
           03  IDX-D               PIC 9(02).
           03  IDY2                PIC 9(02).
      *
           03  IDX-YAK             PIC 9(04).
           03  IDX-C1              PIC 9(04).
           03  IDX-C               PIC 9(04).
      *
           03  IDX-ZAI             PIC 9(04).
           03  IDX-DAY             PIC 9(04).
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
      *
           03  WRK-DD              PIC 9(02).
      *
      **   03  WRK-TENSU           PIC 9(07)V9(04).
           03  WRK-TENSU           PIC 9(11)V9(04).
           03  WRK-SURYO-9         PIC 9(07).
           03  WRK-SURYO           PIC 9(07)V9(05).
           03  WRK-HAIKI-SURYO     PIC 9(07)V9(05).
           03  WRK-EKIRYOU         PIC 9(07)V9(05).
      *
      **   03  WRK-TENSU-KEI       PIC 9(07)V9(06).
      **   03  WRK-SYOKEIS2        PIC 9(08)V9(06).
      **   03  WRK-TENSU-GOK       PIC 9(08)V9(05).
           03  WRK-TENSU-KEI       PIC 9(11)V9(06).
           03  WRK-SYOKEIS2        PIC 9(11)V9(06).
           03  WRK-TENSU-GOK       PIC 9(11)V9(05).
      *
           03  WRK-SRYCD               PIC X(09).
           03  WRK-MAX-RYOU            PIC 9(07).
      *
           03  WRK-SRYKA               PIC X(02).
           03  WRK-HKNCOMBI            PIC X(04).
           03  WRK-ZAINUM              PIC 9(08).
      *
           03  WRK-BYMD.
               05  WRK-BYY             PIC 9(04).
               05  WRK-BMM             PIC 9(02).
               05  WRK-BDD             PIC 9(02).
      *
           03  WRK-COMMENT             PIC X(80).
      *ver.4.7
           03  IDXD                    PIC 9(04).
           03  WRK-DAY                 PIC 9(02).
           03  WRK-DOU-SURYO           PIC 9(07)V9(05).
      **   03  WRK-DOU-TENSU-GOK       PIC 9(08)V9(05)  OCCURS  2.
           03  WRK-DOU-TENSU-GOK       PIC 9(11)V9(05)  OCCURS  2.
      *
      *    液量集計用
       01  TBL-AREA.
           03  TBL-AREA-OCC            OCCURS   100.
               05  TBL-SRYCD           PIC X(09).
      **       05  TBL-TEN             PIC 9(07)V9(5).
               05  TBL-TEN             PIC 9(09)V9(5).
               05  TBL-CSYYOURYO       PIC 9(07)V9(5).
      *
               05  TBL-SEIBUTU         PIC 9(01).
               05  TBL-MADOKU          PIC 9(01).
               05  TBL-DAY-AREA        OCCURS   31.
                   07  TBL-DAY             PIC 9(03).
                   07  TBL-SURYO           PIC 9(05)V9(05).
                   07  TBL-CHUSYARYO       PIC 9(07)V9(05).
      *            保険組合せ
                   07  TBL-HKNCOMBI        PIC X(04).
                   07  TBL-SRYKA           PIC X(02).
      *            廃棄フラグ
                   07  TBL-HAIKI           PIC 9(01).
                   07  TBL-HAIKI-SURYO     PIC 9(05)V9(05).
      *            同日再入院別
                   07  TBL-SURYO-D         PIC 9(05)V9(05)
                                               OCCURS   2.
      *        薬剤のみ(331)
               05  TBL-DAY-AREA2       OCCURS   31.
                   07  TBL-DAY2            PIC 9(03).
                   07  TBL-SURYO2          PIC 9(05)V9(05).
      *H26.10      診療科
                   07  TBL-SRYKA2          PIC X(02).
      *            廃棄フラグ
                   07  TBL-HAIKI2          PIC 9(01).
                   07  TBL-HAIKI-SURYO2    PIC 9(05)V9(05).
                   07  TBL-SURYO2-D        PIC 9(05)V9(05)
                                               OCCURS   2.
      *        手術以外(332)
               05  TBL-DAY-AREA4       OCCURS   31.
                   07  TBL-DAY4            PIC 9(03).
                   07  TBL-SURYO4          PIC 9(05)V9(05).
                   07  TBL-CHUSYARYO4      PIC 9(07)V9(05).
      *            廃棄フラグ
                   07  TBL-HAIKI4          PIC 9(01).
                   07  TBL-HAIKI-SURYO4    PIC 9(05)V9(05).
      *            保険組合せ
                   07  TBL-HKNCOMBI4       PIC X(04).
                   07  TBL-SRYKA4          PIC X(02).
                   07  TBL-SURYO4-D        PIC 9(05)V9(05)
                                               OCCURS   2.
      *        中心静脈分
               05  TBL-DAY-AREA3       OCCURS   31.
                   07  TBL-DAY3            PIC 9(03).
                   07  TBL-SURYO3          PIC 9(05)V9(05).
      *            廃棄フラグ
                   07  TBL-HAIKI3          PIC 9(01).
                   07  TBL-HAIKI-SURYO3    PIC 9(05)V9(05).
      *            保険組合せ
                   07  TBL-HKNCOMBI3       PIC X(04).
                   07  TBL-SRYKA3          PIC X(02).
                   07  TBL-SURYO3-D        PIC 9(05)V9(05)
                                               OCCURS   2.
      *        中心静脈分（手術以外）(352)
               05  TBL-DAY-AREA5       OCCURS   31.
                   07  TBL-DAY5            PIC 9(03).
                   07  TBL-SURYO5          PIC 9(05)V9(05).
      *            廃棄フラグ
                   07  TBL-HAIKI5          PIC 9(01).
                   07  TBL-HAIKI-SURYO5    PIC 9(05)V9(05).
      *            保険組合せ
                   07  TBL-HKNCOMBI5       PIC X(04).
                   07  TBL-SRYKA5          PIC X(02).
      *
                   07  TBL-SURYO5-D        PIC 9(05)V9(05)
                                               OCCURS   2.
      *        薬剤コメント
               05  TBL-COM-AREA        OCCURS   31.
                   07  TBL-COMMNT-OCCURS   OCCURS   5.
                       09  TBL-COM-DAY         PIC 9(01).
                       09  TBL-COM-SRYCD       PIC X(09).
                       09  TBL-COM-COMMENT     PIC X(80).
      *
      *    器材集計用
       01  TBL-KIZAI-AREA.
           03  TBL-KIZAI-AREA-OCC          OCCURS   10.
               05  TBL-KIZ-SRYCD           PIC X(09).
               05  TBL-KIZ-SURYO           PIC 9(05)V9(05).
               05  TBL-KIZ-CHK             PIC 9(01).
      *        器材コメント
               05  TBL-KIZ-COMMNT-AREA         OCCURS   5.
                   07  TBL-KIZ-COM-SRYCD       PIC X(09).
                   07  TBL-KIZ-COM-COMMENT     PIC X(80).
      *
      *    手技料用
       01  TBL2-AREA.
           03  TBL2-AREA-OCC           OCCURS   50.
               05  TBL2-SRYCD           PIC X(09).
               05  TBL2-DATAKBN         PIC X(01).
               05  TBL2-SEIBUTU         PIC 9(01).
               05  TBL2-MADOKU          PIC 9(01).
               05  TBL2-IVHFLG          PIC 9(01).
               05  TBL2-DAY-AREA        OCCURS   31.
                   07  TBL2-DAY             PIC 9(01).
                   07  TBL2-ZAINUM          PIC 9(08).
                   07  TBL2-YAKU-ARI        PIC 9(01).
      *            診療科
                   07  TBL2-SRYKA           PIC X(02).
      *            保険組合せ
                   07  TBL2-HKNCOMBI        PIC X(04).
      *
      *    変更テーブル
       01  TBL-HEN-AREA.
           03  TBL-HEN-OCC                 OCCURS  50.
               05  TBL-HEN-ZAINUM          PIC 9(08).
               05  TBL-HEN-SRYKA           PIC X(02).
               05  TBL-HEN-HKNCOMBI        PIC X(04).
      *        削除
               05  TBL-HEN-DEL             PIC 9(01)
                                           OCCURS  31.
      *    点滴液量
       01  TBL-CHUSYARYO2-AREA.
           03  TBL-CHUSYARYO2-TBL      OCCURS   31.
      *        330
               05  TBL-CHUSYARYO-2      PIC 9(07)V9(05).
      *        332
               05  TBL-CHUSYARYO4-2     PIC 9(07)V9(05).
      *        350
               05  TBL-SURYO3-2         PIC 9(07)V9(05).
      *        352
               05  TBL-SURYO5-2         PIC 9(07)V9(05).
      *
      *    算定不可条件
       01  TBL-CHK-AREA.
      *    手術有無
           03  TBL-SYU-ARI         PIC 9(01)
                                       OCCURS   31.
           03  TBL-SYU-ARI2        PIC 9(01)
                                       OCCURS   31.
      *    中心静脈有無
           03  TBL-CHU-TBL             OCCURS   31.
               05  TBL-CHU-ARI             PIC 9(01).
      *    動脈有無
           03  TBL-DOU-TBL             OCCURS   31.
               05  TBL-DOU-ARI         PIC 9(01).
      *H28.6
      *    中心静脈（植入型カテーテル）有無
           03  TBL-CHU6-TBL             OCCURS   31.
               05  TBL-CHU6-ARI         PIC 9(01).
      *
      *    当日分
       01  TBL-SYUKEI-AREA.
           03  TBL-SYUKEI-OCC          OCCURS  31.
               05  TBL-S-SEIBUTU       PIC 9(01).
               05  TBL-S-MADOKU        PIC 9(01).
      *!!
      *    日毎の順
       01  TBL-ZAIDAY-AREA.
           03  TBL-ZAIDAY-OCC             OCCURS  31.
               05  TBL-ZAIDAY-OCC2            OCCURS  50.
                   07  TBL-D-SRYCD         PIC X(09).
      *
       01  TBL-ZAI-AREA.
           03  TBL-ZAI-OCC             OCCURS  50.
               05  TBL-ZAI-SRYCD           PIC X(09).
      *
      *    受診履歴
       01  TBL-JYURRK-AREA.
           02  TBL-JYURRK-REC         OCCURS  15.
           COPY    "CPJYURRK.INC"     REPLACING
                                     //JYURRK-// BY //TBL-JYURRK-//.
      *
      *    診療行為マスター
       01  TBL-SRYACT-AREA.
           02  TBL-SRYACT-REC         OCCURS  10.
           COPY    "CPSRYACT.INC"     REPLACING
                                     //SRY-// BY //TBL-SRY-//.
      *
      *H28.6
      *    中心静脈注射(植入型カテーテル）
       01  CON-CHUJYO6             PIC X(09)   VALUE   "130008510".
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    診療行為マスター
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
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
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *   算定履歴更新サブ
           COPY    "CPORCSSANTEI.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    診療行為入院更新パラメタ領域
           COPY    "CPORCSTENTEK.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCSTENTEKAREA
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
           INITIALIZE                  TBL-AREA
           INITIALIZE                  TBL2-AREA
           INITIALIZE                  TBL-HEN-AREA
           INITIALIZE                  TBL-CHK-AREA
           INITIALIZE                  TBL-CHUSYARYO2-AREA
           INITIALIZE                  TBL-ZAIDAY-AREA
      *
           INITIALIZE                  ORCSTEN-OUT-AREA
           INITIALIZE                  ORCSTEN-DAY-AREA
           INITIALIZE                  ORCSTEN-ERRCD
           MOVE    ZERO                TO  ORCSTEN-HENFLG
           MOVE    ZERO                TO  ORCSTEN-HENFLG2
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    "1"                 TO  SPA-NYUGAIKBN
      *
      *    診療行為編集処理
           PERFORM 100-HENSYU-SYORI-SEC
      *    点数計算処理
           PERFORM 200-KEISAN-SYORI-SEC
      *
           IF      ORCSTEN-KBN         =   "1"  OR  "3"
      *        手技料一括変更処理
               PERFORM 300-KOUSIN-SYORI-SEC
           END-IF
      *
      *TEST
      *    PERFORM VARYING    IDX  FROM   1   BY  1
      *            UNTIL      IDX  >   31
      *       IF  ORCSTEN-ZAITEN(IDX)  >   ZERO
      *          DISPLAY "IDX:"  IDX
      *           ",ORCSTEN-ZAITEN:" ORCSTEN-ZAITEN (IDX)
      *           ",ORCSTEN-CHUSYARYO:" ORCSTEN-CHUSYARYO (IDX)
      *           ",SRYKA:" ORCSTEN-YK-SRYKA (IDX)
      *           ",SRYCD:" ORCSTEN-SRYCD (IDX 1)
      *      END-IF
      *       IF  ORCSTEN-KIZ-DAY(1 IDX)  >   ZERO
      *          DISPLAY "IDX:"  IDX
      *           ",ORCSTEN-KIZ-ZAITEN:" ORCSTEN-KIZ-ZAITEN (IDX)
      *           ",SRYKA:" ORCSTEN-KIZ-SRYKA (1 IDX)
      *           ",SRYCD:" ORCSTEN-KIZ-SRYCD (1 1)
      *      END-IF
      *     END-PERFORM
      *TEST
           . 
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    診療行為編集処理
      *****************************************************************
       100-HENSYU-SYORI-SEC            SECTION.
      *
      *    診療年月の診療会計マスタ読み込み
           MOVE    SPACE               TO  SRYACCT-REC
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    ORCSTEN-PTID        TO  ACCT-PTID
           MOVE    ORCSTEN-SRYYM       TO  ACCT-SRYYM
           MOVE    "33"                TO  ACCT-SRYKBN
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           PERFORM
               UNTIL       (FLG-SRYACCT         =   1       )
               MOVE    ZERO                TO  FLG-ZAIARI
               MOVE    ZERO                TO  WRK-DD
      *        最初の日を保存
               PERFORM VARYING     IDY2    FROM    1   BY  1
                       UNTIL       IDY2    >   31
                   IF      ACCT-DAY (1 IDY2)   >   ZERO
                       MOVE    IDY2                TO  WRK-DD
                       MOVE    1                   TO  FLG-ZAIARI
                   END-IF
               END-PERFORM
               IF      FLG-ZAIARI          =   ZERO
                   MOVE    1                   TO  WRK-DD
                   MOVE    1                   TO  FLG-ZAIARI
               END-IF
      *        保険チェック
               IF     (ORCSTEN-KBN         =   "2"    )  AND
                      (ORCSTEN-HKNCOMBI    NOT =   SPACE )  AND
                      (ACCT-HKNCOMBI   NOT =   ORCSTEN-HKNCOMBI)
                   MOVE    ZERO                TO  FLG-ZAIARI
               END-IF
      *
      *        包括まとめ入力は対象外とする
               IF     (ORCSTEN-HKNCOMBI    NOT =   "9999" )  AND
                      (ACCT-HKNCOMBI           =   "9999" )
                   MOVE    ZERO                TO  FLG-ZAIARI
               END-IF
      *        レセプト編集に包括診療は対象外
               IF     (ORCSTEN-KBN         =   "2"   )
                 AND  (ACCT-JIHOKBN        =   "1"   )
                   MOVE    ZERO                TO  FLG-ZAIARI
               END-IF
      *        薬評は対象外
               IF     (ACCT-ZAIKBN         =   1     )
                   MOVE    ZERO                TO  FLG-ZAIARI
               END-IF
      *
      *        回数があるもの
               IF      FLG-ZAIARI          =   1
      *            診療行為より液量集計
                   PERFORM 1001-SRYACT-HEN-SEC
               END-IF
      *
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       100-HENSYU-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    診療行為マスター編集処理
      *****************************************************************
       1001-SRYACT-HEN-SEC            SECTION.
      *
           INITIALIZE                 TBL-SRYACT-AREA
           MOVE    ZERO               TO  IDXS
      *
      *    診療会計マスターから診療行為マスターを検索する
           MOVE    SPACE               TO  SRYACT-REC
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
               PERFORM 920-SRYACT-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           MOVE    ZERO                TO  FLG-YAKU-ARI
           PERFORM UNTIL      FLG-SRYACT   =   1
      *
               ADD     1                   TO  IDXS
               MOVE    SRYACT-REC          TO  TBL-SRYACT-REC (IDXS)
      *        手技料と薬剤の混在判定
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2        >   5   )  OR
                                  (SRY-SRYCD(IDX2) =   SPACE )
                   IF      SRY-SRYCD(IDX2)(1:1)    =   "6"
                       MOVE    1                   TO  FLG-YAKU-ARI
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SRYACT-NEXT-SEC
           END-PERFORM
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    ZERO                TO  IDX-YAK
           MOVE    ZERO                TO  FLG-COMMENT
           INITIALIZE                      TBL-ZAI-AREA
           MOVE    ZERO                TO  IDX-ZAI
      *    テーブルの診療行為マスターをチェックする
           PERFORM VARYING     IDXA    FROM    1   BY  1
                   UNTIL       IDXA        >   IDXS
      *
               MOVE    TBL-SRYACT-REC (IDXA)   TO  SRYACT-REC
      *
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2        >   5   )  OR
                                  (SRY-SRYCD(IDX2) =   SPACE )
      *            点滴・中心静脈薬剤
                   IF     (SRY-SRYSYUKBN           =   "330"
                                                   OR  "331"
                                                   OR  "350"
                                                   OR  "332"
                                                   OR  "352")
      *                薬剤
                       IF     (SRY-SRYCD (IDX2)(1:1)   =   "6" )
                           PERFORM 10012-TBL-HENSYU-SEC
                           MOVE    1               TO  FLG-COMMENT
      *
                           ADD     1               TO  IDX-ZAI
                           MOVE    SRY-SRYCD (IDX2)
                                                   TO  TBL-ZAI-SRYCD
                                                            (IDX-ZAI)
                       END-IF
      *                薬剤のコメント
                       IF     (FLG-COMMENT          =   1   )  AND
                              (IDX-YAK             >   ZERO ) AND
                             ((SRY-SRYCD (IDX2)(1:1)   =   "8" )  OR
                              (SRY-SRYCD (IDX2)(1:3)   =   "008"))
                           IF     (SRY-SRYCD (IDX2)(1:4)   =   "0085"
                                                           OR  "0086")
                               CONTINUE
                           ELSE
                               PERFORM 10017-COMMNT-HENSYU-SEC
      *
                               ADD     1               TO  IDX-ZAI
                               MOVE    SRY-SRYCD (IDX2)
                                                       TO  TBL-ZAI-SRYCD
                                                            (IDX-ZAI)
                           END-IF
                       END-IF
      *                コメントフラグクリア
                       IF     (SRY-SRYCD (IDX2)(1:1)   =   "6" )  OR
                             ((SRY-SRYCD (IDX2)(1:1)   =   "8" )  OR
                              (SRY-SRYCD (IDX2)(1:3)   =   "008"))
      *                    残量廃棄
                       OR     (SRY-SRYCD (IDX2)        =   "099309901")
                           CONTINUE
                       ELSE
                           MOVE    ZERO                TO  FLG-COMMENT
                       END-IF
                   END-IF
      *            手技料
                   IF     (SRY-SRYCD (IDX2)(1:1)   =   "1" )  AND
                          (SRY-SRYSYUKBN           =   "330" )
                       PERFORM 10013-SYUG-HENSYU-SEC
                   END-IF
      *            手技料（中心静脈）
                   IF      SRY-SRYCD (IDX2)        =   "130004410"
                       PERFORM 10013-SYUG-HENSYU-SEC
                       PERFORM 10014-CHU-HENSYU-SEC
                   END-IF
      *            手技料（動脈）
                   IF      SRY-SRYCD (IDX2)        =   "130006110" OR
                                                       "130006120"
                       PERFORM 10015-DOU-HENSYU-SEC
                   END-IF
      *H28.6       中心静脈注射(植入型カテーテル）
                   IF      ORCSTEN-SRYYM       >=  "201604"
                       IF      SRY-SRYCD (IDX2)        =   CON-CHUJYO6
                           PERFORM 10016-CHU06-HENSYU-SEC
                       END-IF
                   END-IF
      *
               END-PERFORM
           END-PERFORM
      *
      *    器材テーブル編集
           IF     (TBL-SRY-SRYSYUKBN (1)   =   "330"  OR  "331"
                                           OR  "350"
                                           OR  "332"
                                           OR  "352")
           AND    (ACCT-ZAIKAISU           >   ZERO )
      *        診療行為より器材編集
               PERFORM 10016-TBL-KIZAIHEN-SEC
           END-IF
      *
      *    日毎のテーブル編集
           IF     (TBL-SRY-SRYSYUKBN (1)   =   "330"
                                           OR  "331"
                                           OR  "350"
                                           OR  "332"
                                           OR  "352")
           AND    (FLG-YAKU-ARI            =   1    )
      *        診療行為剤順編集
               PERFORM 10017-TBL-DAYHEN-SEC
           END-IF
           .
       100-HENSYU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル編集処理
      *****************************************************************
       10012-TBL-HENSYU-SEC            SECTION.
      *
      *    同一コードチェック
           PERFORM VARYING IDX-T       FROM    1   BY  1
                   UNTIL   IDX-T       >   100
               IF      SRY-SRYCD (IDX2)    =   TBL-SRYCD (IDX-T)
                   IF      SRY-SRYSYUKBN       =   "330"
                       PERFORM 100121-DAY-HEN-SEC
                   END-IF
                   IF      SRY-SRYSYUKBN       =   "331"
                       PERFORM 100122-DAY2-HEN-SEC
                   END-IF
                   IF      SRY-SRYSYUKBN       =   "350"
                       PERFORM 100123-DAY3-HEN-SEC
                   END-IF
                   IF      SRY-SRYSYUKBN       =   "332"
                       PERFORM 100122-DAY4-HEN-SEC
                   END-IF
                   IF      SRY-SRYSYUKBN       =   "352"
                       PERFORM 100123-DAY5-HEN-SEC
                   END-IF
      *
                   MOVE    IDX-T           TO  IDX-YAK
      *
                   MOVE    100             TO  IDX-T
               ELSE
                   IF      TBL-SRYCD (IDX-T)   =   SPACE
                       MOVE    SRY-SRYCD (IDX2)    TO  TBL-SRYCD (IDX-T)
      *                点数マスタ
                       MOVE    SRY-SRYCD (IDX2)    TO  TNS-SRYCD
                       PERFORM 910-TENSU-READ-SEC
                       IF      FLG-TENSU           =   ZERO
      *                    点数
                           MOVE    TNS-TEN         TO  TBL-TEN   (IDX-T)
      *H31.4
      *                    点数識別による計算
                           IF    (TNS-SRYCD(1:1)   =   "6"  )
                             AND (TNS-TEN          >   ZERO )
      *                        薬剤　乗算金額（１０で乗ずる）
                               IF      TNS-TENSIKIBETU =   5
                                   COMPUTE TBL-TEN   (IDX-T)
                                                       =   TNS-TEN
                                                       *   10
                               END-IF
      *                        薬剤　除算金額（１０で除す）
                               IF      TNS-TENSIKIBETU =   4
                                   COMPUTE TBL-TEN   (IDX-T)
                                                       =   TNS-TEN
                                                       /   10
                               END-IF
                           END-IF
      *
      *                    液量
                           IF      TNS-CSYYOURYO   =   ZERO
                               MOVE    1               TO  TBL-CSYYOURYO
                                                                 (IDX-T)
                           ELSE
                               MOVE    TNS-CSYYOURYO   TO  TBL-CSYYOURYO
                                                                 (IDX-T)
                           END-IF
      *                    生物学的製剤区分
                           MOVE    TNS-SEIBUTUGAKUKBN  TO  TBL-SEIBUTU
                                                               (IDX-T)
      *                    麻毒
                           MOVE    TNS-MADOKUKBN       TO  TBL-MADOKU
                                                               (IDX-T)
      *
                           IF      SRY-SRYSYUKBN       =   "330"
                               PERFORM 100121-DAY-HEN-SEC
                           END-IF
                           IF      SRY-SRYSYUKBN       =   "331"
                               PERFORM 100122-DAY2-HEN-SEC
                           END-IF
                           IF      SRY-SRYSYUKBN       =   "350"
                               PERFORM 100123-DAY3-HEN-SEC
                           END-IF
                           IF      SRY-SRYSYUKBN       =   "332"
                               PERFORM 100122-DAY4-HEN-SEC
                           END-IF
                           IF      SRY-SRYSYUKBN       =   "352"
                               PERFORM 100123-DAY5-HEN-SEC
                           END-IF
                       END-IF
      *
                       MOVE    IDX-T           TO  IDX-YAK
      *
                       MOVE    100             TO  IDX-T
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       10012-TBL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル編集処理
      *****************************************************************
       100121-DAY-HEN-SEC            SECTION.
      *
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               IF      ACCT-DAY (1 IDX-D)  >   ZERO
                   ADD     ACCT-DAY (1 IDX-D)  TO  TBL-DAY (IDX-T IDX-D)
      *ver.4.7
      *            数量計算
                   MOVE    ACCT-DAY (1 IDX-D)  TO  WRK-DAY
                   PERFORM 100120-SURYO-KEI-SEC
      *            数量
                   IF      FLG-HAIKI            =   1
      *                廃棄数量
                       MOVE    1               TO  TBL-HAIKI
                                                          (IDX-T IDX-D)
                       ADD     WRK-HAIKI-SURYO TO  TBL-HAIKI-SURYO
                                                          (IDX-T IDX-D)
                   END-IF
                   ADD     WRK-SURYO            TO  TBL-SURYO
                                                          (IDX-T IDX-D)
      *            液量
                   COMPUTE WRK-EKIRYOU     =   TBL-CSYYOURYO (IDX-T)
                                           *   WRK-SURYO
                   ADD     WRK-EKIRYOU     TO  TBL-CHUSYARYO
                                                      (IDX-T IDX-D)
      *            診療科・保険
      *           １つの薬剤は１日に同じ科・保険でのみ使用するものとする
                   MOVE    ACCT-SRYKA          TO  TBL-SRYKA
                                                      (IDX-T IDX-D)
                   MOVE    ACCT-HKNCOMBI       TO  TBL-HKNCOMBI
                                                      (IDX-T IDX-D)
      *ver.4.7
      *            同日再入院数量計算
                   IF      ACCT-DAY (2 IDX-D)  >   ZERO
                       MOVE    ACCT-DAY (2 IDX-D)  TO  WRK-DAY
                       PERFORM 100120-SURYO-KEI-SEC
                       ADD     WRK-SURYO            TO  TBL-SURYO-D
                                                       (IDX-T IDX-D 1)
                   END-IF
                   IF      ACCT-DAY (3 IDX-D)  >   ZERO
                       MOVE    ACCT-DAY (3 IDX-D)  TO  WRK-DAY
                       PERFORM 100120-SURYO-KEI-SEC
                       ADD     WRK-SURYO            TO  TBL-SURYO-D
                                                       (IDX-T IDX-D 2)
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       100121-DAY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    数量計算編集処理
      *****************************************************************
       100120-SURYO-KEI-SEC            SECTION.
      *
      *    残量廃棄計算
           MOVE    ZERO                TO  FLG-HAIKI
           MOVE    ZERO                TO  WRK-SURYO
           MOVE    ZERO                TO  WRK-HAIKI-SURYO
      *
           IF     (IDX2                <   5  )  AND
                  (SRY-SRYCD (IDX2 + 1)    =   "099309901")
               MOVE    1                   TO  FLG-HAIKI
           END-IF
           IF     (IDX2                =   5  )  AND
                  (IDXA                <   IDXS)
               COMPUTE IDXA2           =   IDXA    +   1
               IF      TBL-SRY-SRYCD (IDXA2 1)
                                           =   "099309901"
                   MOVE    1                   TO  FLG-HAIKI
               END-IF
           END-IF
      *    数量
           IF      FLG-HAIKI           =   ZERO
               COMPUTE WRK-SURYO           =   SRY-SRYSURYO(IDX2)
                                           *   WRK-DAY
           ELSE
               COMPUTE WRK-SURYO-9         =   SRY-SRYSURYO(IDX2)
                                           +   0.99999
               COMPUTE WRK-SURYO           =   WRK-SURYO-9
                                           *   WRK-DAY
      *        廃棄数量
               COMPUTE WRK-HAIKI-SURYO =  (WRK-SURYO-9
                                           -   SRY-SRYSURYO(IDX2))
                                           *   WRK-DAY
           END-IF
      *
           .
       100120-SURYO-KEI-EXT.
           EXIT.
      *****************************************************************
      *    テーブル編集処理（点滴手技料なし）
      *****************************************************************
       100122-DAY2-HEN-SEC            SECTION.
      *
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               IF      ACCT-DAY (1 IDX-D)  >   ZERO
                   ADD     ACCT-DAY (1 IDX-D)  TO  TBL-DAY2(IDX-T IDX-D)
      *            数量計算
                   MOVE    ACCT-DAY (1 IDX-D)  TO  WRK-DAY
                   PERFORM 100120-SURYO-KEI-SEC
      *            数量
                   IF      FLG-HAIKI            =   1
      *                廃棄数量
                       MOVE    1               TO  TBL-HAIKI2
                                                          (IDX-T IDX-D)
                       ADD     WRK-HAIKI-SURYO TO  TBL-HAIKI-SURYO2
                                                          (IDX-T IDX-D)
                   END-IF
                   ADD     WRK-SURYO            TO  TBL-SURYO2
                                                          (IDX-T IDX-D)
      *H26.10
      *            診療科編集
                   MOVE    ACCT-SRYKA          TO  TBL-SRYKA2
                                                      (IDX-T IDX-D)
      *ver.4.7
      *            同日再入院数量計算
                   IF      ACCT-DAY (2 IDX-D)  >   ZERO
                       MOVE    ACCT-DAY (2 IDX-D)  TO  WRK-DAY
                       PERFORM 100120-SURYO-KEI-SEC
                       ADD     WRK-SURYO            TO  TBL-SURYO2-D
                                                       (IDX-T IDX-D 1)
                   END-IF
                   IF      ACCT-DAY (3 IDX-D)  >   ZERO
                       MOVE    ACCT-DAY (3 IDX-D)  TO  WRK-DAY
                       PERFORM 100120-SURYO-KEI-SEC
                      ADD     WRK-SURYO            TO  TBL-SURYO2-D
                                                       (IDX-T IDX-D 2)
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       100122-DAY2-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル編集処理（点滴手術以外）
      *****************************************************************
       100122-DAY4-HEN-SEC            SECTION.
      *
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               IF      ACCT-DAY (1 IDX-D)  >   ZERO
                   ADD     ACCT-DAY (1 IDX-D)  TO  TBL-DAY4(IDX-T IDX-D)
      *ver.4.7
      *            数量計算
                   MOVE    ACCT-DAY (1 IDX-D)  TO  WRK-DAY
                   PERFORM 100120-SURYO-KEI-SEC
      *            数量
                   IF      FLG-HAIKI            =   1
      *                廃棄数量
                       MOVE    1               TO  TBL-HAIKI4
                                                          (IDX-T IDX-D)
                       ADD     WRK-HAIKI-SURYO TO  TBL-HAIKI-SURYO4
                                                          (IDX-T IDX-D)
                   END-IF
                   ADD     WRK-SURYO            TO  TBL-SURYO4
                                                          (IDX-T IDX-D)
      *            液量
                   COMPUTE WRK-EKIRYOU     =   TBL-CSYYOURYO (IDX-T)
                                           *   WRK-SURYO
                   ADD     WRK-EKIRYOU     TO  TBL-CHUSYARYO4
                                                      (IDX-T IDX-D)
      *            診療科・保険
      *           １つの薬剤は１日に同じ科・保険でのみ使用するものとする
                   MOVE    ACCT-SRYKA          TO  TBL-SRYKA4
                                                      (IDX-T IDX-D)
                   MOVE    ACCT-HKNCOMBI       TO  TBL-HKNCOMBI4
                                                      (IDX-T IDX-D)
      *ver.4.7
      *            同日再入院数量計算
                   IF      ACCT-DAY (2 IDX-D)  >   ZERO
                       MOVE    ACCT-DAY (2 IDX-D)  TO  WRK-DAY
                       PERFORM 100120-SURYO-KEI-SEC
                       ADD     WRK-SURYO            TO  TBL-SURYO4-D
                                                       (IDX-T IDX-D 1)
                   END-IF
                   IF      ACCT-DAY (3 IDX-D)  >   ZERO
                       MOVE    ACCT-DAY (3 IDX-D)  TO  WRK-DAY
                       PERFORM 100120-SURYO-KEI-SEC
                       ADD     WRK-SURYO            TO  TBL-SURYO4-D
                                                       (IDX-T IDX-D 2)
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       100122-DAY4-HEN-EXT.
           EXIT.
      *****************************************************************
      *    テーブル編集処理（中心静脈薬剤）
      *****************************************************************
       100123-DAY3-HEN-SEC            SECTION.
      *
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               IF      ACCT-DAY (1 IDX-D)  >   ZERO
                   ADD     ACCT-DAY (1 IDX-D)  TO  TBL-DAY3(IDX-T IDX-D)
      *ver.4.7
      *            数量計算
                   MOVE    ACCT-DAY (1 IDX-D)  TO  WRK-DAY
                   PERFORM 100120-SURYO-KEI-SEC
      *            数量
                   IF      FLG-HAIKI            =   1
      *                廃棄数量
                       MOVE    1               TO  TBL-HAIKI3
                                                          (IDX-T IDX-D)
                       ADD     WRK-HAIKI-SURYO TO  TBL-HAIKI-SURYO3
                                                          (IDX-T IDX-D)
                   END-IF
                   ADD     WRK-SURYO            TO  TBL-SURYO3
                                                          (IDX-T IDX-D)
      *            診療科・保険
      *           １つの薬剤は１日に同じ科・保険でのみ使用するものとする
                   MOVE    ACCT-SRYKA          TO  TBL-SRYKA3
                                                      (IDX-T IDX-D)
                   MOVE    ACCT-HKNCOMBI       TO  TBL-HKNCOMBI3
                                                      (IDX-T IDX-D)
      *ver.4.7
      *            同日再入院数量計算
                   IF      ACCT-DAY (2 IDX-D)  >   ZERO
                       MOVE    ACCT-DAY (2 IDX-D)  TO  WRK-DAY
                       PERFORM 100120-SURYO-KEI-SEC
                       ADD     WRK-SURYO           TO  TBL-SURYO3-D
                                                       (IDX-T IDX-D 1)
                   END-IF
                   IF      ACCT-DAY (3 IDX-D)  >   ZERO
                       MOVE    ACCT-DAY (3 IDX-D)  TO  WRK-DAY
                       PERFORM 100120-SURYO-KEI-SEC
                       ADD     WRK-SURYO           TO  TBL-SURYO3-D
                                                       (IDX-T IDX-D 2)
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       100123-DAY3-HEN-EXT.
           EXIT.
      *****************************************************************
      *    テーブル編集処理（中心静脈薬剤 手術以外）
      *****************************************************************
       100123-DAY5-HEN-SEC            SECTION.
      *
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               IF      ACCT-DAY (1 IDX-D)  >   ZERO
                   ADD     ACCT-DAY (1 IDX-D)  TO  TBL-DAY5(IDX-T IDX-D)
      *ver.4.7
      *            数量計算
                   MOVE    ACCT-DAY (1 IDX-D)  TO  WRK-DAY
                   PERFORM 100120-SURYO-KEI-SEC
      *            数量
                   IF      FLG-HAIKI            =   1
      *                廃棄数量
                       MOVE    1               TO  TBL-HAIKI5
                                                          (IDX-T IDX-D)
                       ADD     WRK-HAIKI-SURYO TO  TBL-HAIKI-SURYO5
                                                          (IDX-T IDX-D)
                   END-IF
                   ADD     WRK-SURYO            TO  TBL-SURYO5
                                                          (IDX-T IDX-D)
      *            診療科・保険
      *           １つの薬剤は１日に同じ科・保険でのみ使用するものとする
                   MOVE    ACCT-SRYKA          TO  TBL-SRYKA5
                                                      (IDX-T IDX-D)
                   MOVE    ACCT-HKNCOMBI       TO  TBL-HKNCOMBI5
                                                      (IDX-T IDX-D)
      *ver.4.7
      *            同日再入院数量計算
                   IF      ACCT-DAY (2 IDX-D)  >   ZERO
                       MOVE    ACCT-DAY (2 IDX-D)  TO  WRK-DAY
                       PERFORM 100120-SURYO-KEI-SEC
                       ADD     WRK-SURYO            TO  TBL-SURYO5-D
                                                       (IDX-T IDX-D 1)
                   END-IF
                   IF      ACCT-DAY (3 IDX-D)  >   ZERO
                       MOVE    ACCT-DAY (3 IDX-D)  TO  WRK-DAY
                       PERFORM 100120-SURYO-KEI-SEC
                       ADD     WRK-SURYO            TO  TBL-SURYO5-D
                                                       (IDX-T IDX-D 2)
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       100123-DAY5-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤コメントテーブル編集処理
      *****************************************************************
       10017-COMMNT-HENSYU-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-COMMENT
           IF      SRY-INPUTNUM (IDX2) =   ZERO
               MOVE    SRY-SRYCD (IDX2)    TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
               IF      FLG-TENSU           =   ZERO
                   MOVE    TNS-NAME            TO  WRK-COMMENT
               END-IF
           ELSE
               PERFORM 100141-PTCOM-SET-SEC
           END-IF
      *
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               IF      ACCT-DAY (1 IDX-D)  >   ZERO
                   PERFORM VARYING IDX-C1      FROM    1   BY  1
                           UNTIL   IDX-C1      >   5
                       IF      TBL-COM-COMMENT (IDX-YAK IDX-D IDX-C1)
                                               =   SPACE
                           MOVE    WRK-COMMENT     TO  TBL-COM-COMMENT
                                               (IDX-YAK IDX-D IDX-C1)
                           MOVE    SRY-SRYCD (IDX2)
                                                   TO   TBL-COM-SRYCD
                                               (IDX-YAK IDX-D IDX-C1)
                       END-IF
                       IF      WRK-COMMENT     =   TBL-COM-COMMENT
                                               (IDX-YAK IDX-D IDX-C1)
                           MOVE    ACCT-DAY (1 IDX-D)
                                               TO  TBL-COM-DAY
                                               (IDX-YAK IDX-D IDX-C1)
                           MOVE    5           TO  IDX-C1
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           .
       10017-COMMNT-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    患者コメントから編集する
      *****************************************************************
       100141-PTCOM-SET-SEC              SECTION.
      *
           INITIALIZE                     PTCOM-REC
           MOVE    SRY-HOSPNUM         TO  PTCOM-HOSPNUM
           MOVE    SRY-PTID            TO  PTCOM-PTID
           MOVE    SRY-ZAINUM          TO  PTCOM-ZAINUM
           MOVE    SRY-SRYCD   (IDX2)  TO  PTCOM-SRYCD
           MOVE    SRY-INPUTNUM(IDX2)  TO  PTCOM-RENNUM
      *    主キーで検索
           PERFORM 950-PTCOM-READ-SEC
           IF      FLG-PTCOM           =   ZERO
               MOVE    PTCOM-INPUTCOMENT   TO  WRK-COMMENT
           END-IF
           .
       100141-PTCOM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    器材集計テーブル編集処理
      *****************************************************************
       10016-TBL-KIZAIHEN-SEC            SECTION.
      *
           INITIALIZE                  TBL-KIZAI-AREA
           MOVE    ZERO                TO  FLG-COMMENT
           MOVE    ZERO                TO  IDX-C1
      *    器材のみを保存する
           MOVE    ZERO                TO  IDX-T
           PERFORM VARYING     IDXA    FROM    1   BY  1
                   UNTIL      (IDXA        >   IDXS  )
                           OR (IDX-T       >=  10    )
      *
               MOVE    TBL-SRYACT-REC (IDXA)   TO  SRYACT-REC
      *
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2        >   5   )  OR
                                  (SRY-SRYCD(IDX2) =   SPACE ) OR
                                  (IDX-T           >=  10    )
                   IF     (SRY-SRYCD (IDX2)(1:1)   =   "7" )  OR
                          (SRY-SRYCD (IDX2)(1:3)   =   "059" )
                   OR     (SRY-SRYCD (IDX2)(1:3)   =   "058" )
                       ADD     1                   TO  IDX-T
                       MOVE    SRY-SRYCD (IDX2)    TO  TBL-KIZ-SRYCD
                                                               (IDX-T)
                       MOVE    SRY-SRYSURYO(IDX2)  TO  TBL-KIZ-SURYO
                                                               (IDX-T)
                       MOVE    1                   TO  FLG-COMMENT
                       MOVE    ZERO                TO  IDX-C1
                   END-IF
      *            薬剤のコメント
                   IF     (FLG-COMMENT          =   1   )  AND
                         ((SRY-SRYCD (IDX2)(1:1)   =   "8" )  OR
                          (SRY-SRYCD (IDX2)(1:3)   =   "008"))
                       IF     (SRY-SRYCD (IDX2)(1:4)   =   "0085"
                                                       OR  "0086")
                           CONTINUE
                       ELSE
                           PERFORM 100163-KIZ-COMMNT-SEC
                       END-IF
                   END-IF
      *            コメントフラグクリア
                   IF     (SRY-SRYCD (IDX2)(1:1)   =   "7" )  OR
                          (SRY-SRYCD (IDX2)(1:3)   =   "059" ) OR
                          (SRY-SRYCD (IDX2)(1:3)   =   "058" ) OR
                         ((SRY-SRYCD (IDX2)(1:1)   =   "8" )  OR
                          (SRY-SRYCD (IDX2)(1:3)   =   "008"))
                       CONTINUE
                   ELSE
                       MOVE    ZERO                TO  FLG-COMMENT
                   END-IF
               END-PERFORM
           END-PERFORM
           IF      IDX-T               =   ZERO
               GO      TO      10016-TBL-KIZAIHEN-EXT
           END-IF
      *
      *    剤点数の同じものを検索する
           PERFORM VARYING     IDX-T   FROM    1   BY  1
                   UNTIL       IDX-T       >   50
               IF      ORCSTEN-KIZ-ZAITEN (IDX-T)  =   ZERO
                   PERFORM 100161-KIZAI-HEN-SEC
                   MOVE    50              TO  IDX-T
               ELSE
                   IF      ORCSTEN-KIZ-ZAITEN (IDX-T)
                                           =   ACCT-KIZAITEN
                       PERFORM 100162-KIAZI-CHK-SEC
                       IF      FLG-ERR         =   ZERO
                           PERFORM 100164-KIZAI-ADDHEN-SEC
                           MOVE    50              TO  IDX-T
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       10016-TBL-KIZAIHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル編集処理
      *****************************************************************
       100161-KIZAI-HEN-SEC            SECTION.
      *
           MOVE    ACCT-KIZAITEN       TO  ORCSTEN-KIZ-ZAITEN (IDX-T)
      *
           PERFORM VARYING     IDX-T2  FROM    1   BY  1
                   UNTIL      (IDX-T2      >   10    )
                           OR (TBL-KIZ-SRYCD(IDX-T2)  =   SPACE)
               MOVE    TBL-KIZ-SRYCD(IDX-T2)   TO  ORCSTEN-KIZ-SRYCD
                                                        (IDX-T IDX-T2)
               MOVE    TBL-KIZ-SURYO(IDX-T2)   TO  ORCSTEN-KIZ-SURYO
                                                        (IDX-T IDX-T2)
               PERFORM VARYING     IDX-C1  FROM    1   BY  1
                       UNTIL      (IDX-C1      >   5     )
                               OR (TBL-KIZ-COM-SRYCD(IDX-T2 IDX-C1)
                                              =   SPACE)
                   MOVE    TBL-KIZ-COM-SRYCD(IDX-T2 IDX-C1)
                                           TO  ORCSTEN-KIZ-COM-SRYCD
                                                  (IDX-T IDX-T2 IDX-C1)
                   MOVE    TBL-KIZ-COM-COMMENT(IDX-T2 IDX-C1)
                                           TO  ORCSTEN-KIZ-COM-MEI
                                                  (IDX-T IDX-T2 IDX-C1)
               END-PERFORM
           END-PERFORM
      *    回数集計
           PERFORM VARYING     IDX-T2  FROM    1   BY  1
                   UNTIL       IDX-T2      >   31
               ADD     ACCT-DAY (1 IDX-T2) TO  ORCSTEN-KIZ-DAY
                                                      (IDX-T IDX-T2)
      *ver.4.7
               ADD     ACCT-DAY (2 IDX-T2) TO  ORCSTEN-KIZ-DAY2
                                                      (IDX-T IDX-T2 1)
               ADD     ACCT-DAY (3 IDX-T2) TO  ORCSTEN-KIZ-DAY2
                                                      (IDX-T IDX-T2 2)
      *H26.10
      *        診療科
               IF      ORCSTEN-KIZ-SRYKA  (IDX-T IDX-T2)
                                           =   SPACE
                   MOVE    ACCT-SRYKA      TO  ORCSTEN-KIZ-SRYKA
                                                          (IDX-T IDX-T2)
               END-IF
           END-PERFORM
      *
           .
       100161-KIZAI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルチェック処理
      *****************************************************************
       100162-KIAZI-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
      *
           PERFORM VARYING     IDX-T2  FROM    1   BY  1
                   UNTIL       IDX-T2      >   10
               MOVE    ZERO                TO  FLG-CHK
               PERFORM VARYING     IDX-T3  FROM    1   BY  1
                       UNTIL      (IDX-T3      >   10 )  OR
                                  (FLG-CHK     =   1  )
      *            コードと数量が同じものがあること
                   IF     (ORCSTEN-KIZ-SRYCD (IDX-T IDX-T2)
                                           =   TBL-KIZ-SRYCD(IDX-T3))
                   AND    (ORCSTEN-KIZ-SURYO (IDX-T IDX-T2)
                                           =   TBL-KIZ-SURYO(IDX-T3))
                   AND    (TBL-KIZ-CHK  (IDX-T3)
                                           =   ZERO            )
      *                コメント内容一致
                       PERFORM 1001621-KIZA-COM-CHK-SEC
                       IF      FLG-COMMENT-OK  =   ZERO
                           MOVE    1                TO  TBL-KIZ-CHK
                                                              (IDX-T3)
                           MOVE    1                TO  FLG-CHK
                       END-IF
                   END-IF
               END-PERFORM
               IF      FLG-CHK             =   ZERO
                   MOVE    1               TO  FLG-ERR
                   MOVE    10              TO  IDX-T2
               END-IF
           END-PERFORM
      *
           .
       100162-KIAZI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    器材コメントテーブルチェック処理
      *****************************************************************
       1001621-KIZA-COM-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-COMMENT-OK
           PERFORM VARYING     IDX-C1  FROM    1   BY  1
                   UNTIL      (IDX-C1      >   5     )
                           OR (TBL-KIZ-COM-SRYCD(IDX-T2 IDX-C1)
                                              =   SPACE)
                           OR (FLG-COMMENT-OK =   1   )
               IF     (TBL-KIZ-COM-SRYCD(IDX-T3 IDX-C1)
                                           =   ORCSTEN-KIZ-COM-SRYCD
                                                  (IDX-T IDX-T2 IDX-C1))
                 AND  (TBL-KIZ-COM-COMMENT(IDX-T3 IDX-C1)
                                           =   ORCSTEN-KIZ-COM-MEI
                                                  (IDX-T IDX-T2 IDX-C1))
                   CONTINUE
               ELSE
                   MOVE    1               TO  FLG-COMMENT-OK
               END-IF
           END-PERFORM
           .
       1001621-KIZA-COM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル編集処理
      *****************************************************************
       100164-KIZAI-ADDHEN-SEC            SECTION.
      *
      *    回数集計
           PERFORM VARYING     IDX-T2  FROM    1   BY  1
                   UNTIL       IDX-T2      >   31
               ADD     ACCT-DAY (1 IDX-T2) TO  ORCSTEN-KIZ-DAY
                                                      (IDX-T IDX-T2)
      *ver.4.7
               ADD     ACCT-DAY (2 IDX-T2) TO  ORCSTEN-KIZ-DAY2
                                                      (IDX-T IDX-T2 1)
               ADD     ACCT-DAY (3 IDX-T2) TO  ORCSTEN-KIZ-DAY2
                                                      (IDX-T IDX-T2 2)
           END-PERFORM
      *
           .
       100164-KIZAI-ADDHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    器材コメント編集処理
      *****************************************************************
       100163-KIZ-COMMNT-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-COMMENT
           IF      SRY-INPUTNUM (IDX2) =   ZERO
               MOVE    SRY-SRYCD (IDX2)    TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
               IF      FLG-TENSU           =   ZERO
                   MOVE    TNS-NAME            TO  WRK-COMMENT
               END-IF
           ELSE
               PERFORM 100141-PTCOM-SET-SEC
           END-IF
      *
           ADD     1                   TO  IDX-C1
           IF      IDX-C1              <=   5
               MOVE    SRY-SRYCD (IDX2)    TO  TBL-KIZ-COM-SRYCD 
                                                      (IDX-T IDX-C1)
               MOVE    WRK-COMMENT         TO  TBL-KIZ-COM-COMMENT
                                                      (IDX-T IDX-C1)
           END-IF
           .
       100163-KIZ-COMMNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為剤順編集編集処理
      *****************************************************************
       10017-TBL-DAYHEN-SEC            SECTION.
      *
           MOVE    ZERO                TO  IDX-DAY
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL      (IDX-D   >   31  )
               IF      ACCT-DAY (1 IDX-D)  >   ZERO
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL      (IDX2        >   50   )
                                 OR   (TBL-ZAI-SRYCD(IDX2) =   SPACE)
                       PERFORM VARYING     IDX3    FROM    1   BY  1
                               UNTIL      (IDX3        >   50   )
                           IF     (TBL-D-SRYCD (IDX-D IDX3)
                                                       =   SPACE )
                               OR (TBL-D-SRYCD (IDX-D IDX3)
                                                       =   TBL-ZAI-SRYCD
                                                                (IDX2) )
                               MOVE    TBL-ZAI-SRYCD(IDX2) 
                                                       TO  TBL-D-SRYCD
                                                           (IDX-D IDX3)
                               MOVE    50              TO  IDX3
                           END-IF
                       END-PERFORM
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           .
       10017-TBL-DAYHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    手技料（中心静脈）編集処理
      *****************************************************************
       10014-CHU-HENSYU-SEC            SECTION.
      *
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               IF      ACCT-DAY (1 IDX-D)  >   ZERO
                   ADD     ACCT-DAY (1 IDX-D)  TO  TBL-CHU-ARI (IDX-D)
                   IF      TBL-CHU-ARI (IDX-D)     >   1
                       MOVE    1               TO  ORCSTEN-CHU-DAY
                                                              (IDX-D)
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       10014-CHU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    手技料（動脈）チェック処理
      *****************************************************************
       10015-DOU-HENSYU-SEC            SECTION.
      *
      *
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               IF      ACCT-DAY (1 IDX-D)  >   ZERO
                   ADD     ACCT-DAY (1 IDX-D)  TO  TBL-DOU-ARI (IDX-D)
                   IF      TBL-DOU-ARI (IDX-D)     >   1
                       MOVE    1               TO  ORCSTEN-DOU-DAY
                                                              (IDX-D)
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       10015-DOU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    手技料（中心静脈注射(植入型カテーテル））チェック処理
      *****************************************************************
       10016-CHU06-HENSYU-SEC            SECTION.
      *
      *
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               IF      ACCT-DAY (1 IDX-D)  >   ZERO
                   ADD     ACCT-DAY (1 IDX-D)  TO  TBL-CHU6-ARI (IDX-D)
               END-IF
           END-PERFORM
      *
           .
       10016-CHU06-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    手技料編集処理
      *****************************************************************
       10013-SYUG-HENSYU-SEC            SECTION.
      *
      *    同一コードチェック
           PERFORM VARYING IDX-T       FROM    1   BY  1
                   UNTIL   IDX-T       >   50
               IF      SRY-SRYCD (IDX2)    =   TBL2-SRYCD (IDX-T)
                   PERFORM 100131-SYU-DAY-HEN-SEC
                   MOVE    50              TO  IDX-T
               ELSE
                   IF      TBL2-SRYCD (IDX-T)  =   SPACE
                       MOVE    SRY-SRYCD (IDX2)    TO  TBL2-SRYCD(IDX-T)
      *                点数マスタ
                       MOVE    SRY-SRYCD (IDX2)    TO  TNS-SRYCD
                       PERFORM 910-TENSU-READ-SEC
                       IF      FLG-TENSU           =   ZERO
      *                    区分
                           MOVE    TNS-DATAKBN     TO  TBL2-DATAKBN
                                                             (IDX-T)
                       END-IF
      *                生物学的製剤区分
                       IF      SRY-SRYCD (IDX2)    =   "130000110"
                           MOVE    1               TO  TBL2-SEIBUTU
                                                                (IDX-T)
                       END-IF
      *                麻毒
                       IF      SRY-SRYCD (IDX2)    =   "130000310"
                           MOVE    1               TO  TBL2-MADOKU
                                                               (IDX-T)
                       END-IF
      *
      *                中心静脈
                       IF      SRY-SRYCD (IDX2)    =   "130004410"
                           MOVE    1               TO  TBL2-IVHFLG
                                                               (IDX-T)
                       END-IF
      *
                       PERFORM 100131-SYU-DAY-HEN-SEC
                       MOVE    50              TO  IDX-T
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       10013-SYUG-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル編集処理
      *****************************************************************
       100131-SYU-DAY-HEN-SEC            SECTION.
      *
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               ADD     ACCT-DAY (1 IDX-D)  TO  TBL2-DAY (IDX-T IDX-D)
      *        診療科・保険
               IF      ACCT-DAY (1 IDX-D)  >   ZERO
                   IF      TBL2-SRYKA (IDX-T IDX-D)    =   SPACE
                       MOVE    ACCT-SRYKA          TO  TBL2-SRYKA
                                                      (IDX-T IDX-D)
                       MOVE    ACCT-HKNCOMBI       TO  TBL2-HKNCOMBI
                                                      (IDX-T IDX-D)
                       MOVE    ACCT-ZAINUM         TO  TBL2-ZAINUM
                                                      (IDX-T IDX-D)
                       IF      FLG-YAKU-ARI        =   1
                           MOVE    1               TO  TBL2-YAKU-ARI
                                                      (IDX-T IDX-D)
                       ELSE
                           MOVE    ZERO            TO  TBL2-YAKU-ARI
                                                      (IDX-T IDX-D)
                       END-IF
                   ELSE
      *                同じ診療コードは１日に１回のみとする
                       PERFORM 1001311-SYU-ERR-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       100131-SYU-DAY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    同日複数算定エラー処理
      *****************************************************************
       1001311-SYU-ERR-SEC            SECTION.
      *
           MOVE    1                   TO  ORCSTEN-FUKU-DAY (IDX-D)
           .
       1001311-SYU-ERR-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数計算処理
      *****************************************************************
       200-KEISAN-SYORI-SEC            SECTION.
      *
           INITIALIZE                  TBL-SYUKEI-AREA
      *
           MOVE    ZERO                TO  IDX
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               MOVE    ZERO            TO  IDX
               MOVE    ZERO            TO  WRK-TENSU-GOK
               MOVE    ZERO            TO  WRK-DOU-TENSU-GOK(1)
               MOVE    ZERO            TO  WRK-DOU-TENSU-GOK(2)
               MOVE    ZERO            TO  FLG-TENTEKI
               PERFORM VARYING     IDX3    FROM    1   BY  1
                       UNTIL      (IDX3        >   50   )
                               OR (TBL-D-SRYCD (IDX-D IDX3)
                                               =   SPACE )
      *            コード点数編集処理
                   PERFORM 2001-TBL-CHEK-SEC
               END-PERFORM
      *        点数計算
               IF      FLG-TENTEKI         =   1
                   IF      WRK-TENSU-GOK       <=  15
                       MOVE    1               TO  WRK-SYOKEIS2
                   ELSE
                       COMPUTE WRK-SYOKEIS2    =  ((WRK-TENSU-GOK
                                               -  15)
                                               /    10 )
                                               +   1
                   END-IF
                   COMPUTE WRK-TENSU           =   WRK-SYOKEIS2
                                               +  0.99999
                   MOVE    WRK-TENSU           TO  ORCSTEN-ZAITEN
                                                           (IDX-D)
      *ver.4.7
                   PERFORM 2009-DOUJIT-ZAITEN-SEC
               ELSE
                   MOVE    ZERO                TO  ORCSTEN-ZAITEN
                                                           (IDX-D)
                   MOVE    ZERO                TO  ORCSTEN-DOU-ZAITEN
                                                           (IDX-D 1)
                   MOVE    ZERO                TO  ORCSTEN-DOU-ZAITEN
                                                           (IDX-D 2)
               END-IF
           END-PERFORM
      *
           .
       200-KEISAN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    同日再入院点数計算処理
      *****************************************************************
       2009-DOUJIT-ZAITEN-SEC            SECTION.
      *
           PERFORM VARYING     IDXD    FROM    1   BY  1
                   UNTIL       IDXD    >   2
               IF      WRK-DOU-TENSU-GOK (IDXD)    >   ZERO
                   IF      WRK-DOU-TENSU-GOK (IDXD)    <=  15
                       MOVE    1               TO  WRK-SYOKEIS2
                   ELSE
                       COMPUTE WRK-SYOKEIS2    =  ((WRK-DOU-TENSU-GOK
                                                                  (IDXD)
                                               -  15)
                                               /    10 )
                                               +   1
                   END-IF
                   COMPUTE WRK-TENSU           =   WRK-SYOKEIS2
                                               +  0.99999
                   MOVE    WRK-TENSU           TO  ORCSTEN-DOU-ZAITEN
                                                           (IDX-D IDXD)
               ELSE
                   MOVE    ZERO                TO  ORCSTEN-DOU-ZAITEN
                                                           (IDX-D IDXD)
               END-IF
           END-PERFORM
      *
           .
       2009-DOUJIT-ZAITEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数編集テーブル確定処理
      *****************************************************************
       2001-TBL-CHEK-SEC            SECTION.
      *
           PERFORM VARYING     IDX-T   FROM    1   BY  1
                   UNTIL      (IDX-T   >   100  )  OR
                              (TBL-SRYCD (IDX-T)   =   SPACE)
               IF     (TBL-D-SRYCD (IDX-D IDX3)
                                           =   TBL-SRYCD (IDX-T))
                   IF     (TBL-DAY (IDX-T IDX-D)   >   ZERO ) OR
                          (TBL-DAY2 (IDX-T IDX-D)  >   ZERO ) OR
                          (TBL-DAY3 (IDX-T IDX-D)  >   ZERO )
                       OR (TBL-DAY4 (IDX-T IDX-D)  >   ZERO )
                       OR (TBL-DAY5 (IDX-T IDX-D)  >   ZERO )
                       MOVE    1               TO  FLG-TENTEKI
                       ADD     1               TO  IDX
                       IF      IDX             <=  50
                           PERFORM 2001-TBL-HENSYU-SEC
                       END-IF
                   END-IF
                   MOVE    100                 TO  IDX-T
               END-IF
           END-PERFORM
      *
           .
       2001-TBL-CHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数編集処理
      *****************************************************************
       2001-TBL-HENSYU-SEC            SECTION.
      *
           MOVE    TBL-SRYCD (IDX-T)   TO  ORCSTEN-SRYCD (IDX-D IDX)
           MOVE    TBL-SURYO (IDX-T IDX-D)
                                       TO  ORCSTEN-SURYO (IDX-D IDX)
      *H26.10
      *    診療科編集
           MOVE    TBL-SRYKA (IDX-T IDX-D)
                                       TO  ORCSTEN-YK-SRYKA (IDX-D)
      *
           IF      TBL-DAY2 (IDX-T IDX-D)  >   ZERO
               ADD     TBL-SURYO2(IDX-T IDX-D)
                                       TO  ORCSTEN-SURYO (IDX-D IDX)
      *
               IF      ORCSTEN-YK-SRYKA (IDX-D)    =   SPACE
                   MOVE    TBL-SRYKA2 (IDX-T IDX-D)
                                       TO  ORCSTEN-YK-SRYKA (IDX-D)
               END-IF
           END-IF
           IF      TBL-DAY4 (IDX-T IDX-D)  >   ZERO
               ADD     TBL-SURYO4(IDX-T IDX-D)
                                       TO  ORCSTEN-SURYO (IDX-D IDX)
      *
               IF      ORCSTEN-YK-SRYKA (IDX-D)    =   SPACE
                   MOVE    TBL-SRYKA4 (IDX-T IDX-D)
                                       TO  ORCSTEN-YK-SRYKA (IDX-D)
               END-IF
           END-IF
           IF      TBL-DAY3 (IDX-T IDX-D)  >   ZERO
               ADD     TBL-SURYO3(IDX-T IDX-D)
                                       TO  ORCSTEN-SURYO (IDX-D IDX)
      *
               IF      ORCSTEN-YK-SRYKA (IDX-D)    =   SPACE
                   MOVE    TBL-SRYKA3 (IDX-T IDX-D)
                                       TO  ORCSTEN-YK-SRYKA (IDX-D)
               END-IF
           END-IF
           IF      TBL-DAY5 (IDX-T IDX-D)  >   ZERO
               ADD     TBL-SURYO5(IDX-T IDX-D)
                                       TO  ORCSTEN-SURYO (IDX-D IDX)
      *
               IF      ORCSTEN-YK-SRYKA (IDX-D)    =   SPACE
                   MOVE    TBL-SRYKA5 (IDX-T IDX-D)
                                       TO  ORCSTEN-YK-SRYKA (IDX-D)
               END-IF
           END-IF
      *
           COMPUTE WRK-TENSU-KEI       =   TBL-TEN  (IDX-T)
      ******************************   *   TBL-SURYO (IDX-T IDX-D)
                                       *   ORCSTEN-SURYO (IDX-D IDX)
           COMPUTE WRK-TENSU-GOK       =   WRK-TENSU-GOK 
                                       +   WRK-TENSU-KEI
      *****IF      WRK-TENSU-KEI       <=  15
      *        MOVE    1               TO  WRK-SYOKEIS2
      *    ELSE
      *        COMPUTE WRK-SYOKEIS2    =  ((WRK-TENSU-KEI   -  15)
      *                                    /    10 )
      *                                    +   1
      *    END-IF
      *    COMPUTE WRK-TENSU           =   WRK-SYOKEIS2   +  0.999
      *
      *****ADD     WRK-TENSU           TO  ORCSTEN-ZAITEN (IDX-D)
      *    液量
           IF      TBL-DAY (IDX-T IDX-D)  >   ZERO
               ADD     TBL-CHUSYARYO (IDX-T IDX-D)
                                       TO  ORCSTEN-CHUSYARYO (IDX-D)
           END-IF
           IF      TBL-DAY4(IDX-T IDX-D)  >   ZERO
               ADD     TBL-CHUSYARYO4(IDX-T IDX-D)
                                       TO  ORCSTEN-CHUSYARYO (IDX-D)
           END-IF
      *    手術以外判定用
           IF      TBL-DAY (IDX-T IDX-D)  >   ZERO
               ADD     TBL-CHUSYARYO (IDX-T IDX-D)
                                       TO  TBL-CHUSYARYO-2 (IDX-D)
           END-IF
           IF      TBL-DAY4(IDX-T IDX-D)  >   ZERO
               ADD     TBL-CHUSYARYO4(IDX-T IDX-D)
                                       TO  TBL-CHUSYARYO4-2 (IDX-D)
           END-IF
           IF      TBL-DAY3 (IDX-T IDX-D)  >   ZERO
               ADD     TBL-SURYO3(IDX-T IDX-D)
                                       TO  TBL-SURYO3-2 (IDX-D)
           END-IF
           IF      TBL-DAY5 (IDX-T IDX-D)  >   ZERO
               ADD     TBL-SURYO5(IDX-T IDX-D)
                                       TO  TBL-SURYO5-2 (IDX-D)
           END-IF
      *    廃棄あり
           IF      TBL-HAIKI (IDX-T IDX-D) =   1
               MOVE    "099309901"         TO  ORCSTEN-HAIK-SRYCD
                                                     (IDX-D IDX)
               MOVE    TBL-HAIKI-SURYO(IDX-T IDX-D)
                                           TO  ORCSTEN-HAIK-SURYO
                                                     (IDX-D IDX)
           END-IF
      *    廃棄あり（手技なし）
           IF      TBL-HAIKI2(IDX-T IDX-D) =   1
               MOVE    "099309901"         TO  ORCSTEN-HAIK-SRYCD
                                                     (IDX-D IDX)
               ADD     TBL-HAIKI-SURYO2(IDX-T IDX-D)
                                           TO  ORCSTEN-HAIK-SURYO
                                                     (IDX-D IDX)
           END-IF
      *    廃棄あり（中心静脈薬剤）
           IF      TBL-HAIKI3(IDX-T IDX-D) =   1
               MOVE    "099309901"         TO  ORCSTEN-HAIK-SRYCD
                                                     (IDX-D IDX)
               ADD     TBL-HAIKI-SURYO3(IDX-T IDX-D)
                                           TO  ORCSTEN-HAIK-SURYO
                                                     (IDX-D IDX)
           END-IF
      *
      *    生物学的製剤区分
           IF      TBL-SEIBUTU (IDX-T) =   1
               MOVE    1               TO  TBL-S-SEIBUTU (IDX-D)
           END-IF
      *    麻毒
           IF      TBL-MADOKU (IDX-T)  =   1
               MOVE    1               TO  TBL-S-MADOKU (IDX-D)
           END-IF
      *
      *    コメント
           PERFORM VARYING IDX-C1      FROM    1   BY  1
                   UNTIL   IDX-C1      >   5
               IF      TBL-COM-COMMENT (IDX-T IDX-D IDX-C1)
                                      NOT =   SPACE
                   MOVE    TBL-COM-COMMENT (IDX-T IDX-D IDX-C1)
                                          TO  ORCSTEN-COM-MEI
                                               (IDX-D IDX IDX-C1)
                   MOVE    TBL-COM-SRYCD (IDX-T IDX-D IDX-C1)
                                          TO  ORCSTEN-COM-SRYCD
                                               (IDX-D IDX IDX-C1)
               END-IF
           END-PERFORM
      *
      *ver.4.7
      *    同日再入院分
           PERFORM 20012-DOUTBL-HENSYU-SEC
           .
       2001-TBL-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    同日再入院分編集処理
      *****************************************************************
       20012-DOUTBL-HENSYU-SEC         SECTION.
      *
           PERFORM VARYING     IDXD    FROM    1   BY  1
                   UNTIL       IDXD    >   2
               MOVE    TBL-SURYO-D (IDX-T IDX-D IDXD)
                                           TO  WRK-DOU-SURYO
               IF      TBL-DAY2 (IDX-T IDX-D)  >   ZERO
                   ADD     TBL-SURYO2-D(IDX-T IDX-D IDXD)
                                           TO  WRK-DOU-SURYO
               END-IF
               IF      TBL-DAY4 (IDX-T IDX-D)  >   ZERO
                   ADD     TBL-SURYO4-D(IDX-T IDX-D IDXD)
                                           TO  WRK-DOU-SURYO
               END-IF
               IF      TBL-DAY3 (IDX-T IDX-D)  >   ZERO
                   ADD     TBL-SURYO3-D(IDX-T IDX-D IDXD)
                                           TO  WRK-DOU-SURYO
               END-IF
               IF      TBL-DAY5 (IDX-T IDX-D)  >   ZERO
                   ADD     TBL-SURYO5-D(IDX-T IDX-D IDXD)
                                           TO  WRK-DOU-SURYO
               END-IF
      *
               COMPUTE WRK-TENSU-KEI       =   TBL-TEN  (IDX-T)
                                           *   WRK-DOU-SURYO
               COMPUTE WRK-DOU-TENSU-GOK (IDXD)
                                           =   WRK-DOU-TENSU-GOK (IDXD)
                                           +   WRK-TENSU-KEI
           END-PERFORM
           .
       20012-DOUTBL-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    手技料一括変更処理
      *****************************************************************
       300-KOUSIN-SYORI-SEC         SECTION.
      *
      *    患者マスター存在チェック
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    ORCSTEN-PTID        TO  PTINF-PTID
           PERFORM 940-PTINF-READ-SEC
           IF      FLG-PTINF      NOT =   ZERO
               MOVE    9                   TO  ORCSTEN-ERRCD
               GO      TO      300-KOUSIN-SYORI-EXT
           END-IF
      *    年齢計算
      *    ６歳到達日
           MOVE    PTINF-BIRTHDAY      TO  WRK-BYMD
           COMPUTE WRK-BYY             =   WRK-BYY
                                       +   6
      *
      *    すべての手術の有無を判定
           PERFORM 3001-SYUJYU-HEN-SEC
      *
      *    手術以外の点滴判定
           PERFORM 3009-SYUJYU-CHECK-SEC
      *
      *    中心静脈処理
           PERFORM 3005-CHUSIN-HEN-SEC
      *
      *    今回分液量集計編集
           PERFORM VARYING    IDX-D    FROM    1   BY  1
                   UNTIL      IDX-D    >   31
      *        最大液量と対象診療コード
               PERFORM 3004-TENSRYCD-HEN-SEC
      *        液量が対象で手術なしの時
               IF     (ORCSTEN-CHUSYARYO (IDX-D) >=  WRK-MAX-RYOU) AND
                      (TBL-SYU-ARI       (IDX-D) =   ZERO        )
      *            手技料あり
                   PERFORM 49004-SYUGI-ARI-SEC
               ELSE
      *            手技料なし
                   MOVE    ZERO            TO  FLG-CHU-ARI
                   PERFORM 49005-SYUGI-NASI-SEC
               END-IF
           END-PERFORM
      *    加算の未算定チェック
           PERFORM 49007-MISAN-CHK-SEC
      *
      *    一括変更の時
           IF      ORCSTEN-KBN         =   "1"
      *        マスタ更新
               PERFORM 49006-SRYACCT-UPDATE-SEC
           END-IF
      *
           .
       300-KOUSIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    手術編集処理
      *****************************************************************
       3001-SYUJYU-HEN-SEC           SECTION.
      *
      *    診療年月の診療会計マスタ読み込み
           MOVE    SPACE               TO  SRYACCT-REC
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    ORCSTEN-PTID        TO  ACCT-PTID
           MOVE    ORCSTEN-SRYYM       TO  ACCT-SRYYM
           MOVE    "50"                TO  ACCT-SRYKBN
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           PERFORM UNTIL       (FLG-SRYACCT         =   1       )
               MOVE    ZERO                TO  FLG-YUKETU
      *        レセプト編集に包括診療は対象外
               IF     (ORCSTEN-KBN         =   "2"   )
                 AND  (ACCT-JIHOKBN        =   "1"   )
                   MOVE    1                   TO  FLG-YUKETU
               END-IF
      *        輸血の判定
               IF     (ACCT-SYUTEN1        >   ZERO   )
      *        包括まとめ入力は対象外とする
                 AND  (ACCT-HKNCOMBI   NOT =   "9999" )
                 AND  (ACCT-ZAIKBN     NOT =   1      )
                 AND  (FLG-YUKETU          =   ZERO   )
                   PERFORM 30031-SRYACT-YUKETU-SEC
               ELSE
                   MOVE    1                   TO  FLG-YUKETU
               END-IF
      *        手技料があるもの
               IF     (FLG-YUKETU          =   ZERO  )  AND
                      (ACCT-SYUTEN1        >   ZERO  )
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL       IDX2    >   31
      *                手術有
                       IF      ACCT-DAY (1 IDX2)   >   ZERO
                           ADD     ACCT-DAY (1 IDX2)   TO  TBL-SYU-ARI
                                                                 (IDX2)
                       END-IF
                   END-PERFORM
               END-IF
      *
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3001-SYUJYU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     診療行為より輸血の判定処理
      *****************************************************************
       30031-SRYACT-YUKETU-SEC         SECTION.
      *
      *
      *    診療会計マスターから診療行為マスターを検索する
           MOVE    SPACE               TO  SRYACT-REC
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
               PERFORM 920-SRYACT-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    ZERO                TO  FLG-YUKETU
           IF      FLG-SRYACT          =   ZERO
      *        輸血の時対象外
               IF      SRY-SRYSYUKBN       =   "510"
                   MOVE    1                   TO  FLG-YUKETU
               END-IF
           END-IF
      *
           .
       30031-SRYACT-YUKETU-EXT.
           EXIT.
      *
      *****************************************************************
      *    手術以外の点滴判定処理
      *****************************************************************
       3009-SYUJYU-CHECK-SEC           SECTION.
      *
           PERFORM VARYING    IDX-D    FROM    1   BY  1
                   UNTIL      IDX-D    >   31
               MOVE    TBL-SYU-ARI (IDX-D) TO  TBL-SYU-ARI2(IDX-D)
               IF      TBL-SYU-ARI (IDX-D) >   ZERO
      *            手術以外の判定
                   IF     (TBL-CHUSYARYO-2 (IDX-D) >   ZERO ) AND
                          (TBL-CHUSYARYO4-2(IDX-D) =   ZERO )
                       CONTINUE
                   ELSE
                       MOVE    ZERO            TO  TBL-SYU-ARI (IDX-D)
                   END-IF
                   IF      TBL-CHUSYARYO4-2(IDX-D) >   ZERO
                       MOVE    TBL-CHUSYARYO4-2(IDX-D)
                                           TO  ORCSTEN-CHUSYARYO (IDX-D)
                   END-IF
               END-IF
               IF      TBL-SYU-ARI2(IDX-D) >   ZERO
      *            中心静脈
                   IF     (TBL-SURYO3-2 (IDX-D)    >   ZERO) AND
                          (TBL-SURYO5-2 (IDX-D)    =   ZERO)
                       CONTINUE
                   ELSE
                       MOVE    ZERO            TO  TBL-SYU-ARI2(IDX-D)
                   END-IF
               END-IF
           END-PERFORM
           .
       3009-SYUJYU-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    中心静脈処理
      *****************************************************************
       3005-CHUSIN-HEN-SEC           SECTION.
      *
           PERFORM VARYING    IDX-D    FROM    1   BY  1
                   UNTIL      IDX-D    >   31
               MOVE    ZERO                TO  FLG-OK
               MOVE    SPACE               TO  WRK-HKNCOMBI
               MOVE    SPACE               TO  WRK-SRYKA
               PERFORM VARYING    IDY      FROM    1   BY  1
                       UNTIL     (IDY      >   50 )  OR
                                 (FLG-OK   =   1  )
      *            中心静脈の薬剤判定
                   IF     (TBL-SYU-ARI2(IDX-D)     =   ZERO ) AND
                         ((TBL-DAY3 (IDY IDX-D)    >   ZERO ) OR
                          (TBL-DAY5 (IDY IDX-D)    >   ZERO ))
                       MOVE    TBL-HKNCOMBI3 (IDY IDX-D)
                                                   TO  WRK-HKNCOMBI
                       MOVE    TBL-SRYKA3    (IDY IDX-D)
                                                   TO  WRK-SRYKA
                       MOVE    1                   TO  FLG-OK
                   END-IF
               END-PERFORM
      *!!
      *H28.6
      *        中心静脈注射(植入型カテーテル）算定あり
               IF     (TBL-CHU6-ARI(IDX-D) >   ZERO )
                   MOVE    ZERO            TO  FLG-OK
      *            当日の中心静脈有
                   IF      TBL-CHU-ARI (IDX-D) >   ZERO
                       MOVE    9               TO  ORCSTEN-MIIVH-DAY
                                                              (IDX-D)
                   END-IF
               END-IF
      *
               IF      FLG-OK              =   1
      *            今回対象あり
                   PERFORM 30052-CHU-ARI-SEC
                   MOVE    1               TO  TBL-CHU-ARI (IDX-D)
               END-IF
           END-PERFORM
           .
       3005-CHUSIN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    中心静脈手技料あり編集処理
      *****************************************************************
       30052-CHU-ARI-SEC            SECTION.
      *
      *    対象の剤判定
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING    IDY      FROM    1   BY  1
                   UNTIL     (IDY      >   50 )  OR
                             (TBL2-SRYCD(IDY)  =   SPACE)  OR
                             (FLG-OK           =   1    )
               IF     (TBL2-DAY (IDY IDX-D)    >   ZERO )  AND
                      (TBL2-IVHFLG (IDY)       =   1    )
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
      *    当日の中心静脈なし
           IF      FLG-OK              =   ZERO
               MOVE    1               TO  ORCSTEN-MIIVH-DAY (IDX-D)
           END-IF
      *
           .
       30051-CHU-NASI-EXT.
           EXIT.
      *
      *****************************************************************
      *    点滴手技料と対象液量編集処理
      *****************************************************************
       3004-TENSRYCD-HEN-SEC           SECTION.
      *
           EVALUATE    TRUE
      *        ６歳未満
               WHEN    WRK-BYMD(1:6)       >   ORCSTEN-SRYYM
                   MOVE    "130003710"     TO  WRK-SRYCD
                   MOVE    100             TO  WRK-MAX-RYOU
      *        ６歳
               WHEN    WRK-BYMD(1:6)       =   ORCSTEN-SRYYM
                   IF      WRK-BDD         <=  IDX-D
                       MOVE    "130003810"     TO  WRK-SRYCD
                       MOVE    500             TO  WRK-MAX-RYOU
                   ELSE
                       MOVE    "130003710"     TO  WRK-SRYCD
                       MOVE    100             TO  WRK-MAX-RYOU
                   END-IF
      *        ６歳以上
               WHEN    WRK-BYMD(1:6)       <   ORCSTEN-SRYYM
                   MOVE    "130003810"     TO  WRK-SRYCD
                   MOVE    500             TO  WRK-MAX-RYOU
           END-EVALUATE
      *
           .
       3004-TENSRYCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    手技料あり編集処理
      *****************************************************************
       49004-SYUGI-ARI-SEC             SECTION.
      *
      *    中心静脈あり
           IF      TBL-CHU-ARI (IDX-D) =   1
             OR   (TBL-CHU6-ARI(IDX-D) =   1 )
               MOVE    1               TO  FLG-CHU-ARI
      *        他の点滴手技料を削除する
               PERFORM 49005-SYUGI-NASI-SEC
               GO      TO      49004-SYUGI-ARI-EXT
           END-IF
      *
      *    対象の剤判定
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  FLG-SEIBUTU-OK
           MOVE    ZERO                TO  FLG-MADOKU-OK
           PERFORM VARYING    IDY      FROM    1   BY  1
                   UNTIL      IDY      >   50
      *        手技料
               IF     (TBL2-DAY (IDY IDX-D)    >   ZERO )  AND
                      (TBL2-DATAKBN (IDY)      =   "1"  )  AND
                      (TBL2-SEIBUTU (IDY)      =   ZERO )  AND
                      (TBL2-MADOKU  (IDY)      =   ZERO )  AND
                      (TBL2-IVHFLG  (IDY)      =   ZERO )
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
      *
      *    手技料未算定
           IF      FLG-OK              =   ZERO
               MOVE    1               TO  ORCSTEN-MI-DAY (IDX-D)
               GO      TO      49004-SYUGI-ARI-EXT
           END-IF
      *
           .
       49004-SYUGI-ARI-EXT.
           EXIT.
      *
      *****************************************************************
      *    手技料なし編集処理
      *****************************************************************
       49005-SYUGI-NASI-SEC            SECTION.
      *
      *    対象の剤判定
           PERFORM VARYING    IDY      FROM    1   BY  1
                   UNTIL     (IDY      >   50 )  OR
                             (TBL2-SRYCD(IDY)  =   SPACE)
               MOVE    ZERO                TO  FLG-OK
               IF     (TBL2-DAY (IDY IDX-D)    >   ZERO )  AND
                      (TBL2-IVHFLG (IDY)       =   ZERO )
      *            当日の中心静脈点滴あり
                   IF      TBL-CHU-ARI (IDX-D) =   1
                     OR   (TBL-CHU6-ARI(IDX-D) =   1 )
      *************IF      FLG-CHU-ARI         =   1
                       IF     (TBL-S-SEIBUTU (IDX-D)   =   1  )  AND
                              (TBL2-SEIBUTU  (IDY)     =   1  )
                           MOVE    1               TO  FLG-OK
                       END-IF
                       IF     (TBL-S-MADOKU (IDX-D)    =   1  )  AND
                              (TBL2-MADOKU  (IDY)      =   1  )
                           MOVE    1               TO  FLG-OK
                       END-IF
                   END-IF
                   IF      FLG-OK              =   ZERO
                       IF      TBL2-YAKU-ARI(IDY IDX-D)    =   ZERO
      *                    当日削除
                           PERFORM 490051-SYUGI-DEL-SEC
                       ELSE
      *                    薬剤と混在の為削除エラー
                           MOVE    1           TO  ORCSTEN-HENFLG2
                       END-IF
                       MOVE    ZERO            TO  TBL2-DAY (IDY IDX-D)
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       49005-SYUGI-NASI-EXT.
           EXIT.
      *
      *****************************************************************
      *    手技料削除編集処理
      *****************************************************************
       490051-SYUGI-DEL-SEC           SECTION.
      *
           PERFORM VARYING     IDX-H   FROM    1   BY  1
                   UNTIL       IDX-H   >   50
               IF      TBL-HEN-ZAINUM (IDX-H)  =   ZERO
                   MOVE    TBL2-ZAINUM (IDY IDX-D)
                                           TO  TBL-HEN-ZAINUM(IDX-H)
                   MOVE    TBL2-SRYKA  (IDY IDX-D)
                                           TO  TBL-HEN-SRYKA (IDX-H)
                   MOVE    TBL2-HKNCOMBI(IDY IDX-D)
                                           TO  TBL-HEN-HKNCOMBI
                                                             (IDX-H)
               END-IF
      *        削除
               IF      TBL-HEN-ZAINUM (IDX-H)  =   TBL2-ZAINUM
                                                          (IDY IDX-D)
                   MOVE    1               TO  TBL-HEN-DEL (IDX-H IDX-D)
      *            変更あり
                   MOVE    1               TO  ORCSTEN-HENFLG
      *
                   MOVE    50              TO  IDX-H
               END-IF
           END-PERFORM
      *
           .
       490051-SYUGI-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    加算の未算定チェック処理
      *****************************************************************
       49007-MISAN-CHK-SEC           SECTION.
      *
           PERFORM VARYING    IDX-D    FROM    1   BY  1
                   UNTIL      IDX-D    >   31
      *
      *        対象の剤判定
               MOVE    ZERO                TO  FLG-OK
               MOVE    ZERO                TO  FLG-SEIBUTU-OK
               MOVE    ZERO                TO  FLG-MADOKU-OK
               PERFORM VARYING    IDY      FROM    1   BY  1
                       UNTIL     (IDY      >   50  )  OR
                                 (TBL2-SRYCD(IDY)  =   SPACE )
      *            手技料
                   IF    ((TBL2-DAY (IDY IDX-D)    >   ZERO )  AND
                          (TBL2-DATAKBN (IDY)      =   "1"  )  AND
                          (TBL2-SEIBUTU (IDY)      =   ZERO )  AND
                          (TBL2-MADOKU  (IDY)      =   ZERO )  AND
                          (TBL2-IVHFLG  (IDY)      =   ZERO ))  OR
                         ( TBL-CHU-ARI (IDX-D)     =   1    )
                    OR   ( TBL-CHU6-ARI (IDX-D)     =   1    )
                       MOVE    1                   TO  FLG-OK
                   END-IF
      *            加算
                   IF     (TBL2-DAY (IDY IDX-D)    >   ZERO )  AND
                          (TBL2-SEIBUTU (IDY)      =   1    )  AND
                          (TBL-S-SEIBUTU (IDX-D)   =   1    )
                       MOVE    1                   TO  FLG-SEIBUTU-OK
                   END-IF
                   IF     (TBL2-DAY (IDY IDX-D)    >   ZERO )  AND
                          (TBL2-MADOKU (IDY)       =   1    )  AND
                          (TBL-S-MADOKU (IDX-D)    =   1    )
                       MOVE    1                   TO  FLG-MADOKU-OK
                   END-IF
               END-PERFORM
      *
      *        手技料未算定
               IF     (FLG-OK              =   1       )  AND
                      (ORCSTEN-MI-DAY (IDX-D)  =   ZERO )
      *            生物学的
                   IF      TBL-S-SEIBUTU (IDX-D)   =   1
                       IF      FLG-SEIBUTU-OK      =   ZERO
                           MOVE    2               TO  ORCSTEN-MI-DAY
                                                              (IDX-D)
                       END-IF
                   END-IF
      *            麻毒
                   IF      TBL-S-MADOKU (IDX-D)    =   1
                       IF      FLG-MADOKU-OK       =   ZERO
                           MOVE    2               TO  ORCSTEN-MI-DAY
                                                               (IDX-D)
                       END-IF
                   END-IF
               END-IF
      *
      *        手技料算定
               IF     (ORCSTEN-MI-DAY (IDX-D)  =   ZERO )
      *            生物学的
                   IF      FLG-SEIBUTU-OK      =   1
                       IF     (FLG-OK          =   ZERO )  OR
                              (TBL-S-SEIBUTU (IDX-D)   =   ZERO)
                           MOVE    3               TO  ORCSTEN-MI-DAY
                                                              (IDX-D)
                       END-IF
                   END-IF
      *            麻毒
                   IF      FLG-MADOKU-OK       =   1
                       IF     (FLG-OK          =   ZERO )  OR
                              (TBL-S-MADOKU (IDX-D)    =   ZERO)
                           MOVE    3               TO  ORCSTEN-MI-DAY
                                                              (IDX-D)
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       49007-MISAN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計更新処理
      *****************************************************************
       49006-SRYACCT-UPDATE-SEC           SECTION.
      *
           PERFORM VARYING    IDY      FROM    1   BY  1
                   UNTIL     (IDY      >   50 )  OR
                             (TBL-HEN-ZAINUM (IDY) =   ZERO) OR
                             (ORCSTEN-ERRCD        =   1   )
               INITIALIZE                      SRYACCT-REC
               MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
               MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
               MOVE    ORCSTEN-PTID        TO  ACCT-PTID
               MOVE    TBL-HEN-SRYKA (IDY) TO  ACCT-SRYKA
               MOVE    ORCSTEN-SRYYM       TO  ACCT-SRYYM
               MOVE    "33"                TO  ACCT-SRYKBN
               MOVE    TBL-HEN-ZAINUM(IDY) TO  ACCT-ZAINUM
      *
               MOVE    SRYACCT-REC         TO  MCPDATA-REC
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 930-SRYACCT-READ-SEC
               ELSE
                   INITIALIZE                  SRYACCT-REC
                   MOVE    1               TO  FLG-SRYACCT
               END-IF
               IF      FLG-SRYACCT          =   ZERO
      *            受診履歴変更
                   PERFORM 490063-SANTEI-DEL-SEC
      *            診療会計マスターを変更する
                   PERFORM 490061-DAY-DEL-SEC
      *
      *
                   MOVE    SMCNDATE-YMD        TO  ACCT-UPYMD
                   MOVE    SMCNDATE-HMS        TO  ACCT-UPHMS
      *
                   MOVE    SRYACCT-REC         TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                        DISPLAY "ORCSTENTEKI SRYACCT UPD ERR:" MCP-RC
                                ",KEY:" ACCT-KEY
                        MOVE    1               TO  ORCSTEN-ERRCD
                   END-IF
               ELSE
                   DISPLAY "ORCSTENTEKI SRYACCT READ ERR:" ACCT-KEY
               END-IF
      *
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *
           END-PERFORM
      *
           .
       49006-SRYACCT-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスター変更編集処理
      *****************************************************************
       490061-DAY-DEL-SEC           SECTION.
      *
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
              IF      TBL-HEN-DEL (IDY IDX-D) =   1
      *            剤回数　変更
                   COMPUTE ACCT-ZAIKAISU       =   ACCT-ZAIKAISU
                                               -   ACCT-DAY
                                                       (1  IDX-D)
      *v48         MOVE    ZERO                TO  ACCT-DAY (1 IDX-D)
      *            MOVE    ZERO                TO  ACCT-DAY (2 IDX-D)
      *            MOVE    ZERO                TO  ACCT-DAY (3 IDX-D)
      *            MOVE    ZERO                TO  ACCT-DAY (4 IDX-D)
                   PERFORM VARYING    IDZ      FROM    1   BY  1
                           UNTIL      IDZ      >   10
                       MOVE    ZERO            TO  ACCT-DAY (IDZ IDX-D)
                   END-PERFORM
               END-IF
           END-PERFORM
           .
       490061-DAY-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴変更処理
      *****************************************************************
      *490062-JYURRK-HEN-SEC           SECTION.
      *
      *    受診歴を検索する  医療機関、患者ＩＤ、伝票番号
      *    INITIALIZE                  JYURRK-REC
      *    MOVE    ACCT-HOSPNUM         TO  JYURRK-HOSPNUM
      *    MOVE    ACCT-PTID           TO  JYURRK-PTID
      *    MOVE    ACCT-NYUGAIKBN      TO  JYURRK-NYUGAIKBN
      *    MOVE    ACCT-SRYKA          TO  JYURRK-SRYKA 
      *    MOVE    ACCT-SRYYM          TO  JYURRK-SRYYMD(1:6)
      *    MOVE    IDX-D               TO  JYURRK-SRYYMD(7:2)
      *
      *    MOVE    JYURRK-REC          TO  MCPDATA-REC
      *    MOVE    "tbl_jyurrk"        TO  MCP-TABLE
      *    MOVE    "key4"              TO  MCP-PATHNAME
      *    PERFORM 910-DBSELECT-SEC
      *    IF      MCP-RC              =   ZERO
      *        MOVE    "tbl_jyurrk"        TO  MCP-TABLE
      *        MOVE    "key4"              TO  MCP-PATHNAME
      *        PERFORM 970-JYURRK-READ-SEC
      *    ELSE
      *        INITIALIZE                  JYURRK-REC
      *        MOVE    1               TO  FLG-JYURRK
      *    END-IF
      *
      *    INITIALIZE                  TBL-JYURRK-AREA
      *    MOVE    ZERO                TO  IDX-H
      *    PERFORM UNTIL  (FLG-JYURRK         =   1  )  OR
      *                   (IDX-H              >   15 )
      *        ADD     1                   TO  IDX-H
      *        MOVE    JYURRK-REC          TO  TBL-JYURRK-REC (IDX-H)
      *
      *        MOVE    "tbl_jyurrk"        TO  MCP-TABLE
      *        MOVE    "key4"              TO  MCP-PATHNAME
      *        PERFORM 970-JYURRK-READ-SEC
      *    END-PERFORM
      *
      *    MOVE    "tbl_jyurrk"        TO  MCP-TABLE
      *    MOVE    "key4"              TO  MCP-PATHNAME
      *    PERFORM 990-DBCLOSE-SEC
      *
      *    PERFORM VARYING IDX         FROM    1   BY  1
      *            UNTIL   IDX         >   IDX-H
      *        PERFORM VARYING IDX2        FROM    1   BY  1
      *                UNTIL   IDX2        >   15
      *            IF      TBL-JYURRK-ZAINUM(IDX IDX2)
      *                                    =   TBL-HEN-ZAINUM(IDY)
      *                MOVE    ZERO        TO  TBL-JYURRK-ZAINUM
      *                                                    (IDX IDX2)
      *            END-IF
      *            IF      TBL-JYURRK-ZAINUM(IDX IDX2) =   ZERO
      *                IF       IDX2           <   15
      *                    COMPUTE IDX3        =   IDX2    +   1
      *                    MOVE    TBL-JYURRK-ZAINUM(IDX IDX3)
      *                                        TO  TBL-JYURRK-ZAINUM
      *                                                   (IDX IDX2)
      *                    MOVE    ZERO        TO  TBL-JYURRK-ZAINUM
      *                                                   (IDX IDX3)
      *                ELSE
      *                    IF      IDX         <   IDX-H
      *                        COMPUTE IDX3        =   IDX-H   +   1
      *                        MOVE    TBL-JYURRK-ZAINUM(IDX3 1)
      *                                            TO  TBL-JYURRK-ZAINUM
      *                                                   (IDX IDX2)
      *                        MOVE    ZERO        TO  TBL-JYURRK-ZAINUM
      *                                                   (IDX3 1)
      *                    END-IF
      *                END-IF
      *            END-IF
      *        END-PERFORM
      *    END-PERFORM
      *    ＤＢ更新
      *    PERFORM VARYING IDX         FROM    1   BY  1
      *            UNTIL   IDX         >   IDX-H
      *        MOVE    TBL-JYURRK-REC (IDX-H)  TO  JYURRK-REC
      *
      *        MOVE    SMCNDATE-YMD        TO  JYURRK-UPYMD
      *        MOVE    SMCNDATE-HMS        TO  JYURRK-UPHMS
      *
      *        MOVE    JYURRK-REC          TO  MCPDATA-REC
      *        MOVE    "DBUPDATE"          TO  MCP-FUNC
      *        MOVE    "JYURRK-KEY"        TO  ORC-DBPATH
      *        CALL    "ORCMCPSUB"          USING
      *                                     MCPAREA
      *                                     ORCMCPAREA
      *                                     MCPDATA-REC
      *         IF      MCP-RC          NOT =   ZERO
      *             DISPLAY "ORCSTENTEKI JYURRK  UPD ERR:" MCP-RC
      *                     ",KEY:" JYURRK-KEY
      *              MOVE    2               TO  ORCSTEN-ERRCD
      *         END-IF
      *    END-PERFORM
      *
      *    .
      *490062-JYURRK-HEN-EXT.
      **** EXIT.
      *
      *****************************************************************
      *    算定履歴更新処理
      *****************************************************************
       490063-SANTEI-DEL-SEC           SECTION.
      *
      *    診療会計マスターから診療行為マスターを検索する
           MOVE    SPACE               TO  SRYACT-REC
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
               PERFORM 920-SRYACT-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           PERFORM UNTIL      FLG-SRYACT   =   1
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2        >   5   )  OR
                                  (SRY-SRYCD(IDX2)     =   SPACE )
      *            点数マスタ
                   MOVE    SRY-SRYCD (IDX2)    TO  TNS-SRYCD
                   PERFORM 910-TENSU-READ-SEC
      *            算定履歴を削除する
                   IF      TNS-SANTEIRRKKBN    NOT =   ZERO
                       PERFORM 49021-SANTEI-UPD-SEC
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SRYACT-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       490063-SANTEI-DEL-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    算定履歴更新処理
      *****************************************************************
       49021-SANTEI-UPD-SEC           SECTION.
      *
      *    保険組合せ
           MOVE    ACCT-HKNCOMBI       TO  SPA-HKNCOMBINUM
      *    保険内容編集
           PERFORM 510-HKNCOMBI-MEI-SEC
      *
           INITIALIZE                  ORCSSANTEIAREA
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
              IF      TBL-HEN-DEL (IDY IDX-D) =   1
      *            今回分
                   MOVE    ACCT-DAY(1  IDX-D)
                                           TO  SSANTEI-DAY (IDX-D)
               END-IF
           END-PERFORM
      *    削除
           MOVE    "3"                 TO  SSANTEI-SYORI
           MOVE    "J02"               TO  SSANTEI-PG
           MOVE    SRY-SRYCD (IDX2)    TO  SSANTEI-SRYCD
           MOVE    ACCT-SRYYM          TO  SSANTEI-SRYYMD(1:6)
           MOVE    IDX-D               TO  SSANTEI-SRYYMD(7:2)
           MOVE    ACCT-PTID           TO  SSANTEI-PTID
           MOVE    ACCT-NYUGAIKBN      TO  SSANTEI-NYUGAIKBN
           MOVE    ACCT-SRYKA          TO  SSANTEI-SRYKA
           CALL    "ORCSSANTEI"        USING
                                       ORCSSANTEIAREA
                                       SPA-AREA
      *
           .
       49021-SANTEI-UPD-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ名称編集処理
      *****************************************************************
       510-HKNCOMBI-MEI-SEC           SECTION.
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
               PERFORM 940-HKNCOMBI-NEXT-SEC
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *    保険区分編集
           EVALUATE    COMB-HKNNUM
      *        労災
               WHEN    "971"
      *        自賠責
               WHEN    "973"
                   MOVE    1                   TO  SPA-HKNKBN
               WHEN    OTHER
      *        健保
                   MOVE    ZERO                TO  SPA-HKNKBN
           END-EVALUATE
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       510-HKNCOMBI-MEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    ORCSTEN-SRYYM       TO  TNS-YUKOSTYMD(1:6)
           MOVE    WRK-DD              TO  TNS-YUKOSTYMD(7:2)
           MOVE    TNS-YUKOSTYMD       TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
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
      *    附加情報の上限回数設定
           IF     (FLG-TENSU           =   ZERO ) AND
                  (TNS-SRYCD(1:1)      =   "1"  )
               PERFORM 9101-TENSUPLUS-SYORI-SEC
           END-IF
           .
       910-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ付加情報より編集処理
      *****************************************************************
       9101-TENSUPLUS-SYORI-SEC          SECTION.
      *
           MOVE    SPACE               TO  TENSUPLUS-REC
           INITIALIZE                      TENSUPLUS-REC
           MOVE    TNS-SRYCD           TO  TNSP-SRYCD
      **   MOVE    TNS-YUKOSTYMD       TO  TNSP-YUKOSTYMD
      **   MOVE    TNS-YUKOEDYMD       TO  TNSP-YUKOEDYMD
           MOVE    ORCSTEN-SRYYM       TO  TNSP-YUKOSTYMD(1:6)
           MOVE    WRK-DD              TO  TNSP-YUKOSTYMD(7:2)
           MOVE    TNSP-YUKOSTYMD      TO  TNSP-YUKOEDYMD
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
                   MOVE    1                   TO  FLG-TENSUPLUS
                   INITIALIZE                      TENSUPLUS-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSUPLUS
               INITIALIZE                      TENSUPLUS-REC
           END-IF
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    上限回数
           IF      TNSP-SANTEIRRKKBN   =   1
               MOVE    TNSP-SANTEIRRKKBN   TO  TNS-SANTEIRRKKBN
               MOVE    TNSP-JGNCNT         TO  TNS-JGNCNT
               MOVE    TNSP-JGNCNT1D       TO  TNS-JGNCNT1D
               MOVE    TNSP-JGNCNTERR      TO  TNS-JGNCNTERR
           END-IF
           .
       9101-TENSUPLUS-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       920-SRYACT-NEXT-SEC         SECTION.
      *
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
       920-SRYACT-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスター読込
      *****************************************************************
       930-SRYACCT-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
               MOVE    ZERO                TO  FLG-SRYACCT
           ELSE
               INITIALIZE                      SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           .
       930-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       940-PTINF-READ-SEC         SECTION.
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
                   MOVE    MCPDATA-REC        TO  PTINF-REC
                   MOVE    ZERO               TO  FLG-PTINF
               ELSE
                   MOVE    1                  TO  FLG-PTINF
               END-IF
           ELSE
               MOVE    1              TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       940-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込
      *****************************************************************
       940-HKNCOMBI-NEXT-SEC         SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
               MOVE    ZERO                TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                      HKNCOMBI-REC
               MOVE    1                   TO  FLG-HKNCOMBI
           END-IF
      *
           .
       940-HKNCOMBI-NEXT-EXT.
           EXIT.
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
           PERFORM  990-DBCLOSE-SEC
      *
           .
       950-PTCOM-READ-EXT.
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
      *    ＤＢＣＬＯＳＥ処理
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
