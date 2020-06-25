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
       PROGRAM-ID.                 ORCSCHOZAIPRV.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院会計照会
      *  コンポーネント名  : 入院用入院調剤料更新処理
      *                    : リアルタイムプレビュー用に作成
      *  管理者            :
      *  作成日付    作業者        記述
      *  05/06/30    NACL−竹田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-xxxxxx  yy/mm/dd
      *                           NYUINACCT PTNYUINRRK は保留
      *  02.09.01    NACL-竹田    06/04/17
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *  04.03.00    NACL-多々納  08/08/01  保険組合せ期間チェック追加
      *  04.04.00    NACL-多々納  08/12/17  入院料包括チェック対応
      *  04.05.00    NACL-多々納  09/12/03  前月保険組合せ終了対応
      *  04.07.00    NACL-多々納  11/10/XX  同日再入院対応
      *  04.07.00    NACL-多々納  12/08/XX  サンプル薬（サ）対応
      *  04.07.00    NACL-多々納  14/04/01  出来高算定（剤）等対応
      *  04.08.00    NACL-多々納  16/10/XX  H28.10月途中で麻毒変更対応
      *  05.00.00    NACL-多々納  19/11/XX  前月診療行為の反映対応
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
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-NYUINACCT       PIC 9(01).
           03  FLG-PTNYUINRRK      PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
      *
           03  FLG-ARI             PIC 9(01).
           03  FLG-MADOKU          PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
      *
           03  FLG-SYORI           PIC 9(01).
           03  FLG-CHK             PIC 9(01).
      *
           03  FLG-SRYACCT-DEL     PIC 9(01).
      *
           03  FLG-NASI            PIC 9(01).
           03  FLG-HKN-CHK         PIC 9(01).
           03  FLG-HKTCHK          PIC 9(01).
           03  FLG-HEN             PIC 9(01).
      *
           03  FLG-NYUIN01         PIC 9(01).
           03  FLG-ARI2            PIC 9(01).
      *ver.4.7
           03  FLG-DEKIDA          PIC 9(01).
           03  FLG-HOKATU          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX5                PIC 9(04).
           03  IDY                 PIC 9(04).
      *
           03  IDX-T               PIC 9(04).
           03  IDX-H               PIC 9(04).
           03  IDX-D               PIC 9(02).
           03  IDY2                PIC 9(02).
      *
           03  IDX-S               PIC 9(02).
           03  IDX-E               PIC 9(02).
           03  IDX-TAI             PIC 9(04).
      *
           03  IDX-TOU             PIC 9(03).
      *
           03  IDX-N               PIC 9(04).
           03  IDZ                 PIC 9(04).
      *ver.4.7
           03  IDX4                PIC 9(04).
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
           03  WRK-MAX             PIC 9(02).
      *
           03  WRK-SRYCD               PIC X(09).
           03  WRK-SRYCD-9             PIC 9(09).
      *
      *    末日
           03  WRK-MATU                PIC 9(02).
           03  WRK-SRYYM.
               05  WRK-SRYYY           PIC 9(04).
               05  WRK-SRYMM           PIC 9(02).
           03  WRK-SRYKBN              PIC X(02).
           03  WRK-KAISU               PIC 9(03).
      *    翌月
           03  WRK-MATU-NEXT           PIC 9(02).
           03  WRK-SRYYM-N.
               05  WRK-SRYYY-N         PIC 9(04).
               05  WRK-SRYMM-N         PIC 9(02).
      *
           03  WRK-IDX2                PIC 9(04).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-SRYSYUKBN           PIC X(03).
           03  WRK-HKNCOMBI            PIC X(04).
           03  WRK-JIHOKBN             PIC X(01).
           03  WRK-ZAINUM              PIC 9(08).
           03  WRK-DAY-AREA.
               05  WRK-DAY-TBL         OCCURS   10.
                    07  WRK-DAY        PIC 9(03)
                                           OCCURS  31.
      *
           03  WRK-STYMD.
               05  WRK-STYM.
                   07  WRK-STYY        PIC 9(04).
                   07  WRK-STMM        PIC 9(02).
               05  WRK-STDD            PIC 9(02).
           03  WRK-HKNERR              PIC X(01).
      *ver.4.7
           03  WRK-DDAY                PIC 9(02).
           03  WRK-RENNUM              PIC 9(01).
      *
      *H26.7
      *    前月からの入院日
           03  WRK-NYUINYMD            PIC X(08).
           03  WRK-CHK-SRYYMD.
               05  WRK-CHK-SRYYM       PIC X(06).
               05  WRK-CHK-DD          PIC 9(02).
      *H26.7
      *H28.10
      *    麻薬開始
           03  WRK-MADOKU-ST           PIC 9(02).
      *    マスタ開始日付
           03  WRK-YKSTYMD.
               05  WRK-YKSTYM          PIC X(06).
               05  WRK-YKSTDD          PIC 9(02).
           03  WRK-HZN-DD              PIC 9(02).
      *
           03  WRK-MCP-TABLE               PIC X(64).
           03  WRK-MCP-TABLE2              PIC X(64).
      *
      *    投薬日
       01  TBL-TOUYAKU-AREA.
           03  TBL-TOUYAKU-OCC1        OCCURS   3.
               05  TBL-TOUYAKU-OCC         OCCURS   31.
                   07  TBL-TOU-DAY         PIC 9(01).
                   07  TBL-TOU-AREA        OCCURS   10.
                       09  TBL-TOU-SRYKA       PIC X(02).
                       09  TBL-TOU-HKNCOMBI    PIC X(04).
                       09  TBL-TOU-HKNERR      PIC X(01).
               05  TBL-DOKUYAKU-OCC        OCCURS   31.
                   07  TBL-DOK-DAY         PIC 9(01).
                   07  TBL-DOK-AREA        OCCURS   10.
                       09  TBL-DOK-SRYKA       PIC X(02).
                       09  TBL-DOK-HKNCOMBI    PIC X(04).
                       09  TBL-DOK-HKNERR      PIC X(01).
      *    調剤料
       01  TBL-CHOZAI-AREA.
           03  TBL-CHOZAI-OCC         OCCURS   10.
               05  TBL-CHO-ZAINUM          PIC 9(08).
               05  TBL-CHO-SRYKA           PIC X(02).
               05  TBL-CHO-HKNCOMBI        PIC X(04).
               05  TBL-CHO-JIHOKBN         PIC X(01).
               05  TBL-CHO-ADD             PIC X(01).
               05  TBL-CHO-SYORI           PIC X(01).
      *        作成
               05  TBL-CHO-DAY-AREA.
                   07  TBL-CHO-DAY-TBL     OCCURS   10.
                        09  TBL-CHO-DAY    PIC 9(03)
                                           OCCURS  31.
      *    麻毒料
       01  TBL-MADOKU-AREA.
           03  TBL-MADOKU-OCC         OCCURS   10.
               05  TBL-MAD-ZAINUM          PIC 9(08).
               05  TBL-MAD-SRYKA           PIC X(02).
               05  TBL-MAD-HKNCOMBI        PIC X(04).
               05  TBL-MAD-JIHOKBN         PIC X(01).
               05  TBL-MAD-ADD             PIC X(01).
               05  TBL-MAD-SYORI           PIC X(01).
      *        作成
               05  TBL-MAD-DAY-AREA.
                   07  TBL-MAD-DAY-TBL     OCCURS   10.
                        09  TBL-MAD-DAY    PIC 9(03)
                                           OCCURS  31.
      *    外泊
       01  TBL-GAIHAKU-AREA.
           03  TBL-GAIHAKU-OCC         OCCURS   31.
               05  TBL-GAI-DAY         PIC 9(01).
      *    入院期間
       01  TBL-NYUIN-AREA.
           03  TBL-NYUIN-OCC         OCCURS   31.
               05  TBL-NYU-DAY         PIC 9(01).
               05  TBL-NYU-SRYKA       PIC X(02).
               05  TBL-NYU-HKNCOMBI    PIC X(04).
      *
      *    保険組合せ期間
       01  TBL-HKNCOMBI-AREA.
           03  TBL-HKNCOMBI-OCC         OCCURS   50.
               05  TBL-HKNCOMBINUM         PIC X(04).
               05  TBL-TEKSTYMD            PIC X(08).
               05  TBL-TEKEDYMD            PIC X(08).
      *
      *入院料包括
       01  TBL-NYUAREA.
           03  TBL-HKTCHKFLG-G.
               05  TBL-HKTCHKFLG-OCC        OCCURS   31.
                   07  TBL-HKTCHKFLG        PIC 9(01).
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
      *    入院診療会計マスタ−
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    診療会計マスタ−
       01  HZN-SRYACCT-REC.
           COPY    "CPSRYACCT.INC"  REPLACING
                                    //ACCT-// BY //HZN-ACCT-//.
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
      *   算定履歴更新サブ
           COPY    "CPORCSSANTEI.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    入院料包括診療チェクパラメタ
           COPY    "CPORCSCHKTALL.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
           COPY    "MCPAREA".
      **** COPY    "ORCA-SPA".
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
           COPY    "CPORCSCHOZAI.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCSCHOZAIAREA
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
           INITIALIZE                  TBL-TOUYAKU-AREA
           INITIALIZE                  TBL-CHOZAI-AREA
           INITIALIZE                  TBL-MADOKU-AREA
           INITIALIZE                  TBL-GAIHAKU-AREA
      *
           INITIALIZE                  ORCSCHO-ERRCD
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    "1"                 TO  SPA-NYUGAIKBN
      *
      *    入院・外泊の編集
           PERFORM 200-GAIHAKU-HEN-SEC
      *    保険組合せの編集
           PERFORM 200-HKNCOMBI-HEN-SEC
      *
           EVALUATE    ORCSCHO-KBN
      *        通常
               WHEN    "1"
      *            入院調剤料と麻毒の確定
                   PERFORM 300-CHOZAI-HEN-SEC
      *            投薬の日数編集
                   PERFORM 100-TOUYAKU-HEN-SEC
      *            入院調剤料と麻毒の更新
                   PERFORM 400-SANTEI-SYORI-SEC
      *        退院処理
               WHEN    "2"
                   PERFORM 500-TAIIN-SYORI-SEC
           END-EVALUATE
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    投薬の日数編集処理
      *****************************************************************
       100-TOUYAKU-HEN-SEC             SECTION.
      *
           INITIALIZE                  TBL-TOUYAKU-AREA
           MOVE    ORCSCHO-SRYYM       TO  WRK-SRYYM
      *    前１月分
           COMPUTE WRK-SRYMM           =   WRK-SRYMM   -   1
           IF      WRK-SRYMM           =   ZERO
               COMPUTE WRK-SRYYY       =   WRK-SRYYY   -   1
               MOVE    12              TO  WRK-SRYMM
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
      *        診療年月の末日算定
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY7-AREA
               MOVE    "71"                TO  LNK-DAY7-IRAI
               MOVE    WRK-SRYYM           TO  LNK-DAY7-YM
               CALL    "ORCSDAY"           USING
                                           STS-AREA-DAY
                                           LNK-DAY7-AREA
               MOVE    LNK-DAY7-KOYOMI(7:2)    TO  WRK-MATU
      *        診療年月＋１月の末日算定
               COMPUTE WRK-SRYMM-N         =   WRK-SRYMM   +   1
               IF      WRK-SRYMM-N         >   12
                   COMPUTE WRK-SRYYY-N     =   WRK-SRYYY   +   1
                   MOVE    1                   TO  WRK-SRYMM-N
               ELSE
                   MOVE    WRK-SRYYY           TO  WRK-SRYYY-N
               END-IF
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY7-AREA
               MOVE    "71"                TO  LNK-DAY7-IRAI
               MOVE    WRK-SRYYM-N         TO  LNK-DAY7-YM
               CALL    "ORCSDAY"           USING
                                           STS-AREA-DAY
                                           LNK-DAY7-AREA
               MOVE    LNK-DAY7-KOYOMI(7:2)    TO  WRK-MATU-NEXT
      *
               IF     (FLG-NYUIN01         =   1   )  AND
                      (WRK-SRYYM       NOT =   ORCSCHO-SRYYM )
                   CONTINUE
               ELSE
                   MOVE    "21"                TO  WRK-SRYKBN
                   PERFORM 10011-SRYACCT-21HEN-SEC
               END-IF
      *        次月
               COMPUTE WRK-SRYMM           =   WRK-SRYMM   +   1
               IF      WRK-SRYMM           >   12
                   COMPUTE WRK-SRYYY       =   WRK-SRYYY   +   1
                   MOVE    1               TO  WRK-SRYMM
               END-IF
      *
           END-PERFORM
      *    頓服
           MOVE    "22"                TO  WRK-SRYKBN
           MOVE    ORCSCHO-SRYYM       TO  WRK-SRYYM
           PERFORM 10012-SRYACCT-HEN-SEC
      *    外用
           MOVE    "23"                TO  WRK-SRYKBN
           MOVE    ORCSCHO-SRYYM       TO  WRK-SRYYM
           PERFORM 10012-SRYACCT-HEN-SEC
      *
           .
       100-TOUYAKU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    投薬の編集処理
      *****************************************************************
       10011-SRYACCT-21HEN-SEC             SECTION.
      *
      *R01.11
           IF      ORCSCHO-SRYYM       =   WRK-SRYYM
      *        当月分
               MOVE    "tbl_sryacct_prv"   TO  WRK-MCP-TABLE
               MOVE    "tbl_sryact_prv"    TO  WRK-MCP-TABLE2
           ELSE
      *        前月分
               MOVE    "tbl_sryacct"       TO  WRK-MCP-TABLE
               MOVE    "tbl_sryact"        TO  WRK-MCP-TABLE2
           END-IF
      *
      *    診療年月の診療会計マスタ読み込み
           MOVE    SPACE               TO  SRYACCT-REC
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    ORCSCHO-PTID        TO  ACCT-PTID
           MOVE    WRK-SRYYM           TO  ACCT-SRYYM
           MOVE    "21"                TO  ACCT-SRYKBN
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
      ***  MOVE    "tbl_sryacct_prv"   TO  MCP-TABLE
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
      ***      MOVE    "tbl_sryacct_prv"   TO  MCP-TABLE
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           PERFORM
               UNTIL       (FLG-SRYACCT         =   1       )
      *????  IF     (ACCT-YKZTEN       >   ZERO  )
      *        包括まとめ入力は対象外とする
      *??      AND  (ACCT-HKNCOMBI NOT =   "9999" )
      ***    IF     (ACCT-HKNCOMBI NOT =   "9999" )
      *        薬評は対象外とする
      ***      AND  (ACCT-ZAIKBN   NOT =   1      )
      *H28.9
               IF     (ACCT-HKNCOMBI     =   "9999" )
      *          薬評は対象外とする
                 OR   (ACCT-ZAIKBN       =   1      )
      *          持参薬は対象外とする
                 OR   (ACCT-ZAIKBN       =   3      )
                 OR   (ACCT-ZAIKBN       =   4      )
                   CONTINUE
               ELSE
      *
      *H26.7
      *        前月分で入院日が診療年月以前の時
               IF      (WRK-SRYYM      NOT =   ORCSCHO-SRYYM)
                   AND (WRK-NYUINYMD   NOT =   SPACE)
                   MOVE    ACCT-SRYYM       TO  WRK-CHK-SRYYM
               ELSE
                   MOVE    SPACE            TO  WRK-CHK-SRYYM
               END-IF
      *
      *
      *        最初の日を保存（点数マスタ検索のため）
               MOVE    01                  TO  WRK-DD
               PERFORM VARYING     IDY2    FROM    1   BY  1
                       UNTIL       IDY2    >   31
                   IF      ACCT-DAY (1 IDY2)   >   ZERO
                       MOVE    IDY2                TO  WRK-DD
      *
      *H26.7           前月の入院日より前の算定はなし
                       IF      WRK-CHK-SRYYM       NOT =   SPACE
                           MOVE    IDY2            TO  WRK-CHK-DD
                           IF     (WRK-NYUINYMD    >   WRK-CHK-SRYYMD)
                               MOVE    ZERO        TO  ACCT-DAY (1 IDY2)
                               MOVE    ZERO        TO  ACCT-DAY (2 IDY2)
                               MOVE    ZERO        TO  ACCT-DAY (3 IDY2)
                           END-IF
                       END-IF
      *!!
                   END-IF
               END-PERFORM
      *        診療行為より麻毒判定
               PERFORM 600111-SRYACT-HEN-SEC
      *
               IF      FLG-NASI            =   ZERO
      *        回数により保存
               PERFORM VARYING     IDY2    FROM    1   BY  1
                       UNTIL       IDY2    >   31
      *            同日入院
                   MOVE    ZERO                TO  WRK-RENNUM
                   MOVE    ZERO                TO  WRK-DDAY
                   MOVE    ZERO                TO  WRK-KAISU
                   IF     (IDX                 =  2  )  AND
                          (TBL-NYU-DAY (IDY2)  =  3  )
                       IF      ACCT-DAY (3 IDY2)   >   ZERO
                           MOVE    ACCT-DAY (3 IDY2)   TO  WRK-KAISU
                           MOVE    IDY2                TO  WRK-DDAY
                           IF      ACCT-DAY (2 IDY2)   =   ZERO
                               MOVE    1               TO  WRK-RENNUM
                           END-IF
                       ELSE
      **                   MOVE    ACCT-DAY (2 IDY2)   TO  WRK-KAISU
      *                    同日退院分は、１回のみとする
                           IF      ACCT-DAY (2 IDY2)   >   ZERO
                               MOVE    1               TO  WRK-KAISU
                           END-IF
                       END-IF
                   ELSE
                       MOVE    ACCT-DAY (1 IDY2)   TO  WRK-KAISU
                   END-IF
      *            投薬の日が入院中であること
      *********    IF      ACCT-DAY (1 IDY2)   >   ZERO
                   IF      WRK-KAISU           >   ZERO
      ***************  MOVE    ACCT-DAY (1 IDY2)   TO  WRK-KAISU
                       PERFORM VARYING IDX2    FROM    1   BY   1
                               UNTIL   IDX2    >   WRK-KAISU
                           COMPUTE IDX3        =   IDY2
                                               +   IDX2
                                               -   1
                           IF      IDX3        >   WRK-MATU
      *                        翌月へ
                               COMPUTE IDX3        =   IDX3
                                                   -   WRK-MATU
      *                        ２月以上またがらないこととする
      *                        （翌月末日まで）
                               IF      IDX3        >   WRK-MATU-NEXT
                                   MOVE    WRK-MATU-NEXT   TO  IDX3
                               END-IF
                               COMPUTE IDX1            =   IDX   +   1
                           ELSE
                               MOVE    IDX             TO  IDX1
                           END-IF
      *                    当月で退院以降はなしとする
                           IF     (IDX1                =  2  )  AND
                                  (TBL-NYU-DAY (IDX3)  =   ZERO)
                               MOVE    WRK-KAISU       TO  IDX2
                           ELSE
      *ver.4.7                 算定あり編集
                               PERFORM 100110-TOUDAY-HEN-SEC
      *                        退院日の時、以降はなしとする
                               IF     (IDX1                =  2  )  AND
                                     ((TBL-NYU-DAY (IDX3)  =  2  )  OR
                                      (TBL-NYU-DAY (IDX3)  =  3  AND
                                       IDX3                >  IDY2))
                                   MOVE    WRK-KAISU       TO  IDX2
                               END-IF
                           END-IF
                       END-PERFORM
                   END-IF
               END-PERFORM
      *
             END-IF
      *
             END-IF
      *
      **       MOVE    "tbl_sryacct_prv"   TO  MCP-TABLE
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-READ-SEC
           END-PERFORM
      *
      **   MOVE    "tbl_sryacct_prv"   TO  MCP-TABLE
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       10011-SRYACCT-21HEN-EXT.
           EXIT.
      *****************************************************************
      *    投薬有編集処理
      *****************************************************************
       100110-TOUDAY-HEN-SEC             SECTION.
      *
           IF     (WRK-DDAY            =   IDX3 )
             AND  (WRK-RENNUM          =   1    )
      *        同日再入院日で退院分がない時
               IF      TBL-TOU-DAY (IDX1 IDX3) =   ZERO
                   MOVE    2           TO  TBL-TOU-DAY (IDX1 IDX3)
      *            診療科・保険編集処理（投薬）
                   PERFORM 100111-TOU-SRYKAHEN-SEC
               END-IF
           ELSE
               MOVE    1               TO  TBL-TOU-DAY (IDX1 IDX3)
      *        診療科・保険編集処理（投薬）
               PERFORM 100111-TOU-SRYKAHEN-SEC
           END-IF
      *    麻毒算定
           IF      FLG-MADOKU          =   1
      *H28.10
            AND   (WRK-MADOKU-ST       <=  IDX3  )
               IF     (WRK-DDAY            =   IDX3 )
                 AND  (WRK-RENNUM          =   1    )
      *            同日再入院日で退院分がない時
                   IF      TBL-DOK-DAY (IDX1 IDX3) =   ZERO
                       MOVE    2           TO  TBL-DOK-DAY (IDX1 IDX3)
                       PERFORM 100112-DOK-SRYKAHEN-SEC
                   END-IF
               ELSE
                   MOVE    1               TO  TBL-DOK-DAY (IDX1 IDX3)
                   PERFORM 100112-DOK-SRYKAHEN-SEC
               END-IF
           END-IF
           .
       100110-TOUDAY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療科・保険編集処理（投薬）
      *****************************************************************
       100111-TOU-SRYKAHEN-SEC             SECTION.
      *
           MOVE    ZERO                 TO  FLG-HKN-CHK
           MOVE    WRK-SRYYM            TO  WRK-STYM
           MOVE    IDX3                 TO  WRK-STDD
           IF      IDX             NOT  =   IDX1
      *        翌月
               COMPUTE WRK-STMM         =   WRK-STMM   +   1
               IF      WRK-STMM         >   12
                   COMPUTE WRK-STYY         =   WRK-STYY   +   1
                   MOVE    1                TO  WRK-STMM
               END-IF
           END-IF
      *    保険組合せチェック
           MOVE    ACCT-HKNCOMBI       TO  WRK-HKNCOMBI
           PERFORM 100111-HKNCOMBI-CHK-SEC
      *    前月分で保険組合せがない場合
           MOVE    ACCT-HKNCOMBI       TO  WRK-HKNCOMBI
           IF     (FLG-HKN-CHK         =   ZERO          ) AND
                  (ACCT-SRYYM      NOT =   ORCSCHO-SRYYM )
               PERFORM VARYING     IDX5    FROM    1   BY  1
                       UNTIL      (IDX5    >   10  )  OR
                                  (TBL-CHO-ZAINUM(IDX5)    =   ZERO )
                   IF      TBL-CHO-SRYKA (IDX5)    =   ACCT-SRYKA
                       IF      TBL-CHO-DAY (IDX5 1 IDX3)   >   ZERO
                           MOVE    TBL-CHO-HKNCOMBI (IDX5)
                                                   TO  WRK-HKNCOMBI
                           MOVE    10              TO  IDX5
                        END-IF
                   END-IF
               END-PERFORM
               PERFORM 100111-HKNCOMBI-CHK-SEC
           END-IF
      *
           PERFORM VARYING     IDX-T    FROM    1   BY  1
                   UNTIL       IDX-T    >   10
               IF      TBL-TOU-SRYKA (IDX1 IDX3 IDX-T) =   SPACE
                   MOVE    ACCT-SRYKA      TO  TBL-TOU-SRYKA
                                                    (IDX1 IDX3 IDX-T)
      *******      MOVE    ACCT-HKNCOMBI   TO  TBL-TOU-HKNCOMBI
                   MOVE    WRK-HKNCOMBI    TO  TBL-TOU-HKNCOMBI
                                                    (IDX1 IDX3 IDX-T)
                   IF      FLG-HKN-CHK         =   ZERO
                       MOVE    "1"             TO  TBL-TOU-HKNERR
                                                    (IDX1 IDX3 IDX-T)
                   END-IF
               END-IF
               IF     (TBL-TOU-SRYKA (IDX1 IDX3 IDX-T)
                                               =   ACCT-SRYKA  )   AND 
                      (TBL-TOU-HKNCOMBI(IDX1 IDX3 IDX-T)
                                               =   ACCT-HKNCOMBI)
                   MOVE    10              TO  IDX-T
               END-IF
           END-PERFORM
      *
           .
       100111-TOU-SRYKAHEN-EXT.
           EXIT.
      *****************************************************************
      *    保険チェック処理
      *****************************************************************
       100111-HKNCOMBI-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-HKN-CHK
           PERFORM VARYING     IDX-H   FROM    1   BY  1
                   UNTIL       IDX-H   >   50
               IF      ACCT-HKNCOMBI   =   TBL-HKNCOMBINUM (IDX-H)
                   IF     (WRK-STYMD       <   TBL-TEKSTYMD (IDX-H))
                      OR  (WRK-STYMD       >   TBL-TEKEDYMD (IDX-H))
                       CONTINUE
                   ELSE
                       MOVE    1               TO  FLG-HKN-CHK
                   END-IF
                   MOVE    50               TO  IDX-H
               END-IF
           END-PERFORM
           .
       100111-HKNCOMBI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療科・保険編集処理（麻毒）
      *****************************************************************
       100112-DOK-SRYKAHEN-SEC             SECTION.
      *
           PERFORM VARYING     IDX-T    FROM    1   BY  1
                   UNTIL       IDX-T    >   10
               IF      TBL-DOK-SRYKA (IDX1 IDX3 IDX-T) =   SPACE
                   MOVE    ACCT-SRYKA      TO  TBL-DOK-SRYKA
                                                       (IDX1 IDX3 IDX-T)
      *********    MOVE    ACCT-HKNCOMBI   TO  TBL-DOK-HKNCOMBI
                   MOVE    WRK-HKNCOMBI    TO  TBL-DOK-HKNCOMBI
                                                       (IDX1 IDX3 IDX-T)
                   IF      FLG-HKN-CHK         =   ZERO
                       MOVE    "1"             TO  TBL-DOK-HKNERR
                                                    (IDX1 IDX3 IDX-T)
                   END-IF
               END-IF
               IF     (TBL-DOK-SRYKA (IDX1 IDX3 IDX-T)
                                               =   ACCT-SRYKA  )   AND 
                      (TBL-DOK-HKNCOMBI(IDX1 IDX3 IDX-T)
                                               =   ACCT-HKNCOMBI)
                   MOVE    10              TO  IDX-T
               END-IF
           END-PERFORM
      *
           .
       100112-DOK-SRYKAHEN-EXT.
           EXIT.
      *****************************************************************
      *    頓服・外用の編集処理
      *****************************************************************
       10012-SRYACCT-HEN-SEC             SECTION.
      *
      *
           IF      ORCSCHO-SRYYM       =   WRK-SRYYM
               MOVE    "tbl_sryacct_prv"   TO  WRK-MCP-TABLE
               MOVE    "tbl_sryact_prv"    TO  WRK-MCP-TABLE2
           ELSE
               MOVE    "tbl_sryacct"       TO  WRK-MCP-TABLE
               MOVE    "tbl_sryact"        TO  WRK-MCP-TABLE2
           END-IF
      *
      *    診療年月の診療会計マスタ読み込み
           MOVE    SPACE               TO  SRYACCT-REC
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    ORCSCHO-PTID        TO  ACCT-PTID
           MOVE    WRK-SRYYM           TO  ACCT-SRYYM
           MOVE    WRK-SRYKBN          TO  ACCT-SRYKBN
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
      **   MOVE    "tbl_sryacct_prv"   TO  MCP-TABLE
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
      **       MOVE    "tbl_sryacct_prv"   TO  MCP-TABLE
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           PERFORM
               UNTIL       (FLG-SRYACCT         =   1       )
      *?????   IF      ACCT-YKZTEN       >   ZERO
      *        包括まとめ入力は対象外とする
      *????      AND  (ACCT-HKNCOMBI NOT =   "9999" )
      **       IF     (ACCT-HKNCOMBI NOT =   "9999" )
      *        薬評は対象外とする
      **         AND  (ACCT-ZAIKBN   NOT =   1      )
      *H28.9
               IF     (ACCT-HKNCOMBI     =   "9999" )
      *          薬評は対象外とする
                 OR   (ACCT-ZAIKBN       =   1      )
      *          持参薬は対象外とする
                 OR   (ACCT-ZAIKBN       =   3      )
                 OR   (ACCT-ZAIKBN       =   4      )
                   CONTINUE
               ELSE
      *
      *            最初の日を保存（点数マスタ検索のため）
                   MOVE    01                  TO  WRK-DD
                   PERFORM VARYING     IDY2    FROM    1   BY  1
                           UNTIL       IDY2    >   31
                       IF      ACCT-DAY (1 IDY2)   >   ZERO
                           MOVE    IDY2                TO  WRK-DD
                       END-IF
                   END-PERFORM
      *            診療行為より麻毒判定
                   PERFORM 600111-SRYACT-HEN-SEC
                 IF      FLG-NASI            =   ZERO
      *
      *            回数により保存
                   PERFORM VARYING     IDX3    FROM    1   BY  1
                           UNTIL       IDX3    >   31
                       IF      ACCT-DAY (1 IDX3)   >   ZERO
      *ver.4.7
                           MOVE    ZERO            TO  WRK-DDAY
                           MOVE    ZERO            TO  WRK-RENNUM
                           IF      ACCT-DAY (3 IDX3)   >   ZERO
                               MOVE    IDX3            TO  WRK-DDAY
                               IF      ACCT-DAY (2 IDX3)   =   ZERO
                                   MOVE    1           TO  WRK-RENNUM
                               END-IF
                           END-IF
      *ver.4.7             MOVE    1               TO  TBL-TOU-DAY
      *                                                     (2  IDX3)
                           MOVE    2               TO  IDX1
                           PERFORM 100110-TOUDAY-HEN-SEC
      *                    PERFORM 100111-TOU-SRYKAHEN-SEC
      *                    IF      FLG-MADOKU      =   1
      *                        MOVE    1               TO  TBL-DOK-DAY
      *                                                     (2  IDX3)
      *                        PERFORM 100112-DOK-SRYKAHEN-SEC
      *                    END-IF
                       END-IF
                   END-PERFORM
                 END-IF
      *
               END-IF
      *
      **       MOVE    "tbl_sryacct_prv"   TO  MCP-TABLE
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-READ-SEC
           END-PERFORM
      *
      **   MOVE    "tbl_sryacct_prv"   TO  MCP-TABLE
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       10012-SRYACCT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為マスター編集処理
      *****************************************************************
       600111-SRYACT-HEN-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-ARI
           MOVE    ZERO                TO  FLG-ARI2
           MOVE    ZERO                TO  FLG-MADOKU
           MOVE    ZERO                TO   WRK-MADOKU-ST
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
      **   MOVE    "tbl_sryact_prv"    TO  MCP-TABLE
           MOVE    WRK-MCP-TABLE2      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
      **       MOVE    "tbl_sryact_prv"    TO  MCP-TABLE
               MOVE    WRK-MCP-TABLE2      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SRYACT-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *    入院調剤料なしを対象外とする
           IF     (FLG-SRYACT          =   ZERO )  AND
                  (SRY-SRYSYUKBN       =   "214"
                                       OR  "224"
                                       OR  "234"
      *     院外処方対象外
                                       OR  "212"
                                       OR  "222"
                                       OR  "232"
                                       OR  "292"
      *H26.4   外用（臨時）
                                       OR  "298"
                                       OR  "295" )
               MOVE    1                   TO  FLG-SRYACT
               MOVE    1                   TO  FLG-NASI
           ELSE
               MOVE    ZERO                TO  FLG-NASI
           END-IF
      *
           PERFORM UNTIL      FLG-SRYACT   =   1
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2            >   5   )  OR
      ************                (FLG-MADOKU      =   1    )  OR
                                  (SRY-SRYCD(IDX2) =   SPACE )
      *            薬剤
                   IF      SRY-SRYCD (IDX2)(1:1)   =   "6"
                       MOVE    1                   TO  FLG-ARI
      *                点数マスタ
                       MOVE    SRY-SRYCD (IDX2)    TO  TNS-SRYCD
                       PERFORM 910-TENSU-READ-SEC
      *
      *H28.10
                       IF      TNS-MADOKUKBN       NOT =   ZERO
                           IF     (TNS-YUKOSTYMD(7:2)  NOT =  "01")
                             AND  (TNS-YUKOSTYMD(1:6)
                                                     =   ORCSCHO-SRYYM)
                               MOVE    TNS-YUKOSTYMD
                                                     TO  WRK-YKSTYMD
      *                        月途中麻毒開始日
                               PERFORM 6001110-TENSU-CHK-SEC
                               IF     (WRK-MADOKU-ST   =   ZERO )
                                  OR  (WRK-MADOKU-ST   >   WRK-YKSTDD)
                                   MOVE    WRK-YKSTDD
                                                   TO   WRK-MADOKU-ST
                               END-IF
                           ELSE
                               MOVE    01          TO   WRK-MADOKU-ST
                           END-IF
                           MOVE    1               TO  FLG-MADOKU
                       END-IF
                   END-IF
      *ver.4.7
      *            サンプル薬コメント
                   IF      SRY-SRYCD (IDX2)    =   "820000046"
                       MOVE    1                   TO  FLG-ARI2
                   END-IF
               END-PERFORM
      *
      **       MOVE    "tbl_sryact_prv"    TO  MCP-TABLE
               MOVE    WRK-MCP-TABLE2      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SRYACT-NEXT-SEC
           END-PERFORM
      *
      **   MOVE    "tbl_sryact_prv"    TO  MCP-TABLE
           MOVE    WRK-MCP-TABLE2      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *ver.4.7
           IF     (ACCT-YKZTEN       =   ZERO )
              AND (FLG-ARI2          =   ZERO )
               MOVE    1                   TO  FLG-NASI
           END-IF
      *
           .
       600111-SRYACT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    月途中麻毒開始日処理
      *****************************************************************
       6001110-TENSU-CHK-SEC             SECTION.
      *
      *    １日の算定
           MOVE    WRK-DD              TO  WRK-HZN-DD
           MOVE    01                  TO  WRK-DD
           MOVE    SRY-SRYCD (IDX2)    TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
      *
           IF      TNS-MADOKUKBN       NOT =   ZERO
      *        １日分が麻毒の時、１日から
               MOVE    01              TO  WRK-YKSTDD
           END-IF
           MOVE    WRK-HZN-DD          TO  WRK-DD
      *
           .
       6001110-TENSU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院・外泊の編集処理
      *****************************************************************
       200-GAIHAKU-HEN-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUIN01
      *    診療会計マスターを特定する
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  NACCT-NYUGAIKBN
           MOVE    ORCSCHO-PTID        TO  NACCT-PTID
           MOVE    ORCSCHO-SRYYM       TO  NACCT-SRYYM
           MOVE    "2"                 TO  NACCT-ZAISKBKBN
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM 930-NYUINACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF
           PERFORM
               UNTIL   FLG-NYUINACCT       =   1
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   31
      *************IF      NACCT-DAY (IDX) >   ZERO
      *            外泊のゼロと４は対象外
                   IF     (NACCT-DAY (IDX)  =  ZERO)
                   OR     (NACCT-DAY (IDX) >=   4  )
                       CONTINUE
                   ELSE
                       MOVE    NACCT-DAY (IDX)     TO  TBL-GAI-DAY(IDX)
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM 930-NYUINACCT-READ-SEC
           END-PERFORM
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    入院履歴判定
           MOVE    SPACE               TO  PTNYUINRRK-REC
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    ORCSCHO-PTID        TO  PTNYUINRRK-PTID
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key33"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key33"             TO  MCP-PATHNAME
               PERFORM 910-PTNYUINRRK-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
           INITIALIZE                  TBL-NYUIN-AREA
           MOVE    ZERO                TO  FLG-OK
           PERFORM
                   UNTIL      (FLG-PTNYUINRRK      =   1 )  OR
                              (FLG-OK              =   1 )  OR
                              (PTNYUINRRK-TAIINYMD(1:6)
                                                   <  ORCSCHO-SRYYM)
      *        入院日から９０日以上の判定
               MOVE    ZERO                    TO  WRK-MAX
      ******** IF      PTNYUINRRK-NYUINYMD(1:6)    <   ORCSCHO-SRYYM
      *            INITIALIZE                      STS-AREA-DAY
      *            INITIALIZE                      LNK-DAY6-AREA
      *            MOVE    "61"                TO  LNK-DAY6-IRAI
      *            MOVE    PTNYUINRRK-NYUINYMD TO  LNK-DAY6-KIJUN
      *            MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
      *            MOVE    90                  TO  LNK-DAY6-ZOGEN
      *            CALL    "ORCSDAY"           USING   STS-AREA-DAY
      *                                                LNK-DAY6-AREA
      *            IF      LNK-DAY6-KOYOMI(1:6)    =   ORCSCHO-SRYYM
      *                MOVE    LNK-DAY6-KOYOMI(7:2)    TO  WRK-MAX
      *            END-IF
      *            IF      LNK-DAY6-KOYOMI(1:6)    <   ORCSCHO-SRYYM
      *                MOVE    31                      TO  WRK-MAX
      *            END-IF
      ******** END-IF
      *
      *        入院中の時
               MOVE    ZERO                TO  IDX-S
               MOVE    ZERO                TO  IDX-E
               IF     (PTNYUINRRK-NYUINYMD(1:6)  <   ORCSCHO-SRYYM) AND
                      (PTNYUINRRK-TAIINYMD(1:6)  >   ORCSCHO-SRYYM)
                   MOVE    1               TO  IDX-S
                   MOVE    31              TO  IDX-E
                   MOVE    1               TO  FLG-OK
               END-IF
      *        入院日
               IF      PTNYUINRRK-NYUINYMD(1:6)  =   ORCSCHO-SRYYM
                   MOVE    PTNYUINRRK-NYUINYMD(7:2)  TO  IDX-S
      *            当月１日入院日
                   IF      IDX-S                 =   1
                       MOVE    1                     TO  FLG-NYUIN01
                   END-IF
               END-IF
               IF      PTNYUINRRK-NYUINYMD(1:6)  <   ORCSCHO-SRYYM
                   MOVE    1                         TO  IDX-S
      *H26.7
      *            前月からの入院日
                   MOVE    PTNYUINRRK-NYUINYMD   TO  WRK-NYUINYMD
      *H26.7
               END-IF
      *        退院日
               IF     PTNYUINRRK-TAIINYMD(1:6)   =   ORCSCHO-SRYYM
                   MOVE    PTNYUINRRK-TAIINYMD(7:2)  TO  IDX-E
                   MOVE    IDX-E                 TO  IDX-TAI
               END-IF
               IF    (PTNYUINRRK-TAIINYMD(1:6)   >   ORCSCHO-SRYYM) OR
                     (PTNYUINRRK-TAIINYMD        =   ALL "9"  )
                   MOVE    31                        TO  IDX-E
               END-IF
      *
               IF     (IDX-S               >   ZERO )  AND
                      (IDX-E               >   ZERO )
                   PERFORM VARYING     IDX     FROM    IDX-S  BY  1
                           UNTIL       IDX     >   IDX-E
      *****            MOVE    1                   TO  TBL-NYU-DAY(IDX)
      *                退院日設定
                       IF      IDX                 =   IDX-TAI
                           IF      TBL-NYU-DAY(IDX)    =   1
                               MOVE    3           TO  TBL-NYU-DAY(IDX)
                           ELSE
                               MOVE    2           TO  TBL-NYU-DAY(IDX)
                           END-IF
                       ELSE
                           MOVE    1               TO  TBL-NYU-DAY(IDX)
                       END-IF
      *                入院の科と保険
                       MOVE    PTNYUINRRK-NYUINKA  TO  TBL-NYU-SRYKA
                                                                  (IDX)
                       MOVE    PTNYUINRRK-HKNCOMBINUM
                                                   TO  TBL-NYU-HKNCOMBI
                                                                  (IDX)
                   END-PERFORM
               END-IF
      *
               IF      WRK-MAX             >   ZERO
                   ADD     1               TO  WRK-MAX
                   PERFORM VARYING     IDX     FROM    WRK-MAX BY  1
                           UNTIL       IDX     >   31
                       MOVE    ZERO                TO  TBL-NYU-DAY(IDX)
                   END-PERFORM
               END-IF
      *
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key33"             TO  MCP-PATHNAME
               PERFORM 910-PTNYUINRRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key33"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       200-GAIHAKU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せの編集処理
      *****************************************************************
       200-HKNCOMBI-HEN-SEC             SECTION.
      *
           INITIALIZE                  TBL-HKNCOMBI-AREA
      *    保険組合マスター編集
           MOVE    SPACE               TO  HKNCOMBI-REC
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-PTID            TO  COMB-PTID
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-HKNCOMBI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM
               UNTIL      (FLG-HKNCOMBI        =   1      )  OR
                          (IDX                 >=  50     )
      *        対象の保険組合せのみ編集
               IF     (COMB-DELKBN     NOT =   "1"   )  AND
      *              ((SPA-SRYYMD(1:6)     >=  COMB-TEKSTYMD(1:6))
      *          AND  (SPA-SRYYMD(1:6)     <=  COMB-TEKEDYMD(1:6)))
                     ((ORCSCHO-SRYYM       >=  COMB-TEKSTYMD(1:6))
                 AND  (ORCSCHO-SRYYM       <=  COMB-TEKEDYMD(1:6)))
                   ADD     1               TO  IDX
                   MOVE    COMB-HKNCOMBINUM    TO  TBL-HKNCOMBINUM
                                                               (IDX)
                   MOVE    COMB-TEKSTYMD       TO  TBL-TEKSTYMD
                                                               (IDX)
                   MOVE    COMB-TEKEDYMD       TO  TBL-TEKEDYMD
                                                               (IDX)
               END-IF
      *
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-HKNCOMBI-READ-SEC
           END-PERFORM
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       200-HKNCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院調剤料と麻毒の確定処理
      *****************************************************************
       300-CHOZAI-HEN-SEC             SECTION.
      *
      *    調剤料
           MOVE    ZERO                TO  IDX1
           INITIALIZE                  TBL-CHOZAI-AREA
      *
      *    診療年月の診療会計マスタ読み込み
           MOVE    SPACE               TO  SRYACCT-REC
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    ORCSCHO-PTID        TO  ACCT-PTID
           MOVE    ORCSCHO-SRYYM       TO  ACCT-SRYYM
           MOVE    "24"                TO  ACCT-SRYKBN
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct_prv"   TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct_prv"   TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           PERFORM
               UNTIL       (FLG-SRYACCT         =   1 )  OR
                           (IDX1                >=  10 )
               IF      ACCT-SYUTEN1        >   ZERO
      *            包括まとめ入力は対象外とする
                  AND  (ACCT-HKNCOMBI NOT =   "9999" )
      *           薬評は対象外とする
                  AND  (ACCT-ZAIKBN   NOT =   1      )
      *
                   ADD     1                   TO  IDX1
                   MOVE    ACCT-ZAINUM         TO  TBL-CHO-ZAINUM(IDX1)
                   MOVE    ACCT-SRYKA          TO  TBL-CHO-SRYKA (IDX1)
                   MOVE    ACCT-HKNCOMBI       TO  TBL-CHO-HKNCOMBI
                                                                 (IDX1)
                   MOVE    ACCT-JIHOKBN        TO  TBL-CHO-JIHOKBN
                                                                 (IDX1)
                   MOVE    ACCT-DAY-AREA       TO  TBL-CHO-DAY-AREA
                                                                 (IDX1)
      *            手入力は内容をチェック
                   IF      ACCT-SRYCDTOTAL     >   1
                       PERFORM 3001-CHOZAI-CHEK-SEC
                       IF     (FLG-DEKIDA      =   1  )
                          OR  (FLG-HOKATU      =   1  )
                           MOVE    "1"         TO  TBL-CHO-SYORI (IDX1)
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "tbl_sryacct_prv"   TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryacct_prv"   TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    麻毒料
           MOVE    ZERO                TO  IDX1
           INITIALIZE                  TBL-MADOKU-AREA
      *
      *    診療年月の診療会計マスタ読み込み
           MOVE    SPACE               TO  SRYACCT-REC
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    ORCSCHO-PTID        TO  ACCT-PTID
           MOVE    ORCSCHO-SRYYM       TO  ACCT-SRYYM
           MOVE    "26"                TO  ACCT-SRYKBN
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct_prv"   TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct_prv"   TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           PERFORM
               UNTIL       (FLG-SRYACCT         =   1 )  OR
                           (IDX1                >=  10 )
      *        包括まとめ入力は対象外とする
               IF   (ACCT-HKNCOMBI NOT =   "9999" )
      *        薬評は対象外とする
               AND  (ACCT-ZAIKBN   NOT =   1      )
      *
               ADD     1                   TO  IDX1
               MOVE    ACCT-ZAINUM         TO  TBL-MAD-ZAINUM  (IDX1)
               MOVE    ACCT-SRYKA          TO  TBL-MAD-SRYKA   (IDX1)
               MOVE    ACCT-HKNCOMBI       TO  TBL-MAD-HKNCOMBI(IDX1)
               MOVE    ACCT-JIHOKBN        TO  TBL-MAD-JIHOKBN (IDX1)
               MOVE    ACCT-DAY-AREA       TO  TBL-MAD-DAY-AREA(IDX1)
      *
      *            手入力は内容をチェック
                   IF      ACCT-SRYCDTOTAL     >   1
                       PERFORM 3001-CHOZAI-CHEK-SEC
                       IF     (FLG-DEKIDA      =   1  )
                          OR  (FLG-HOKATU      =   1  )
                           MOVE    "1"         TO  TBL-MAD-SYORI (IDX1)
                       END-IF
                   END-IF
      *
               END-IF
      *
               MOVE    "tbl_sryacct_prv"   TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryacct_prv"   TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       300-CHOZAI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為内容チェック処理
      *****************************************************************
       3001-CHOZAI-CHEK-SEC            SECTION.
      *
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
           MOVE    "tbl_sryact_prv"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact_prv"    TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SRYACT-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           MOVE    ZERO                TO  FLG-DEKIDA
           MOVE    ZERO                TO  FLG-HOKATU
           PERFORM UNTIL      FLG-SRYACT   =   1
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2            >   5   )  OR
                                  (SRY-SRYCD(IDX2) =   SPACE )
      *            出来高剤
                   IF      SRY-SRYCD (IDX2)        =   "099999903"
                       MOVE    1                   TO  FLG-DEKIDA
                   END-IF
      *            包括剤
                   IF      SRY-SRYCD (IDX2)        =   "099999908"
                       MOVE    1                   TO  FLG-HOKATU
                   END-IF
               END-PERFORM
               MOVE    "tbl_sryact_prv"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SRYACT-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact_prv"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       3001-CHOZAI-CHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    調剤料再編集処理
      *****************************************************************
       400-SANTEI-SYORI-SEC            SECTION.
      *
      *    入院料包括チェック
           PERFORM 4010-HKTNYUIN-HEN-SEC
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   31
               IF     (TBL-TOU-DAY (2 IDX)     =   ZERO )  OR
                      (TBL-GAI-DAY (IDX)       >   ZERO )  OR
                      (TBL-NYU-DAY (IDX)       =   ZERO )
      *         包括でエラー
                OR    (TBL-HKTCHKFLG (IDX)     =   9    )
      *            調剤料なし
                   PERFORM 4001-TOU-DEL-SYORI-SEC
               ELSE
      *            調剤料あり
                   PERFORM 4002-TOU-ADD-SYORI-SEC
               END-IF
      *
               IF     (TBL-DOK-DAY (2 IDX)     =   ZERO )  OR
                      (TBL-GAI-DAY (IDX)       >   ZERO )  OR
                      (TBL-NYU-DAY (IDX)       =   ZERO )
      *         包括でエラー
                OR    (TBL-HKTCHKFLG (IDX)     =   9    )
      *            調剤料なし
                   PERFORM 4003-MAD-DEL-SYORI-SEC
               ELSE
      *            調剤料あり
                   PERFORM 4004-MAD-ADD-SYORI-SEC
               END-IF
           END-PERFORM
      *
      *    調剤料更新
           PERFORM 4005-CHOU-UPDATE-SEC
      *
      *    麻毒更新
           PERFORM 4006-MADO-UPDATE-SEC
           .
      *
       400-SANTEI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    調剤料なし処理
      *****************************************************************
       4001-TOU-DEL-SYORI-SEC            SECTION.
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   10  )  OR
                              (TBL-CHO-ZAINUM(IDX2)    =   ZERO )
               MOVE    ZERO           TO  TBL-CHO-DAY (IDX2 1 IDX)
               MOVE    ZERO           TO  TBL-CHO-DAY (IDX2 2 IDX)
               MOVE    ZERO           TO  TBL-CHO-DAY (IDX2 3 IDX)
               MOVE    ZERO           TO  TBL-CHO-DAY (IDX2 4 IDX)
           END-PERFORM
           .
      *
       4001-TOU-DEL-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    調剤料あり処理
      *****************************************************************
       4002-TOU-ADD-SYORI-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-ARI
           MOVE    ZERO                TO  WRK-IDX2
      *ver.4.7
           IF      TBL-TOU-DAY (2 IDX) =   2
               MOVE    3                   TO  IDX4
           ELSE
               MOVE    2                   TO  IDX4
           END-IF
      *
      *    包括分の判定
           IF      TBL-HKTCHKFLG (IDX)     =   1
               MOVE    "1"                 TO  WRK-JIHOKBN
           ELSE
               MOVE    SPACE               TO  WRK-JIHOKBN
           END-IF
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   10  )  OR
                              (TBL-CHO-ZAINUM(IDX2)    =   ZERO )
               MOVE    ZERO            TO  FLG-CHK
               PERFORM VARYING     IDX-T   FROM    1   BY  1
                       UNTIL      (IDX-T   >   10  )  OR
                              (TBL-TOU-SRYKA(2 IDX IDX-T)  =  SPACE)
      *H26.4
      *            当日入力の科・保険であること
                   IF     (TBL-CHO-SRYKA(IDX2)     =   TBL-TOU-SRYKA
                                                     (2 IDX IDX-T)) AND
                          (TBL-CHO-HKNCOMBI(IDX2)  =   TBL-TOU-HKNCOMBI
                                                     (2 IDX IDX-T))
                     AND  (TBL-CHO-JIHOKBN (IDX2)  =   WRK-JIHOKBN )
                       MOVE    1               TO  FLG-CHK
      *                当日出来高剤・包括算定の手入力がない時
                       IF     (TBL-CHO-SYORI  (IDX2)   =   "1"   )
                         AND  (TBL-CHO-DAY(IDX2 1 IDX) =   ZERO  )
                           MOVE    ZERO            TO  FLG-CHK
                       END-IF
                   END-IF
               END-PERFORM
      *H26.4
               IF      FLG-CHK         =   ZERO
                   PERFORM VARYING     IDX-T   FROM    1   BY  1
                           UNTIL      (IDX-T   >   10  )  OR
                                 (TBL-TOU-SRYKA(2 IDX IDX-T)  =  SPACE)
                             OR  (FLG-CHK      =   1   )
      *            当日出来高剤・包括算定の手入力剤有
                   IF     (TBL-CHO-SRYKA(IDX2)     =   TBL-TOU-SRYKA
                                                     (2 IDX IDX-T)) AND
                          (TBL-CHO-HKNCOMBI(IDX2)  =   TBL-TOU-HKNCOMBI
                                                     (2 IDX IDX-T))
                     AND  (TBL-CHO-JIHOKBN (IDX2)  NOT =   WRK-JIHOKBN )
                     AND  (TBL-CHO-SYORI   (IDX2)  =   "1"       )
                     AND  (TBL-CHO-DAY (IDX2 1 IDX)   >   ZERO  )
                       MOVE    1               TO  FLG-CHK
                   END-IF
                   END-PERFORM
               END-IF
      *
      *
               IF      FLG-CHK         =   1
                   MOVE    IDX2            TO  WRK-IDX2
                   IF      TBL-CHO-DAY (IDX2 1 IDX)   >   ZERO
                       IF      FLG-ARI         =   ZERO
                           MOVE    1               TO  FLG-ARI
      *ver.4.7
                           IF      TBL-CHO-DAY (IDX2 IDX4 IDX) =   ZERO
                               MOVE    ZERO            TO  FLG-ARI
                               MOVE    ZERO            TO  TBL-CHO-DAY
                                                         (IDX2 1 IDX)
                               MOVE    ZERO            TO  TBL-CHO-DAY
                                                         (IDX2 2 IDX)
                               MOVE    ZERO            TO  TBL-CHO-DAY
                                                         (IDX2 3 IDX)
                               MOVE    ZERO            TO  TBL-CHO-DAY
                                                         (IDX2 4 IDX)
                           END-IF
      *ver.4.7
                       ELSE
                           MOVE    ZERO            TO  TBL-CHO-DAY
                                                         (IDX2 1 IDX)
                           MOVE    ZERO            TO  TBL-CHO-DAY
                                                         (IDX2 2 IDX)
                           MOVE    ZERO            TO  TBL-CHO-DAY
                                                         (IDX2 3 IDX)
                           MOVE    ZERO            TO  TBL-CHO-DAY
                                                         (IDX2 4 IDX)
                       END-IF
                   END-IF
               ELSE
                   MOVE    ZERO            TO  TBL-CHO-DAY
                                                         (IDX2 1 IDX)
                   MOVE    ZERO            TO  TBL-CHO-DAY
                                                         (IDX2 2 IDX)
                   MOVE    ZERO            TO  TBL-CHO-DAY
                                                         (IDX2 3 IDX)
                   MOVE    ZERO            TO  TBL-CHO-DAY
                                                         (IDX2 4 IDX)
               END-IF
           END-PERFORM
           IF      FLG-ARI             =   1
               GO      TO      4002-TOU-ADD-SYORI-EXT
           END-IF
      *    当日調剤料指定がない時、対象の剤へ
           IF      WRK-IDX2            >   ZERO
               MOVE    1               TO  TBL-CHO-DAY (WRK-IDX2 1 IDX)
               MOVE    ZERO            TO  TBL-CHO-DAY (WRK-IDX2 2 IDX)
               MOVE    ZERO            TO  TBL-CHO-DAY (WRK-IDX2 3 IDX)
               MOVE    1               TO  TBL-CHO-DAY
                                                    (WRK-IDX2 IDX4 IDX)
               GO      TO      4002-TOU-ADD-SYORI-EXT
           END-IF
      *
      *    保険組合せの期間チェック
           MOVE    ZERO                TO  IDX-TOU
           PERFORM VARYING     IDX-T   FROM    1   BY  1
                   UNTIL      (IDX-T   >   10  )  OR
                              (TBL-TOU-SRYKA(2 IDX IDX-T)  =  SPACE)
               IF     (TBL-TOU-HKNCOMBI (2 IDX IDX-T)  NOT =   SPACE)
               AND    (TBL-TOU-HKNERR (2 IDX IDX-T)    =   SPACE  )
                   MOVE    IDX-T               TO  IDX-TOU
                   MOVE    10                  TO  IDX-T
               END-IF
           END-PERFORM
           IF      IDX-TOU             =   ZERO
               MOVE    1               TO  IDX-TOU
           END-IF
      *    当日調剤料指定がない時、作成へ
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   10  )
               MOVE    ZERO                TO  FLG-HEN
               IF      TBL-CHO-ZAINUM(IDX2)    =   ZERO
                   IF     (WRK-JIHOKBN     =   TBL-CHO-JIHOKBN  (IDX2))
                       MOVE    1               TO  FLG-HEN
                   ELSE
                       IF      TBL-CHO-SRYKA(IDX2) =   SPACE
                           MOVE    1               TO  FLG-HEN
                       END-IF
                   END-IF
               END-IF
               IF      FLG-HEN                 =   1
      *
                   MOVE    "1"                 TO  TBL-CHO-ADD (IDX2)
      *            当日入力の科・保険で作成する
                   MOVE    TBL-TOU-SRYKA   (2 IDX IDX-TOU)
                                           TO  TBL-CHO-SRYKA(IDX2)
                   MOVE    TBL-TOU-HKNCOMBI(2 IDX IDX-TOU)
                                           TO  TBL-CHO-HKNCOMBI (IDX2)
                   MOVE    WRK-JIHOKBN     TO  TBL-CHO-JIHOKBN  (IDX2)
                   MOVE    1               TO  TBL-CHO-DAY (IDX2 1 IDX)
                   MOVE    1               TO  TBL-CHO-DAY
                                                         (IDX2 IDX4 IDX)
                   MOVE    10              TO  IDX2
               END-IF
           END-PERFORM
           .
      *
       4002-TOU-ADD-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    麻毒なし処理
      *****************************************************************
       4003-MAD-DEL-SYORI-SEC            SECTION.
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   10  )  OR
                              (TBL-MAD-ZAINUM(IDX2)    =   ZERO )
               MOVE    ZERO           TO  TBL-MAD-DAY (IDX2 1 IDX)
               MOVE    ZERO           TO  TBL-MAD-DAY (IDX2 2 IDX)
               MOVE    ZERO           TO  TBL-MAD-DAY (IDX2 3 IDX)
               MOVE    ZERO           TO  TBL-MAD-DAY (IDX2 4 IDX)
           END-PERFORM
           .
       4003-MAD-DEL-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    麻毒あり処理
      *****************************************************************
       4004-MAD-ADD-SYORI-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-ARI
           MOVE    ZERO                TO  WRK-IDX2
      *    包括分の判定
           IF      TBL-HKTCHKFLG (IDX)     =   1
               MOVE    "1"                 TO  WRK-JIHOKBN
           ELSE
               MOVE    SPACE               TO  WRK-JIHOKBN
           END-IF
      *ver.4.7
           IF      TBL-DOK-DAY (2 IDX) =   2
               MOVE    3                   TO  IDX4
           ELSE
               MOVE    2                   TO  IDX4
           END-IF
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   10  )  OR
                              (TBL-MAD-ZAINUM(IDX2)    =   ZERO )
               MOVE    ZERO            TO  FLG-CHK
               PERFORM VARYING     IDX-T   FROM    1   BY  1
                       UNTIL      (IDX-T   >   10  )  OR
                              (TBL-DOK-SRYKA(2 IDX IDX-T)  =  SPACE)
      *            当日入力の科・保険であること
                   IF     (TBL-MAD-SRYKA(IDX2)     =   TBL-DOK-SRYKA
                                                     (2 IDX IDX-T)) AND
                          (TBL-MAD-HKNCOMBI(IDX2)  =   TBL-DOK-HKNCOMBI
                                                     (2 IDX IDX-T))
                     AND  (TBL-MAD-JIHOKBN (IDX2)  =   WRK-JIHOKBN )
                       MOVE    1               TO  FLG-CHK
      *H26.4
      *                当日出来高剤・包括算定の手入力がない時
                       IF     (TBL-MAD-SYORI  (IDX2)   =   "1"   )
                         AND  (TBL-MAD-DAY(IDX2 1 IDX) =   ZERO  )
                           MOVE    ZERO            TO  FLG-CHK
                       END-IF
                   END-IF
               END-PERFORM
      *H26.4
               IF      FLG-CHK         =   ZERO
                   PERFORM VARYING     IDX-T   FROM    1   BY  1
                           UNTIL      (IDX-T   >   10  )  OR
                                 (TBL-TOU-SRYKA(2 IDX IDX-T)  =  SPACE)
                             OR  (FLG-CHK      =   1   )
      *            当日出来高剤・包括算定の手入力剤有
                   IF     (TBL-MAD-SRYKA(IDX2)     =   TBL-DOK-SRYKA
                                                     (2 IDX IDX-T)) AND
                          (TBL-MAD-HKNCOMBI(IDX2)  =   TBL-DOK-HKNCOMBI
                                                     (2 IDX IDX-T))
                     AND  (TBL-MAD-JIHOKBN (IDX2)  NOT =   WRK-JIHOKBN )
                     AND  (TBL-MAD-SYORI   (IDX2)  =   "1"       )
                     AND  (TBL-MAD-DAY (IDX2 1 IDX)   >   ZERO  )
                       MOVE    1               TO  FLG-CHK
                   END-IF
      *
                   END-PERFORM
               END-IF
      *
               IF      FLG-CHK         =   1
                   MOVE    IDX2            TO  WRK-IDX2
                   IF      TBL-MAD-DAY (IDX2 1 IDX)   >   ZERO
                       IF      FLG-ARI         =   ZERO
                           MOVE    1               TO  FLG-ARI
      *ver.4.7
                           IF      TBL-MAD-DAY (IDX2 IDX4 IDX) =   ZERO
                               MOVE    ZERO            TO  FLG-ARI
                               MOVE    ZERO            TO  TBL-MAD-DAY
                                                         (IDX2 1 IDX)
                               MOVE    ZERO            TO  TBL-MAD-DAY
                                                         (IDX2 2 IDX)
                               MOVE    ZERO            TO  TBL-MAD-DAY
                                                         (IDX2 3 IDX)
                               MOVE    ZERO            TO  TBL-MAD-DAY
                                                         (IDX2 4 IDX)
                           END-IF
      *ver.4.7
                       ELSE
                           MOVE    ZERO            TO  TBL-MAD-DAY
                                                         (IDX2 1 IDX)
                           MOVE    ZERO            TO  TBL-MAD-DAY
                                                         (IDX2 2 IDX)
                           MOVE    ZERO            TO  TBL-MAD-DAY
                                                         (IDX2 3 IDX)
                           MOVE    ZERO            TO  TBL-MAD-DAY
                                                         (IDX2 4 IDX)
                       END-IF
                   END-IF
               ELSE
                   MOVE    ZERO            TO  TBL-MAD-DAY
                                                         (IDX2 1 IDX)
                   MOVE    ZERO            TO  TBL-MAD-DAY
                                                         (IDX2 2 IDX)
                   MOVE    ZERO            TO  TBL-MAD-DAY
                                                         (IDX2 3 IDX)
                   MOVE    ZERO            TO  TBL-MAD-DAY
                                                         (IDX2 4 IDX)
               END-IF
           END-PERFORM
           IF     FLG-ARI              =   1
               GO      TO      4004-MAD-ADD-SYORI-EXT
           END-IF
      *    当日調剤料指定がない時、対象の剤へ
           IF      WRK-IDX2            >   ZERO
               MOVE    1               TO  TBL-MAD-DAY (WRK-IDX2 1 IDX)
      *ver.4.7
               MOVE    ZERO            TO  TBL-MAD-DAY (WRK-IDX2 2 IDX)
               MOVE    ZERO            TO  TBL-MAD-DAY (WRK-IDX2 3 IDX)
               MOVE    1               TO  TBL-MAD-DAY
                                                     (WRK-IDX2 IDX4 IDX)
               GO      TO      4004-MAD-ADD-SYORI-EXT
           END-IF
      *
      *    保険組合せの期間チェック
           MOVE    ZERO                TO  IDX-TOU
           PERFORM VARYING     IDX-T   FROM    1   BY  1
                   UNTIL      (IDX-T   >   10  )  OR
                              (TBL-DOK-SRYKA(2 IDX IDX-T)  =  SPACE)
               IF     (TBL-DOK-HKNCOMBI (2 IDX IDX-T)  NOT =   SPACE)
               AND    (TBL-DOK-HKNERR (2 IDX IDX-T)    =   SPACE  )
                   MOVE    IDX-T               TO  IDX-TOU
                   MOVE    10                  TO  IDX-T
               END-IF
           END-PERFORM
           IF      IDX-TOU             =   ZERO
               MOVE    1               TO  IDX-TOU
           END-IF
      *    当日調剤料指定がない時、作成へ
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   10  )
               MOVE    ZERO                TO  FLG-HEN
               IF      TBL-MAD-ZAINUM(IDX2)    =   ZERO
                   IF     (WRK-JIHOKBN     =   TBL-MAD-JIHOKBN (IDX2))
                       MOVE    1               TO  FLG-HEN
                   ELSE
                       IF      TBL-MAD-SRYKA(IDX2) =   SPACE
                           MOVE    1               TO  FLG-HEN
                       END-IF
                   END-IF
               END-IF
               IF      FLG-HEN                 =   1
                   MOVE    "1"                 TO  TBL-MAD-ADD (IDX2)
      *            当日入力の科・保険で作成する
                   MOVE    TBL-DOK-SRYKA(2 IDX IDX-TOU)
                                           TO  TBL-MAD-SRYKA(IDX2)
                   MOVE    TBL-DOK-HKNCOMBI(2 IDX IDX-TOU)
                                           TO  TBL-MAD-HKNCOMBI (IDX2)
                   MOVE    WRK-JIHOKBN     TO  TBL-MAD-JIHOKBN  (IDX2)
                   MOVE    1               TO  TBL-MAD-DAY (IDX2 1 IDX)
                   MOVE    1               TO  TBL-MAD-DAY
                                                         (IDX2 IDX4 IDX)
                   MOVE    10              TO  IDX2
               END-IF
           END-PERFORM
           .
       4004-MAD-ADD-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    調剤料更新処理
      *****************************************************************
       4005-CHOU-UPDATE-SEC            SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   10  )  OR
                              (TBL-CHO-ZAINUM(IDX) =   ZERO )
                          OR  (ORCSCHO-ERRCD   NOT =   ZERO )
               INITIALIZE                      SRYACCT-REC
               MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
               MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
               MOVE    ORCSCHO-PTID        TO  ACCT-PTID
               MOVE    TBL-CHO-SRYKA (IDX) TO  ACCT-SRYKA
               MOVE    ORCSCHO-SRYYM       TO  ACCT-SRYYM
               MOVE    "24"                TO  ACCT-SRYKBN
               MOVE    TBL-CHO-ZAINUM(IDX) TO  ACCT-ZAINUM
      *
               MOVE    SRYACCT-REC         TO  MCPDATA-REC
               MOVE    "tbl_sryacct_prv"   TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_sryacct_prv"   TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 930-SRYACCT-READ-SEC
               ELSE
                   INITIALIZE                  SRYACCT-REC
                   MOVE    1               TO  FLG-SRYACCT
               END-IF
               MOVE    "tbl_sryacct_prv"   TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
               IF      FLG-SRYACCT          =   ZERO
      *            診療会計マスターを変更する
                   MOVE    TBL-CHO-DAY-AREA(IDX)   TO  ACCT-DAY-AREA
                   PERFORM 40051-DAY-UPD-SEC
      *            回数がない時、対象の科・保険で投薬のない時削除
                   MOVE    ZERO                TO  FLG-SRYACCT-DEL
                   IF      ACCT-ZAIKAISU       =   ZERO
                       PERFORM 40054-DELETE-CHK-SEC
                   END-IF
      *            退院で回数がない時、削除する
                   IF    ((ORCSCHO-KBN         =   "2"  )  AND
                          (ACCT-ZAIKAISU       =   ZERO ))  OR
                         ( FLG-SRYACCT-DEL     =   1     )
                       PERFORM 40052-SRYACCT-DEL-SEC
                   ELSE
      *                保険組合せ期間チェック
                       MOVE    ACCT-HKNCOMBI       TO  WRK-HKNCOMBI
                       MOVE    ACCT-DAY-AREA       TO  WRK-DAY-AREA
                       PERFORM 40054-HKNCOMBI-CHK-SEC
      *
                       MOVE    SMCNDATE-YMD        TO  ACCT-UPYMD
                       MOVE    SMCNDATE-HMS        TO  ACCT-UPHMS
      *
                       MOVE    SPA-OPID            TO  ACCT-OPID
      *
                       MOVE    SRYACCT-REC         TO  MCPDATA-REC
                       MOVE    "DBUPDATE"          TO  MCP-FUNC
                       MOVE    "tbl_sryacct_prv"   TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
grpsys                 CALL    "ORCDBMAIN"         USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
                       IF      MCP-RC          NOT =   ZERO
                            DISPLAY "ORCSCHOZAI SRYACCT UPD ERR:" MCP-RC
                                ",KEY:" ACCT-KEY
                            MOVE    1               TO  ORCSCHO-ERRCD
                       END-IF
      *
                   END-IF
               ELSE
                   DISPLAY "ORCSCHOZAII SRYACCT READ ERR:" ACCT-KEY
               END-IF
      *
           END-PERFORM
      *
      *    当月入院調剤料の剤がない時、作成する
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   10
                          OR  (ORCSCHO-ERRCD   NOT =   ZERO )
               IF      (TBL-CHO-ZAINUM(IDX)    =   ZERO )  AND
                       (TBL-CHO-ADD   (IDX)    =   "1"  )
                   PERFORM 40052-CHOUZAI-INS-SEC
               END-IF
           END-PERFORM
      *
           .
       4005-CHOU-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスター削除チェック処理
      *****************************************************************
       40054-DELETE-CHK-SEC           SECTION.
      *
           MOVE    SRYACCT-REC         TO  HZN-SRYACCT-REC
      *
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    ORCSCHO-PTID        TO  ACCT-PTID
           MOVE    HZN-ACCT-SRYKA      TO  ACCT-SRYKA
           MOVE    ORCSCHO-SRYYM       TO  ACCT-SRYYM
           MOVE    HZN-ACCT-HKNCOMBI   TO  ACCT-HKNCOMBI
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct_prv"   TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct_prv"   TO  MCP-TABLE
               MOVE    "key11"             TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
           MOVE    ZERO                TO  FLG-CHK
           PERFORM
               UNTIL   (FLG-SRYACCT    =   1  )   OR
                       (FLG-CHK        =   1  )
               IF      ACCT-SRYKBN         =   "21"  OR
                                               "22"  OR
                                               "23"
                   MOVE    1               TO  FLG-CHK
               END-IF
      *
               MOVE    "tbl_sryacct_prv"   TO  MCP-TABLE
               MOVE    "key11"             TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-READ-SEC
           END-PERFORM
           MOVE    "tbl_sryacct_prv"   TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-CHK             =   ZERO
               MOVE    1               TO  FLG-SRYACCT-DEL
           END-IF
      *
           MOVE    HZN-SRYACCT-REC     TO  SRYACCT-REC
      *
           .
       40054-DELETE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスター削除処理
      *****************************************************************
       40052-SRYACCT-DEL-SEC           SECTION.
      *
           INITIALIZE                  SRYACT-REC
           MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_sryact_prv"    TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "ORCSCHOZAI SRYACT DEL ERR:" MCP-RC
                       ",KEY:" SRY-KEY
               MOVE    1               TO  ORCSCHO-ERRCD
           END-IF
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_sryacct_prv"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "ORCSCHOZAI SRYACCT DEL ERR:" MCP-RC
                                ",KEY:" ACCT-KEY
               MOVE    1               TO  ORCSCHO-ERRCD
           END-IF
      *
      *    受診履歴削除
           MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    ACCT-HOSPNUM        TO  JYURRK-HOSPNUM
           MOVE    ACCT-PTID           TO  JYURRK-PTID
           MOVE    ACCT-NYUGAIKBN      TO  JYURRK-NYUGAIKBN
           MOVE    ACCT-SRYKA          TO  JYURRK-SRYKA
           MOVE    ACCT-HKNCOMBI       TO  JYURRK-HKNCOMBINUM
           PERFORM VARYING     IDX3    FROM    1  BY  1
                   UNTIL       IDX3    >   15
               MOVE    ACCT-ZAINUM         TO  JYURRK-ZAINUM (IDX3)
           END-PERFORM
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk_prv"    TO  MCP-TABLE
           MOVE    "key20"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk_prv"    TO  MCP-TABLE
               MOVE    "key20"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           PERFORM
                   UNTIL      (FLG-JYURRK      =   1 )  OR
                              (FLG-OK          =   1 )
               PERFORM VARYING     IDX3    FROM    1  BY  1
                       UNTIL       IDX3    >   15
                   IF      ACCT-ZAINUM         =   JYURRK-ZAINUM (IDX3)
                       MOVE    ZERO            TO  JYURRK-ZAINUM (IDX3)
                   END-IF
               END-PERFORM
      *
               PERFORM VARYING     IDX3    FROM    1   BY  1
                       UNTIL       IDX3    >   15
                   IF     (JYURRK-ZAINUM (IDX3)    =   ZERO)  AND
                          (IDX3                    <   15  )
                       MOVE    JYURRK-ZAINUM (IDX3 + 1)
                                               TO  JYURRK-ZAINUM (IDX3)
                       MOVE    ZERO            TO  JYURRK-ZAINUM
                                                             (IDX3 + 1)
                   END-IF
               END-PERFORM
      *
      *
               MOVE    SMCNDATE-YMD        TO  JYURRK-UPYMD
               MOVE    SMCNDATE-HMS        TO  JYURRK-UPHMS
      *
               MOVE    SPA-OPID            TO  JYURRK-OPID
      *
               MOVE    JYURRK-REC          TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_jyurrk_prv"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                    DISPLAY "ORCSCHOZAI JYURRK UPD ERR:" MCP-RC
                            ",KEY:" JYURRK-KEY
               END-IF
      *
               MOVE    "tbl_jyurrk_prv"    TO  MCP-TABLE
               MOVE    "key20"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_jyurrk_prv"    TO  MCP-TABLE
           MOVE    "key20"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       40052-SRYACCT-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスター変更編集処理
      *****************************************************************
       40051-DAY-UPD-SEC           SECTION.
      *
           MOVE    ZERO                TO  ACCT-ZAIKAISU
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
              IF      ACCT-DAY (1 IDX-D)   >   ZERO
      *            剤回数　変更
                   COMPUTE ACCT-ZAIKAISU       =   ACCT-ZAIKAISU
                                               +   ACCT-DAY
                                                       (1  IDX-D)
               END-IF
           END-PERFORM
           .
       40051-DAY-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    麻毒更新処理
      *****************************************************************
       4006-MADO-UPDATE-SEC            SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   10  )  OR
                              (TBL-MAD-ZAINUM(IDX) =   ZERO )
                          OR  (ORCSCHO-ERRCD   NOT =   ZERO )
               INITIALIZE                      SRYACCT-REC
               MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
               MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
               MOVE    ORCSCHO-PTID        TO  ACCT-PTID
               MOVE    TBL-MAD-SRYKA (IDX) TO  ACCT-SRYKA
               MOVE    ORCSCHO-SRYYM       TO  ACCT-SRYYM
               MOVE    "26"                TO  ACCT-SRYKBN
               MOVE    TBL-MAD-ZAINUM(IDX) TO  ACCT-ZAINUM
      *
               MOVE    SRYACCT-REC         TO  MCPDATA-REC
               MOVE    "tbl_sryacct_prv"   TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_sryacct_prv"   TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 930-SRYACCT-READ-SEC
               ELSE
                   INITIALIZE                  SRYACCT-REC
                   MOVE    1               TO  FLG-SRYACCT
               END-IF
               MOVE    "tbl_sryacct_prv"   TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *
               IF      FLG-SRYACCT          =   ZERO
      *            診療会計マスターを変更する
                   MOVE    TBL-MAD-DAY-AREA(IDX)   TO  ACCT-DAY-AREA
                   PERFORM 40051-DAY-UPD-SEC
      *            回数がない時、対象の科・保険で投薬のない時削除
                   MOVE    ZERO                TO  FLG-SRYACCT-DEL
                   IF      ACCT-ZAIKAISU       =   ZERO
                       PERFORM 40054-DELETE-CHK-SEC
                   END-IF
      *            退院で回数がない時、削除する
                   IF    ((ORCSCHO-KBN         =   "2"  )  AND
                          (ACCT-ZAIKAISU       =   ZERO ))  OR
                         ( FLG-SRYACCT-DEL     =   1    )
                       PERFORM 40052-SRYACCT-DEL-SEC
                   ELSE
      *
                   MOVE    SMCNDATE-YMD        TO  ACCT-UPYMD
                   MOVE    SMCNDATE-HMS        TO  ACCT-UPHMS
      *
                   MOVE    SPA-OPID            TO  ACCT-OPID
      *
                   MOVE    SRYACCT-REC         TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "tbl_sryacct_prv"   TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                        DISPLAY "ORCSCHOZAI SRYACCT UPD ERR:" MCP-RC
                                ",KEY:" ACCT-KEY
                        MOVE    1               TO  ORCSCHO-ERRCD
                   END-IF
      *
                   END-IF
               ELSE
                   DISPLAY "ORCSCHOZAI SRYACCT READ ERR:" ACCT-KEY
               END-IF
           END-PERFORM
      *
      *    当月入院麻毒の剤がない時、作成する
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   10
                          OR  (ORCSCHO-ERRCD   NOT =   ZERO )
               IF      (TBL-MAD-ZAINUM(IDX)    =   ZERO )  AND
                       (TBL-MAD-ADD   (IDX)    =   "1"  )
                   PERFORM 40062-MADOKU-INS-SEC
               END-IF
           END-PERFORM
      *
           .
       4006-MADO-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院調剤料追加処理
      *****************************************************************
       40052-CHOUZAI-INS-SEC         SECTION.
      *
      *    調剤料（入院）
           MOVE    "120001110"         TO  WRK-SRYCD
           MOVE    "24"                TO  WRK-SRYKBN
           MOVE    "240"               TO  WRK-SRYSYUKBN
           MOVE    TBL-CHO-SRYKA (IDX) TO  WRK-SRYKA
           MOVE    TBL-CHO-HKNCOMBI(IDX)
                                       TO  WRK-HKNCOMBI
           MOVE    TBL-CHO-DAY-AREA(IDX)
                                       TO  WRK-DAY-AREA
           MOVE    TBL-CHO-JIHOKBN(IDX)
                                       TO  WRK-JIHOKBN
      *    保険組合せ期間チェック
           PERFORM 40054-HKNCOMBI-CHK-SEC
      *    会計照会作成処理
           PERFORM 40052-SRYACCT-INS-SEC
           IF      ORCSCHO-ERRCD   NOT =   ZERO
               GO     TO      40052-CHOUZAI-INS-EXT
           END-IF
      *
      *    診療行為作成処理
           PERFORM 40053-SRYACT-INS-SEC
      *
           .
       40052-CHOUZAI-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ期間チェック処理
      *****************************************************************
       40054-HKNCOMBI-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-HKN-CHK
           MOVE    SPACE               TO  WRK-HKNERR
           PERFORM VARYING     IDX-H   FROM    1    BY  1
                   UNTIL      (IDX-H   >   50  )  OR
                              (TBL-HKNCOMBINUM (IDX-H)  =   SPACE )
                         OR   (FLG-HKN-CHK     =   1    )
                         OR   (WRK-HKNERR  NOT =   SPACE)
              IF      WRK-HKNCOMBI     =   TBL-HKNCOMBINUM (IDX-H)
                  MOVE    1            TO  FLG-HKN-CHK
                  IF     (TBL-TEKSTYMD (IDX-H) (1:6)
                                           =   ORCSCHO-SRYYM)
                     OR  (TBL-TEKEDYMD (IDX-H) (1:6)
                                           =   ORCSCHO-SRYYM)
                       PERFORM 400541-HKNCOMBI-KIKAN-SEC
                  END-IF
              END-IF
           END-PERFORM
           IF      FLG-HKN-CHK         =   ZERO
               MOVE    1                   TO  ORCSCHO-HKNERR
           END-IF
           IF      WRK-HKNERR          =   "1"
               MOVE    1                   TO  ORCSCHO-HKNERR
           END-IF
           .
       40054-HKNCOMBI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険期間チェック処理
      *****************************************************************
       400541-HKNCOMBI-KIKAN-SEC         SECTION.
      *
           MOVE    ORCSCHO-SRYYM       TO  WRK-STYMD
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               IF      WRK-DAY (1 IDX-D)   NOT =   ZERO
                   MOVE    IDX-D           TO  WRK-STDD
                   IF     (WRK-STYMD       <   TBL-TEKSTYMD (IDX-H))
                      OR  (WRK-STYMD       >   TBL-TEKEDYMD (IDX-H))
                      MOVE    "1"              TO  WRK-HKNERR
                      MOVE    31               TO  IDX2
                   END-IF
               END-IF
           END-PERFORM
           .
       400541-HKNCOMBI-KIKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    麻毒追加処理
      *****************************************************************
       40062-MADOKU-INS-SEC         SECTION.
      *
      *    麻毒料（入院）
           MOVE    "120000410"         TO  WRK-SRYCD
           MOVE    "26"                TO  WRK-SRYKBN
           MOVE    "260"               TO  WRK-SRYSYUKBN
           MOVE    TBL-MAD-SRYKA (IDX) TO  WRK-SRYKA
           MOVE    TBL-MAD-HKNCOMBI(IDX)
                                       TO  WRK-HKNCOMBI
           MOVE    TBL-MAD-DAY-AREA(IDX)
                                       TO  WRK-DAY-AREA
           MOVE    TBL-MAD-JIHOKBN(IDX)
                                       TO  WRK-JIHOKBN
      *    会計照会作成処理
           PERFORM 40052-SRYACCT-INS-SEC
           IF      ORCSCHO-ERRCD   NOT =   ZERO
               GO     TO      40062-MADOKU-INS-EXT
           END-IF
      *
      *    診療行為作成処理
           PERFORM 40053-SRYACT-INS-SEC
      *
           .
       40062-MADOKU-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *   会計照会作成処理
      *****************************************************************
       40052-SRYACCT-INS-SEC          SECTION.
      *
           MOVE    01                  TO  WRK-DD
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU       NOT =   ZERO
               DISPLAY "ORCSCHOZAI TENSU INV:" MCP-RC
                       ",KEY:" WRK-SRYCD
               GO      TO      40052-SRYACCT-INS-EXT
           END-IF
      *    剤番号取得処理
           PERFORM 400521-ZAINUM-SET-SEC
      *
           MOVE    SPACE               TO  SRYACCT-REC
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    ORCSCHO-PTID        TO  ACCT-PTID
           MOVE    ORCSCHO-SRYYM       TO  ACCT-SRYYM
           MOVE    WRK-SRYKBN          TO  ACCT-SRYKBN
           MOVE    WRK-SRYKA           TO  ACCT-SRYKA
           MOVE    WRK-ZAINUM          TO  ACCT-ZAINUM
           MOVE    WRK-HKNCOMBI        TO  ACCT-HKNCOMBI
      *    剤点数
           MOVE    TNS-TEN             TO  ACCT-ZAITEN
      *    診療コード計
           MOVE    WRK-SRYCD           TO  WRK-SRYCD-9
           MOVE    WRK-SRYCD-9         TO  ACCT-SRYCDTOTAL
      *    数量計
           MOVE    1                   TO  ACCT-SURYOUTOTAL
      *    明細数
           MOVE    1                   TO  ACCT-MEISAISU
      *    手技点数１
           MOVE    TNS-TEN             TO  ACCT-SYUTEN1
      *    手技点数２
           MOVE    ZERO                TO  ACCT-SYUTEN2
      *    薬剤点数
           MOVE    ZERO                TO  ACCT-YKZTEN
      *    器材点数
           MOVE    ZERO                TO  ACCT-KIZAITEN
      *    回数
           MOVE    WRK-DAY-AREA        TO  ACCT-DAY-AREA
      *    剤回数
           MOVE    ZERO                TO  ACCT-ZAIKAISU
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL       IDX2    >   31
               ADD     ACCT-DAY (1 IDX2)   TO  ACCT-ZAIKAISU
           END-PERFORM
      *    包括剤の設定
           MOVE    WRK-JIHOKBN         TO  ACCT-JIHOKBN
      *
           MOVE    SMCNDATE-YMD        TO  ACCT-CREYMD
           MOVE    SMCNDATE-YMD        TO  ACCT-UPYMD
           MOVE    SMCNDATE-HMS        TO  ACCT-UPHMS
           MOVE    SPA-OPID            TO  ACCT-OPID
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_sryacct_prv"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "SYRACCT INS ERR:" MCP-RC
                       ",KEY:" ACCT-KEY
               MOVE    5                   TO  ORCSCHO-ERRCD 
           END-IF
      *
           .
       40052-SRYACCT-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *   剤番号取得処理
      *****************************************************************
       400521-ZAINUM-SET-SEC          SECTION.
      *
      *    患者マスターより、最終剤番号を再番し、患者マスターを更新する
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    ORCSCHO-PTID        TO  PTINF-PTID
           PERFORM 940-PTINF-READ-SEC
           IF      FLG-PTINF           =   1
               MOVE    2               TO  ORCSCHO-ERRCD 
               DISPLAY "CHOZAI ZAINUM-SET ERR:" ORCSCHO-PTID
               GO      TO      400521-ZAINUM-SET-EXT
           END-IF
      *
      *    最大剤番号
           IF      PTINF-MAXZAINUM     =   ALL "9"
               MOVE    ZERO                TO  PTINF-MAXZAINUM
           END-IF
           ADD     1                   TO  PTINF-MAXZAINUM
           MOVE    PTINF-MAXZAINUM     TO  WRK-ZAINUM
      *
           MOVE    SMCNDATE-YMD        TO  PTINF-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTINF-UPHMS
           MOVE    SPA-OPID            TO  PTINF-OPID
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_ptinf_prv"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "PTINF UPDTE ERR:"   MCP-RC ",KEY:" PTINF-KEY
               MOVE    3                   TO  ORCSCHO-ERRCD 
           END-IF 
      *
           .
      *
       400521-ZAINUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為ＤＢ新規作成処理
      *****************************************************************
       40053-SRYACT-INS-SEC         SECTION.
      *
           INITIALIZE                          SRYACT-REC
           MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
      *    剤番号
           MOVE    WRK-ZAINUM          TO  SRY-ZAINUM
      *    連番号
           MOVE    1                   TO  SRY-RENNUM
           MOVE    WRK-SRYSYUKBN       TO  SRY-SRYSYUKBN
           MOVE    WRK-SRYKBN          TO  SRY-SRYKBN
           MOVE    ZERO                TO  SRY-JIHIMONEYTOTAL
      *    １明細とする
           MOVE    WRK-SRYCD           TO  SRY-SRYCD    (1)
           MOVE    1                   TO  SRY-SRYSURYO (1)
           MOVE    ZERO                TO  SRY-SRYKAISU (1)
           MOVE    "3"                 TO  SRY-AUTOKBN  (1)
           MOVE    ZERO                TO  SRY-JIHIMONEY(1)
      *
      *
           MOVE    SMCNDATE-YMD        TO  SRY-CREYMD
           MOVE    SMCNDATE-YMD        TO  SRY-UPYMD
           MOVE    SMCNDATE-HMS        TO  SRY-UPHMS
      *
           MOVE    SPA-OPID            TO  SRY-OPID
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_sryact_prv"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "CHOZAI SRYACT INS ERR:" SRY-KEY  "."
               DISPLAY "MCP-RC:" MCP-RC ":" 
               MOVE    4               TO  ORCSCHO-ERRCD
           END-IF
      *
           .
      *
       40053-SRYACT-INS-EXT.
           EXIT.
      *****************************************************************
      *    入院料包括チェック処理
      *****************************************************************
       4010-HKTNYUIN-HEN-SEC             SECTION.
      *
      *    入院料包括診療チェック
           INITIALIZE                  ORCSCHKTALLAREA
           MOVE    ORCSCHO-SRYYM       TO  ORCSCHKTALL-SRYYM
           MOVE    "01"                TO  ORCSCHKTALL-SRYDD
           MOVE    ORCSCHO-PTID        TO  ORCSCHKTALL-PTID
      *    入院調剤料（診療区分のみ判定）
           MOVE    "24"                TO  ORCSCHKTALL-HKTSRYKBN
           MOVE    "240"               TO  ORCSCHKTALL-HKTSRYSYUKBN
      *H26.4   入院調剤料のみ包括の場合の対応
           MOVE    "120001110"         TO  ORCSCHKTALL-HKTSRYCD (1)
           CALL    "ORCSCHKTALL"       USING
                                       ORCSCHKTALLAREA
                                       SPA-AREA
      *    日毎の包括設定
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   31
               MOVE    ORCSCHKTALL-HKTCHKFLG (IDX)
                                           TO  TBL-HKTCHKFLG (IDX)
           END-PERFORM
      *
           .
       4010-HKTNYUIN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    退院処理
      *****************************************************************
       500-TAIIN-SYORI-SEC         SECTION.
      *
      *    調剤料（入院）
           MOVE    "24"                TO  WRK-SRYKBN
           PERFORM 5001-TAIIN-DEL-SEC
      *    麻毒料（入院）
           MOVE    "26"                TO  WRK-SRYKBN
           PERFORM 5001-TAIIN-DEL-SEC
           .
      *
       500-TAIIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    退院削除処理
      *****************************************************************
       5001-TAIIN-DEL-SEC         SECTION.
      *
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    ORCSCHO-PTID        TO  ACCT-PTID
           MOVE    ORCSCHO-SRYYM       TO  ACCT-SRYYM
           MOVE    WRK-SRYKBN          TO  ACCT-SRYKBN
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct_prv"   TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct_prv"   TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
           PERFORM UNTIL   FLG-SRYACCT     NOT =   ZERO
      *
      *        診療会計マスターを変更する
               PERFORM 50011-DAY-DEL-SEC
      *        回数がない時削除
               MOVE    ZERO                TO  FLG-SRYACCT-DEL
               IF      ACCT-ZAIKAISU       =   ZERO
                   PERFORM 40052-SRYACCT-DEL-SEC
               ELSE
      *
                   MOVE    SMCNDATE-YMD        TO  ACCT-UPYMD
                   MOVE    SMCNDATE-HMS        TO  ACCT-UPHMS
      *
                   MOVE    SPA-OPID            TO  ACCT-OPID
      *
                   MOVE    SRYACCT-REC         TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "tbl_sryacct_prv"   TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                        DISPLAY "ORCSCHOZAI SRYACCT UPD ERR:" MCP-RC
                                ",KEY:" ACCT-KEY
                        MOVE    1               TO  ORCSCHO-ERRCD
                   END-IF
               END-IF
      *
               MOVE    "tbl_sryacct_prv"   TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryacct_prv"   TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
      *
       5001-TAIIN-DEL-EXT.
           EXIT.
      *****************************************************************
      *    退院日以降削除処理
      *****************************************************************
       50011-DAY-DEL-SEC         SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   31
               IF      TBL-NYU-DAY (IDX)       =   ZERO
                   MOVE    ZERO                TO  ACCT-DAY (1 IDX)
                   MOVE    ZERO                TO  ACCT-DAY (2 IDX)
                   MOVE    ZERO                TO  ACCT-DAY (3 IDX)
                   MOVE    ZERO                TO  ACCT-DAY (4 IDX)
               END-IF
           END-PERFORM
      *    合計回数設定
           PERFORM 40051-DAY-UPD-SEC
           .
       50011-DAY-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    ORCSCHO-SRYYM       TO  TNS-YUKOSTYMD(1:6)
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
      *
           .
       910-TENSU-READ-EXT.
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
      *    入院診療会計マスター読込
      *****************************************************************
       930-NYUINACCT-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  NYUINACCT-REC
               MOVE    ZERO                TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                      NYUINACCT-REC
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF
      *
           .
       930-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       940-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf_prv"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf_prv"     TO  MCP-TABLE
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
           MOVE    "tbl_ptinf_prv"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       940-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴マスター読込
      *****************************************************************
       910-PTNYUINRRK-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
               MOVE    ZERO            TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴マスター読込
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴順読み
      *****************************************************************
       970-JYURRK-NEXT-SEC           SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  JYURRK-REC 
               MOVE    ZERO                TO  FLG-JYURRK
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
      *
           .
       970-JYURRK-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    システム管理読み込み
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    ZERO            TO  MCP-RC
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
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC               SECTION.
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
