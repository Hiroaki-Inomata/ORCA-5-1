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
       PROGRAM-ID.                 ORCSC80200804.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 診療行為チェック、算定（その他）
      *                      Ｈ２０年４月改正対応
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     MCC-多々納   02/04/16  労災用追加
      * 01.00.02     NACL-多々納  02/10/03  労災加算なしの追加
      * 01.00.03     NACL-多々納  02/11/20  自賠責器材追加
      * 01.00.04     NACL-多々納  03/03/19  老人の労災加算の修正
      * 01.00.05     NACL-多々納  03/09/22  家族通院精神療法算定追加
      * 01.00.06     NACL-多々納  03/10/10  リハビリチェック変更
      * 01.00.07     NACL-多々納  04/03/17  Ｈ１６年４月改正追加
      * 01.00.08     NACL-多々納  05/01/27  酸素追加
      * 01.00.09     NACL-多々納  05/11/22  MONFUNC 対応 他
      * 01.00.11     NACL-多々納  06/06/12  診療科毎の同日初診判定
      * 03.04.00     NACL-多々納  07/01/11  リハビリ終了日追加
      * 03.04.01     NACL-多々納  07/03/XX  リハビリ逓減追加(H19.4改正）
      * 03.05.00     NACL-多々納  07/04/XX  グループ診療対応
      * 04.02.00     NACL-多々納  08/03/XX  平成２０年４月改正対応
      * 04.05.00     NACL-多々納  09/07/XX  早期リハビリテーション加算チェック
      * 04.05.00     NACL-多々納  09/11/XX  入院精神療法６か月対応
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
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-CHKMST          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-PTNYUINRRK      PIC 9(01).
      *
           03  FLG-ERR             PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-ARIOK           PIC 9(01).
           03  FLG-NAIYAK          PIC 9(01).
           03  FLG-TONPUK          PIC 9(01).
           03  FLG-GAIYAK          PIC 9(01).
           03  FLG-MADOKU          PIC 9(01).
           03  FLG-ZAI             PIC 9(01).
           03  FLG-NAIYAK2         PIC 9(01).
           03  FLG-TONPUK2         PIC 9(01).
           03  FLG-GAIYAK2         PIC 9(01).
      *
           03  FLG-YAKJYO          PIC 9(01).
           03  FLG-ADD             PIC 9(01).
      *    施設基準不適合
           03  FLG-KIJUNN          PIC 9(01).
      *    早期リハビリあり
      *****03  FLG-SOUKI           PIC 9(01).
           03  FLG-SOUKI-ALL       PIC 9(01).
           03  FLG-SOUKI-END       PIC 9(01).
           03  FLG-SOUKI-ARI       PIC 9(01).
           03  FLG-SOUKI-ARI1      PIC 9(01).
           03  FLG-SOUKI-ARI2      PIC 9(01).
           03  FLG-SANTEI-ARI      PIC 9(01).
      *    手技料計算フラグ
           03  FLG-SYUGI           PIC 9(01).
      *    労災用
           03  FLG-RIHA            PIC 9(01).
      *    労災加算あり
           03  FLG-ROU-ARI         PIC 9(01).
           03  FLG-ROUSAI          PIC 9(01).
      *    リハビリあり
           03  FLG-RIGAKU          PIC 9(01).
      *
           03  FLG-SOUKIKSN            PIC 9(01).
           03  FLG-KYUSEI-CHK          PIC 9(01).
      *
           03  FLG-NENREI              PIC 9(01).
           03  FLG-TOUJITU             PIC 9(01).
           03  FLG-SEISIN              PIC 9(01).
           03  FLG-SHOKAIKBN           PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX1                PIC 9(04).
      *
           03  IDX2                PIC 9(02).
           03  IDY2                PIC 9(02).
      *
           03  IDD                 PIC 9(02).
           03  IDD2                PIC 9(02).
      *
           03  IDXS                PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-Z1              PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-C               PIC 9(04).
           03  IDX-K               PIC 9(04).
           03  IDX-SA              PIC 9(04).
           03  IDX-MAX             PIC 9(04).
      *
           03  IDX-SYU             PIC 9(04).
           03  HZN-IDX             PIC 9(04).
      *
           03  IDX-Y               PIC 9(04).
           03  IDX-Y2              PIC 9(02).
           03  IDX-T               PIC 9(04).
           03  IDX-R               PIC 9(04).
           03  IDX-R1              PIC 9(04).
      *
           03  IDX-RS              PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
           03  IDX-Z2              PIC 9(04).
           03  IDX-RSI             PIC 9(04).
      *
           03  IDX-HZN             PIC 9(04).
      *
           03  IDX-STR             PIC 9(04).
      *
           03  IDX-DAY             PIC 9(02).
           03  IDX-KSN             PIC 9(04).
           03  IDE                 PIC 9(02).
           03  IDE2                PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SRYCD           PIC X(09).
           03  WRK-SRYCD2          PIC X(09).
      *    減額診療コード（労災）
           03  WRK-GEN-SRYCD       PIC X(09).
      *
           03  WRK-MOJI            PIC X(01). 
      *
           03  WRK-ERRCD           PIC X(04). 
      *
           03  WRK-SYOKEIS         PIC 9(08)V99.
           03  WRK-SYOKEIS2        PIC 9(08)V99.
           03  WRK-SYOKEI          PIC 9(08).
      *
           03  WRK-ZAIKEI          PIC 9(08)V99.
           03  WRK-GAIKEI          PIC 9(08)V99.
           03  WRK-GAITEN          PIC 9(08).
           03  WRK-KIZKEI          PIC 9(08).
           03  WRK-KIZTEN          PIC 9(08).
           03  WRK-FIRTEN          PIC 9(08).
           03  WRK-MAISU           PIC 9(08).
           03  WRK-KEIMAI          PIC 9(08).
           03  WRK-KAISU           PIC 9(04).
           03  WRK-DAYCNT          PIC 9(03).
           03  WRK-TUKCNT          PIC 9(03).
      *
           03  WRK-TENSUKEI        PIC 9(08)V99.
           03  WRK-TENSUKEI-9      PIC S9(08).
           03  WRK-TENSUKEI-G      PIC S9(08).
           03  WRK-TENSUKEI-K      PIC S9(08).
           03  WRK-TENSUKEI-RSI    PIC S9(08).
           03  WRK-TENGAKEI-G      PIC S9(10).
           03  WRK-TENGAKEI-K      PIC S9(10).
      *
           03  WRK-TENSU-KEISAN.
               05  WRK-SUM-TENSUKEI    PIC 9(08)V99.
               05  WRK-SUM-TENGAKEI    PIC 9(08)V99.
      *
           03  WRK-GOK-TENSUKEI        PIC S9(10).
           03  WRK-GOK-TENGAKEI        PIC S9(10).
      *
           03  WRK-IDX             PIC 9(04).
      *
           03  WRK-RIGAKU-KBN      PIC X(01).
           03  WRK-RIGAKU-RYO      PIC X(01).
           03  WRK-RIGAK-CNTALL    PIC 9(03).
           03  WRK-TBL-RSI-KBN     PIC X(01).
      *!
      *    酸素計算用
           03  WRK-SANSO               PIC 9(08).
           03  WRK-SANSOKIN            PIC 9(08)V99.
           03  WRK-SANKEI              PIC 9(08).
           03  WRK-SANTEN              PIC 9(08).
           03  WRK-SANSOKIN-SUM        PIC 9(08)V99.
           03  IDX-SANSO               PIC 9(04).
      *
      *    理学最大単位
           03  WRK-RIGAK-MAX       PIC 9(03).
      *
           03   WRK-SYOYMD.
                05   WRK-SYO-YY    PIC 9(04).
                05   WRK-SYO-MM    PIC 9(02).
                05   WRK-SYO-DD    PIC 9(02).
           03   WRK-SYOYMD2.
                05   WRK-SYO2-YY    PIC 9(04).
                05   WRK-SYO2-MM    PIC 9(02).
                05   WRK-SYO2-DD    PIC 9(02).
      *    診療日
           03   WRK-SRYYMD         PIC X(08).
           03   WRK-EDSRYYMD       PIC X(08).
      **** 急性発生日＋15
      *    03   WRK-KYUSEI-15      PIC X(08).
      *    急性発生日＋30
      *    03   WRK-KYUSEI-30      PIC X(08).
      *    急性発生日＋14
      *    03   WRK-KYUSEI-14      PIC X(08).
      *    急性発生日＋31
      *    03   WRK-KYUSEI-31      PIC X(08).
      *****03   WRK-KYUSEI-89      PIC X(08).
           03   WRK-KYUSEI-YMD     PIC X(08).
           03   WRK-KYUSEI         PIC X(08).
           03   WRK-YMD.
                05   WRK-YY             PIC 9(04).
                05   WRK-MM             PIC 9(02).
                05   WRK-DD             PIC 9(02).
           03   WRK-EDMM           PIC X(02).
           03   WRK-EDDD           PIC X(02).
           03   WRK-KYUSEI-MSG     PIC X(80).
      *
           03   WRK-RIHA-KBN       PIC X(01).
           03   WRK-RIHA-KBN2      PIC X(01).
           03   WRK-RIHA-GEN       PIC X(01).
      *@    逓減開始日
           03   WRK-TEIGEN-YMD     PIC X(08).
           03   WRK-TEIGEN         PIC X(08).
           03   WRK-TEIGEN-MSG     PIC X(80).
      *
      *    自賠責金額用
           03  WRK-KINGAK          PIC 9(08).
           03  FLG-JIHI            PIC 9(01).
      *
           03  WRK-NYU-KAI         PIC 9(03).
      *    集団の月上限
           03  WRK-MAX-CNT         PIC 9(04).
      *    訂正
           03  WRK-TEI-CNT         PIC 9(04).
           03  WRK-ZOGEN           PIC S9(03).
      *    診療年月日−６ヶ月
           03  WRK-SRYYMD-1.
               05  WRK-SRYYM-1.
                   07  WRK-SRYYY       PIC 9(04).
                   07  WRK-SRYMM       PIC 9(02).
               05  WRK-SRYDD           PIC 9(02).
      *
           03  WRK-SRYKA           PIC  X(02).
      *
      *    発症日＋３０日
           03  WRK-KYUSEI-30               PIC X(08).
           03  WRK-KYUSEI-30-MSG           PIC X(80).
      *    入院精神療法期間チェック
           03  WRK-TUKI-9                  PIC 9(03).
           03  WRK-NYUINYMD                PIC X(08).
           03  WRK-NYUIN-YMD               PIC X(08).
           03  WRK-SHONUM                  PIC 9(03).
      *
      *    リハビリ回数集計
       01  WRK-RIGAKU-NCNT-AREA.
           03  WRK-RIGAKU-INT2             PIC X(01).
      *    今回リハビリ発生日
           03  WRK-KYUSEI-INP-G.
               05  WRK-KYUSEI-IN-G         OCCURS   7.
                   07  WRK-KYUSEI-ST       PIC X(01).
                   07  WRK-KYUSEI-ED       PIC X(01).
               05  WRK-TEIGEN-IN           PIC X(01).
      *    当月算定回数
           03  WRK-RIHA-DAY-G.
               05  WRK-RIHA-DAY            PIC 9(03)
                                           OCCURS   31.
      *    今回算定回数
           03  WRK-RIHA-CNT-G.
               05  WRK-RIHA-CNT            PIC 9(03)
                                           OCCURS   31.
      *    月回数
           03  WRK-TUKI-CNT              PIC 9(03).
      *    リハビリ発生日
           03  WRK-KYUSEI1-YMD             PIC X(08).
           03  WRK-KYUSEI2-YMD             PIC X(08).
           03  WRK-KYUSEI3-YMD             PIC X(08).
           03  WRK-KYUSEI4-YMD             PIC X(08).
      *    リハビリ終了日チェック用
           03  WRK-KYUSEI5-YMD             PIC X(08).
           03  WRK-KYUSEI5-CHK             PIC X(01).
           03  WRK-KYUSEI6-YMD             PIC X(08).
           03  WRK-KYUSEI6-CHK             PIC X(01).
           03  WRK-KYUSEI7-YMD             PIC X(08).
           03  WRK-KYUSEI7-CHK             PIC X(01).
      *    逓減対象外
           03  WRK-TEIGEN-GAI              PIC X(08).
      *
      *    通院精神用同日初診
           03  WRK-DOUSYOSIN-AREA.
               05  WRK-DOUSYOSIN-OCC       OCCURS   10.
                   07  WRK-DOU-SRYKA       PIC X(02).
                   07  WRK-DOU-SRYYMD      PIC X(08).
      *    初期区分
           03  WRK-RIGAKU-INT              PIC X(01).
      *
       01  WRK-ERRCD-AREA.
           03  WRK-ERRCD-H                 PIC X(04). 
      *    保存用
       01  HZN-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //HZN-//.
      *    リハビリ同一算定コードテーブル
           COPY   "CMRIHACDTBL.INC".
      *    保険別算定履歴テーブル
           COPY   "CMSANTEITBL.INC".
      *
      *    リハビリの早期リハ加算テーブル
      **** COPY   "CMRIHAKSNTBL.INC".
       01  CON-RIHAKSN                 PIC X(09)   VALUE    "180030770".
      *
      *    酸素コードテーブル
           COPY     "CMSANSOTBL.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ（施設基準）
           COPY    "CPSK1006.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
      *    COPY    "CPTENSU.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    チェックマスタ−
      *01  CHK-REC.
      *    COPY    "CPCHK.INC".
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *
      *    診療行為算定編集パラメタ
           COPY    "CPORCSC90TEN.INC".
      *
      *    診療行為算定歴チェックパラメタ
           COPY    "CPORCSC91.INC".
      *
      *    診療行為点数編集・基本チェック
           COPY    "CPORCSC92.INC".
      *
      *    リハビリ当月算定カウントパラメタ
           COPY    "CPORCSC80CNT.INC".
      *
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    画面表示編集パラメタ
           COPY    "CPORCSC94.INC".
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
           MOVE    LNK-RIGAKU-NCNT-AREA    TO  WRK-RIGAKU-NCNT-AREA
      *
           IF      LNK-ORCSC-PG        =   "K02"
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           END-IF
      *    初回に当月分編集
           IF      LNK-ORCSC-PG        =   "K02"
               IF      WRK-RIGAKU-INT      =   SPACE
                   PERFORM 1000-RIHBIRI-INIT-SEC
               END-IF
               IF      WRK-RIGAKU-INT2     =   SPACE
      *            今回算定回数クリア
                   INITIALIZE  WRK-RIHA-CNT-G
      ************ INITIALIZE  WRK-KYUSEI-INP-G
                   MOVE    "1"             TO  WRK-RIGAKU-INT2
               END-IF
           ELSE
               INITIALIZE              WRK-RIGAKU-NCNT-AREA
           END-IF
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
           MOVE    WRK-RIGAKU-NCNT-AREA    TO  LNK-RIGAKU-NCNT-AREA
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    リハビリの回数編集処理
      *****************************************************************
       1000-RIHBIRI-INIT-SEC                SECTION.
      *
           INITIALIZE                  WRK-RIGAKU-NCNT-AREA
           MOVE    "099800201"         TO  WRK-SRYCD
      *    訂正の時、当日算定有無の判定
           MOVE    ZERO                TO  WRK-TEI-CNT
           IF      LNK-ORCSC-SYORIFLG      =   1
               PERFORM VARYING     IDZ     FROM    1   BY  1
                       UNTIL      (IDZ     >   SPA-MAX-LINE )  OR
                                  (LNK-ORCSC-TEI-SRYCD(IDZ)
                                           =   SPACE )
                   IF      LNK-ORCSC-TEI-SRYCD(IDZ)
                                           =   WRK-SRYCD
                       ADD     LNK-ORCSC-TEI-KAISU(IDZ)
                                               TO  WRK-TEI-CNT
                   END-IF
               END-PERFORM
           END-IF
      *    リハビリ算定回数
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    SPA-SRYYMD (1:6)    TO  SANTEI-SRYYM
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
      *    労災毎とする
           IF      SPA-HKNKBN          =   1
                                       OR  2
               MOVE    SPA-HKNCOMBINUM     TO  SANTEI-HKNCOMBINUM
           ELSE
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           END-IF
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
           MOVE    SPA-SRYYMD(7:2)     TO  IDD2
      *    月上限
           MOVE    ZERO                TO  WRK-TUKI-CNT
           IF      FLG-SANTEI          =   ZERO
      *        当日の算定回数
               PERFORM VARYING     IDD     FROM    1   BY  1
                       UNTIL       IDD     >   31
      *            訂正分
                   IF      IDD             =   IDD2
                       IF      WRK-TEI-CNT     >   SANTEI-DAY (IDD)
                           MOVE    ZERO        TO  SANTEI-DAY (IDD)
                       ELSE
                           COMPUTE SANTEI-DAY (IDD)
                                               =   SANTEI-DAY (IDD)
                                               -   WRK-TEI-CNT
                       END-IF
                   END-IF
                   ADD     SANTEI-DAY (IDD)    TO  WRK-RIHA-DAY(IDD)
                   ADD     SANTEI-DAY (IDD)    TO  WRK-TUKI-CNT
               END-PERFORM
           END-IF
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    各急性発生日
           MOVE    "099800111"         TO  WRK-SRYCD
           PERFORM 10001-KYUSEI-YMD-SEC
           MOVE    WRK-YMD             TO  WRK-KYUSEI1-YMD
      *    終了日判定
           IF      WRK-KYUSEI1-YMD NOT =   SPACE
               MOVE    "099800112"         TO  WRK-SRYCD
               PERFORM 10001-KYUSEI-YMD-SEC
               IF      WRK-YMD         NOT =   SPACE
                   IF      WRK-KYUSEI1-YMD     <   WRK-YMD
                       MOVE    SPACE           TO  WRK-KYUSEI1-YMD
                   END-IF
               END-IF
           END-IF
      *
           MOVE    "099800121"         TO  WRK-SRYCD
           PERFORM 10001-KYUSEI-YMD-SEC
           MOVE    WRK-YMD             TO  WRK-KYUSEI2-YMD
           IF      WRK-KYUSEI2-YMD NOT =   SPACE
               MOVE    "099800122"         TO  WRK-SRYCD
               PERFORM 10001-KYUSEI-YMD-SEC
               IF      WRK-YMD         NOT =   SPACE
                   IF      WRK-KYUSEI2-YMD     <   WRK-YMD
                       MOVE    SPACE           TO  WRK-KYUSEI2-YMD
                   END-IF
               END-IF
           END-IF
      *
           MOVE    "099800131"         TO  WRK-SRYCD
           PERFORM 10001-KYUSEI-YMD-SEC
           MOVE    WRK-YMD             TO  WRK-KYUSEI3-YMD
           IF      WRK-KYUSEI3-YMD NOT =   SPACE
               MOVE    "099800132"         TO  WRK-SRYCD
               PERFORM 10001-KYUSEI-YMD-SEC
               IF      WRK-YMD         NOT =   SPACE
                   IF      WRK-KYUSEI3-YMD     <   WRK-YMD
                       MOVE    SPACE           TO  WRK-KYUSEI3-YMD
                   END-IF
               END-IF
           END-IF
      *
           MOVE    "099800141"         TO  WRK-SRYCD
           PERFORM 10001-KYUSEI-YMD-SEC
           MOVE    WRK-YMD             TO  WRK-KYUSEI4-YMD
           IF      WRK-KYUSEI4-YMD NOT =   SPACE
               MOVE    "099800142"         TO  WRK-SRYCD
               PERFORM 10001-KYUSEI-YMD-SEC
               IF      WRK-YMD         NOT =   SPACE
                   IF      WRK-KYUSEI4-YMD     <   WRK-YMD
                       MOVE    SPACE           TO  WRK-KYUSEI4-YMD
                   END-IF
               END-IF
           END-IF
      *    逓減対象外
           MOVE    "099800401"         TO  WRK-SRYCD
           PERFORM 10001-KYUSEI-YMD-SEC
           MOVE    WRK-YMD             TO  WRK-TEIGEN-GAI
      *
      *    外来の時、精神通院
           IF      SPA-NYUGAIKBN       =   "2"
               PERFORM 10002-DOUJIT-YMD-SEC
           END-IF
      *
           MOVE    "1"                 TO  WRK-RIGAKU-INT
           .
       1000-RIHBIRI-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    急性発生日設定処理
      *****************************************************************
       10001-KYUSEI-YMD-SEC                SECTION.
      *
      *    MOVE    WRK-SRYCD           TO  TNS-SRYCD
      *    訂正時
           MOVE    ZERO                TO  FLG-TOUJITU
           IF      LNK-ORCSC-SYORIFLG      =   1
               PERFORM VARYING     IDZ     FROM    1   BY  1
                       UNTIL      (IDZ     >   100 )  OR
                                  (LNK-ORCSC-TEI-SRYCD(IDZ)
                                           =   SPACE )
                   IF      LNK-ORCSC-TEI-SRYCD(IDZ)
                                           =   WRK-SRYCD
                       MOVE    1           TO  FLG-TOUJITU
                   END-IF
               END-PERFORM
           END-IF
      *    算定履歴検索
           MOVE    SPACE               TO  SANTEI-REC
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
      *
      *    労災毎とする
           IF     (SPA-HKNKBN          =   1
                                       OR  2   )
      *        逓減対象外は１件
              AND (WRK-SRYCD       NOT =   "099800401" )
               MOVE    SPA-HKNCOMBINUM     TO  SANTEI-HKNCOMBINUM
           ELSE
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           END-IF
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           MOVE    SPACE               TO  WRK-YMD
           PERFORM
               UNTIL       FLG-SANTEI      =   1
      *
               IF     (SANTEI-MSANTEICNT   >   ZERO )  AND
                      (SANTEI-SRYYM        <=  SPA-SRYYMD(1:6))
      *            最後の発生日、当日以降は対象外
                   IF      SANTEI-SRYYM        =   SPA-SRYYMD(1:6)
                       MOVE    SPA-SRYYMD(7:2)     TO  IDD2
      *                訂正分は対象外
                       IF      FLG-TOUJITU         =   1
                           COMPUTE IDD2            =   IDD2   -  1
                       END-IF
                   ELSE
                       MOVE    31                  TO  IDD2
                   END-IF
                   MOVE    ZERO                TO  WRK-DD
                   PERFORM VARYING     IDD     FROM    IDD2  BY  -1
                           UNTIL       IDD     <   1
                       IF      SANTEI-DAY (IDD)    >   ZERO
                           MOVE    IDD             TO  WRK-DD
                           MOVE    1               TO  IDD
                       END-IF
                   END-PERFORM
                   IF      WRK-DD              >   ZERO
                       MOVE    SANTEI-SRYYM    TO  WRK-YMD(1:6)
                       MOVE    1               TO  FLG-SANTEI
                   END-IF
               END-IF
      *
               IF     FLG-SANTEI           =   ZERO
                   MOVE    "tbl_santei"        TO  MCP-TABLE
                   MOVE    "key3"              TO  MCP-PATHNAME
                   PERFORM 920-SANTEI-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      WRK-YMD(1:6)        =   SPACE
               MOVE    SPACE           TO  WRK-YMD
           END-IF
      *
           .
       10001-KYUSEI-YMD-EXT.
           EXIT.
      *
      *****************************************************************
      *    精神通院の同日初診編集処理
      *****************************************************************
       10002-DOUJIT-YMD-SEC                SECTION.
      *
      *    同日初診の診療科設定
           MOVE    SPA-SRYYMD          TO  WRK-SRYYMD-1
           IF      WRK-SRYMM           <=  6
               COMPUTE WRK-SRYMM       =  (WRK-SRYMM   +   12 )
                                                       -   6
               COMPUTE WRK-SRYYY       =   WRK-SRYYY   -   1
           ELSE
               COMPUTE WRK-SRYMM       =   WRK-SRYMM   -   6
           END-IF
           IF      WRK-SRYYM-1         <   "200604"
               MOVE    "200604"        TO  WRK-SRYYM-1
           END-IF
           IF      (SPA-DOUYMD         =   SPACE )  OR
                  ((SPA-DOUYMD     NOT =   SPACE )  AND
                   (SPA-SYOYMD         >   SPA-DOUYMD OR
                    SPA-SYOYMD-OLD     >   SPA-DOUYMD ))
               CONTINUE
           ELSE
      *        同日初診
               MOVE    "111011810"         TO  WRK-SRYCD
               PERFORM 10021-DOUSYOSIN-SRYKA-SEC
      *        同日初診（労災）
               MOVE    "101110040"         TO  WRK-SRYCD
               PERFORM 10021-DOUSYOSIN-SRYKA-SEC
           END-IF
           .
       10002-DOUJIT-YMD-EXT.
           EXIT.
      *
      *****************************************************************
      *    同日初診算定処理
      *****************************************************************
       10021-DOUSYOSIN-SRYKA-SEC                SECTION.
      *
      *    算定履歴検索
           MOVE    SPACE               TO  SANTEI-REC
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
      *    労災毎とする
           IF     (SPA-HKNKBN          =   1   )  AND
                  (WRK-SRYCD           =   "101110040")
               MOVE    SPA-HKNCOMBINUM     TO  SANTEI-HKNCOMBINUM
           ELSE
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           END-IF
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           MOVE    SPACE               TO  WRK-YMD
           PERFORM
               UNTIL       FLG-SANTEI      =   1
      *
               IF     (SANTEI-MSANTEICNT   >   ZERO )  AND
                      (SANTEI-SRYYM        >=  WRK-SRYYM-1 )  AND
                      (SANTEI-SRYYM        <=  SPA-SRYYMD(1:6))
      *            最後の発生日、当日以降は対象外
                   IF      SANTEI-SRYYM        =   SPA-SRYYMD(1:6)
                       MOVE    SPA-SRYYMD(7:2)     TO  IDD2
                   ELSE
                       MOVE    31                  TO  IDD2
                   END-IF
                   MOVE    ZERO                TO  WRK-DD
                   PERFORM VARYING     IDD     FROM    IDD2  BY  -1
                           UNTIL       IDD     <   1
                       IF      SANTEI-DAY (IDD)    >   ZERO
                           MOVE    IDD             TO  WRK-DD
                           MOVE    1               TO  IDD
                       END-IF
                   END-PERFORM
                   IF      WRK-DD              >   ZERO
                       PERFORM 100211-DOU-SANTEI-SEC
                   END-IF
               END-IF
      *
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           END-PERFORM
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       10021-DOUSYOSIN-SRYKA-EXT.
           EXIT.
      *****************************************************************
      *    同日初診診療科設定処理 
      *****************************************************************
       100211-DOU-SANTEI-SEC               SECTION.
      *
           MOVE    SPACE               TO  SRYACT-REC
           INITIALIZE                  SRYACT-REC
           MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  SRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  SRY-PTID
           MOVE    SANTEI-SRYYM        TO  SRY-SRYYM
           MOVE    WRK-SRYCD           TO  SRY-SRYCD(1)
           MOVE    WRK-SRYCD           TO  SRY-SRYCD(2)
           MOVE    WRK-SRYCD           TO  SRY-SRYCD(3)
           MOVE    WRK-SRYCD           TO  SRY-SRYCD(4)
           MOVE    WRK-SRYCD           TO  SRY-SRYCD(5)
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-READ-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
           PERFORM
                   UNTIL   FLG-SRYACT      =   1
      *        剤回数取得
               MOVE    SPACE               TO  SRYACCT-REC
               INITIALIZE                      SRYACCT-REC
               MOVE    SRY-HOSPNUM         TO  ACCT-HOSPNUM
               MOVE    SRY-NYUGAIKBN       TO  ACCT-NYUGAIKBN
               MOVE    SRY-PTID            TO  ACCT-PTID
               MOVE    SRY-SRYKA           TO  ACCT-SRYKA
               MOVE    SRY-SRYYM           TO  ACCT-SRYYM
               MOVE    SRY-SRYKBN          TO  ACCT-SRYKBN
               MOVE    SRY-ZAINUM          TO  ACCT-ZAINUM
      *        主キー読み
               PERFORM 940-SRYACCT-READ-SEC
               IF      FLG-SRYACCT         =   ZERO
                   MOVE    SPACE           TO  WRK-YMD
                   MOVE    ACCT-SRYYM      TO  WRK-YMD(1:6)
                   MOVE    ZERO            TO  WRK-DD
                   PERFORM VARYING     IDX-Y2  FROM    31 BY  -1
                           UNTIL       IDX-Y2  <   1
                       IF      ACCT-DAY (1 IDX-Y2) >   ZERO
                           MOVE    IDX-Y2          TO  WRK-DD
                           IF      WRK-YMD         >   SPA-SRYYMD
                               MOVE    ZERO        TO  WRK-DD
                           ELSE
                               MOVE    1               TO  IDX-Y2
                           END-IF
                       END-IF
                   END-PERFORM
                   IF      WRK-DD              =   ZERO
                       MOVE    SPACE           TO  WRK-YMD
                   END-IF
                   PERFORM VARYING     IDX     FROM    1   BY  1
                           UNTIL      (IDX     >   10  )
                                  OR  (WRK-YMD =   SPACE )
                       IF      WRK-DOU-SRYKA (IDX) =   SPACE
                           MOVE    ACCT-SRYKA      TO  WRK-DOU-SRYKA
                                                             (IDX)
                           MOVE    WRK-YMD         TO  WRK-DOU-SRYYMD
                                                             (IDX)
                           MOVE    10              TO  IDX
                       ELSE
                           IF      WRK-DOU-SRYKA (IDX)
                                                   =   ACCT-SRYKA
                               IF     WRK-DOU-SRYYMD (IDX)
                                                   <   WRK-YMD
                                   MOVE    WRK-YMD TO  WRK-DOU-SRYYMD
                                                             (IDX)
                               END-IF
                               MOVE    10              TO  IDX
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-READ-SEC
           END-PERFORM
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
     *
           .
       300911-DOUSYOSIN-SRYKA-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤別のチェック、編集、自動算定処理
      *****************************************************************
       100-SEL-SYORI-SEC                SECTION.
      *
      *    チェック、自動算定
           PERFORM 1001-NAI-CHK-SEC
           IF      SPA-ERRCD(1:1)  =   "K"
               MOVE    SPA-ERRCD       TO  WRK-ERRCD-H
               MOVE    SPACE           TO  SPA-ERRCD
           ELSE
               MOVE    SPACE           TO  WRK-ERRCD-H
           END-IF
           IF     (WRK-GMN-INPUTCD(IDX-MAX)(1:1)
                                       =   "."   )  OR
                  (SPA-ERRCD       NOT =   SPACE )  OR
                  (LNK-ORCSC-KIDCD NOT =   SPACE )
               GO      TO      100-SEL-SYORI-EXT
           END-IF
      *
      *    点数計算
           PERFORM 1002-TENSU-KEI-SEC
           IF      WRK-ERRCD-H     NOT =   SPACE
               MOVE    WRK-ERRCD-H     TO  SPA-ERRCD
           END-IF
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
      *    剤内に早期リハビリ存在チェック
      **** PERFORM 10010-SOUKI-CHK-SEC
      *
           MOVE    ZERO                TO  IDX-MAX
           MOVE    ZERO                TO  IDX-SYU
           INITIALIZE                  WRK-AREA
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE  )   OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *
               ADD     1                   TO  IDX-MAX
      *
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1)     =   "." OR "S"
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
                       PERFORM 10012-CHK2-SEC
                   WHEN    "8"
                   WHEN    "."
                   WHEN    "S"
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
           IF     (WRK-ERRCD           =   SPACE)   AND
                  (IDX-SANSO           >   ZERO )
               PERFORM 10011-SANSO-ADD-SEC
           END-IF
      *
           MOVE    WRK-ERRCD          TO  SPA-ERRCD
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
      *            乳幼児の自動加算を追加する
                   PERFORM 10011-NENREI-SET-SEC
      *            施設基準不適合チェック
                   IF      WRK-COM-KIJUNTEIGENKBN(IDX-Z)
                                               NOT =   ZERO
                       PERFORM 100111-KIJUNN-CHK-SEC
                   END-IF
      *            労災の時、労災加算の算定
                   IF      SPA-HKNKBN          =   1
                     AND  (SPA-RSI-JUNKYO  NOT =   "1" )
                       PERFORM 100113-RSIHKN-SYORI-SEC
                   ELSE
                       IF      WRK-COM-DATAKBN (IDX-Z) =   "1"
      *                    リハビリの上限回数
                           MOVE    SPACE               TO  WRK-GEN-SRYCD
                           IF      LNK-ORCSC-PG        =   "K02"
                               PERFORM 100112-RIGAKU-CHK-SEC
                           END-IF
                       END-IF
                   END-IF
      *
      *            精神通院のチェック
                   IF      LNK-ORCSC-PG        =   "K02"
                       PERFORM 100113-SEISIN-CHK-SEC
                   END-IF
      *
      *            加算チェック
                   IF      WRK-COM-DATAKBN (IDX-Z) =   "2"
                       PERFORM 1005-KASAN-CHK-SEC
                   END-IF
      *        器材
               WHEN    "7"
      ***********  IF      WRK-COM-DATAKBN (IDX-Z) =   "3"
      *                フィルムの時エラー
      *                MOVE    "0047"          TO  WRK-ERRCD
      *                MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
      ************ END-IF
      *            酸素区分判定
                   IF      WRK-COM-SANSOKBN (IDX-Z)    =   1
      *                酸素の時、酸素補正率を自動加算
                       MOVE    WRK-COM-INPUTCD (IDX-Z) TO  WRK-SRYCD
                       PERFORM 100115-SANSO-CHK-SEC
                       IF      FLG-SANSO-OK        =   1
                           MOVE    IDX-Z       TO  IDX-SANSO
                       ELSE
                           IF      IDX-SANSO        =   ZERO
                               MOVE    "0038"       TO  WRK-ERRCD
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
      *****************************************************************
      *    酸素判定処理
      *****************************************************************
       100115-SANSO-CHK-SEC           SECTION.
      *
      *
           SET     TBL-SANSO           TO  1
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
      *    急性発生日チェック処理
      *****************************************************************
       10012-CHK2-SEC                  SECTION.
      *
      *    急性発生終了
           IF      WRK-COM-INPUTCD(IDX-Z)  =   "099800102"
               IF      SPA-HKNKBN          =   1
                   MOVE    "8025"              TO  WRK-ERRCD
                   MOVE    "1"                 TO  WRK-COM-CUR (IDX-Z)
               END-IF
           END-IF
      *
      *    診療行為のみでチェック
           IF     (WRK-ERRCD           =   SPACE  )  AND
                  (LNK-ORCSC-PG        =   "K02"  )
               PERFORM 100121-KYSEI-CHK-SEC
           END-IF
      *
      *    自賠責明細料・診断料の時、数量は１であること
           IF     (WRK-COM-INPUTCD(IDX-Z)(1:5) =   "09591"  OR
                                                   "09592"  OR
                                                   "09593"  OR
                                                   "09594"  )
      **     AND  (WRK-COM-KISOTEN (IDX-Z)     >   ZERO     )
      *D       IF      WRK-COM-SURYO(IDX-Z)    >   1
               IF      WRK-COM-SURYO(IDX-Z)    NOT =   1
                   MOVE    "1002"              TO  WRK-ERRCD
                   MOVE    "1"                 TO  WRK-COM-CUR (IDX-Z)
               END-IF
           END-IF
      *
           .
       10012-CHK2-EXT.
           EXIT.
      *
      *****************************************************************
      *    急性発生日チェック処理
      *****************************************************************
       100121-KYSEI-CHK-SEC                  SECTION.
      *
      *    急性発生日
           IF      WRK-COM-INPUTCD(IDX-Z)  =   "099800101"
               IF      SPA-KYUSEI-END      =   SPA-SRYYMD
                   MOVE    "8023"             TO  WRK-ERRCD
                   MOVE    "1"                TO  WRK-COM-CUR (IDX-Z)
               END-IF
           END-IF
      *    急性発生終了
           IF      WRK-COM-INPUTCD(IDX-Z)  =   "099800102"
               IF     (SPA-KYUSEI-FLG      =   1    )  OR
                      (SPA-KYUSEI-YMD      =   SPA-SRYYMD)
                   MOVE    "8023"             TO  WRK-ERRCD
                   MOVE    "1"                TO  WRK-COM-CUR (IDX-Z)
               ELSE
                   IF     (SPA-KYUSEI-YMD  NOT =   SPACE )  AND
                          (SPA-KYUSEI-END      =   SPACE )
                       CONTINUE
                   ELSE
                       MOVE    "8024"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   END-IF
               END-IF
           END-IF
      *    リハビリ終了日
           EVALUATE    WRK-COM-INPUTCD(IDX-Z)
      *        心大血管疾患リハビリテーション
               WHEN    "099800111"
                   MOVE    "1"             TO  WRK-KYUSEI-ST (1)
               WHEN    "099800112"
                   MOVE    "1"             TO  WRK-KYUSEI-ED (1)
                   IF     (WRK-KYUSEI1-YMD     =   SPACE  )  AND
                          (WRK-KYUSEI-ST (1)   =   SPACE  )
                       MOVE    "8029"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   ELSE
                       IF     (WRK-KYUSEI1-YMD     =   SPA-SRYYMD) OR
                              (WRK-KYUSEI-ST (1)   =   "1"    )
                           MOVE    "8030"      TO  WRK-ERRCD
                           MOVE    "1"         TO  WRK-COM-CUR(IDX-Z)
                       END-IF
                   END-IF
      *        脳血管疾患リハビリテーション
               WHEN    "099800121"
                   MOVE    "1"             TO  WRK-KYUSEI-ST (2)
               WHEN    "099800122"
                   IF     (WRK-KYUSEI2-YMD     =   SPACE  )  AND
                          (WRK-KYUSEI-ST (2)   =   SPACE  )
                       MOVE    "8031"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   ELSE
                       IF     (WRK-KYUSEI2-YMD     =   SPA-SRYYMD) OR
                              (WRK-KYUSEI-ST (2)   =   "1"    )
                           MOVE    "8032"      TO  WRK-ERRCD
                           MOVE    "1"         TO  WRK-COM-CUR(IDX-Z)
                       END-IF
                   END-IF
      *        運動器リハビリテーション
               WHEN    "099800131"
                   MOVE    "1"             TO  WRK-KYUSEI-ST (3)
               WHEN    "099800132"
                   IF     (WRK-KYUSEI3-YMD     =   SPACE  )  AND
                          (WRK-KYUSEI-ST (3)   =   SPACE  )
                       MOVE    "8033"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   ELSE
                       IF     (WRK-KYUSEI3-YMD     =   SPA-SRYYMD) OR
                              (WRK-KYUSEI-ST (3)   =   "1"    )
                           MOVE    "8034"      TO  WRK-ERRCD
                           MOVE    "1"         TO  WRK-COM-CUR(IDX-Z)
                       END-IF
                   END-IF
      *        呼吸器リハビリテーション
               WHEN    "099800141"
                   MOVE    "1"             TO  WRK-KYUSEI-ST (4)
               WHEN    "099800142"
                   IF     (WRK-KYUSEI4-YMD     =   SPACE  )  AND
                          (WRK-KYUSEI-ST (4)   =   SPACE  )
                       MOVE    "8035"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   ELSE
                       IF     (WRK-KYUSEI4-YMD     =   SPA-SRYYMD) OR
                              (WRK-KYUSEI-ST (4)   =   "1"    )
                           MOVE    "8036"      TO  WRK-ERRCD
                           MOVE    "1"         TO  WRK-COM-CUR(IDX-Z)
                       END-IF
                   END-IF
      *        摂食機能療法終了日
               WHEN    "099800151"
                   MOVE    "1"             TO  WRK-KYUSEI-ST (5)
               WHEN    "099800152"
                   IF      WRK-KYUSEI5-CHK     =   SPACE
                       MOVE    "099800151"         TO  WRK-SRYCD
                       PERFORM 10001-KYUSEI-YMD-SEC
                       MOVE    WRK-YMD             TO  WRK-KYUSEI5-YMD
                       MOVE    "1"                 TO  WRK-KYUSEI5-CHK
                   END-IF
                   IF     (WRK-KYUSEI5-YMD     =   SPACE  )  AND
                          (WRK-KYUSEI-ST (5)   =   SPACE  )
                       MOVE    "8037"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   ELSE
                       IF     (WRK-KYUSEI5-YMD     =   SPA-SRYYMD) OR
                              (WRK-KYUSEI-ST (5)   =   "1"    )
                           MOVE    "8038"      TO  WRK-ERRCD
                           MOVE    "1"         TO  WRK-COM-CUR(IDX-Z)
                       END-IF
                   END-IF
      *        難病患者リハビリ
               WHEN    "099800161"
                   MOVE    "1"             TO  WRK-KYUSEI-ST (6)
               WHEN    "099800162"
                   MOVE    "1"             TO  WRK-KYUSEI-ED (6)
                   IF      WRK-KYUSEI6-CHK     =   SPACE
                       MOVE    "099800161"         TO  WRK-SRYCD
                       PERFORM 10001-KYUSEI-YMD-SEC
                       MOVE    WRK-YMD             TO  WRK-KYUSEI6-YMD
                       MOVE    "1"                 TO  WRK-KYUSEI6-CHK
                   END-IF
                   IF     (WRK-KYUSEI6-YMD     =   SPACE  )  AND
                          (WRK-KYUSEI-ST (6)   =   SPACE  )
                       MOVE    "8039"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   ELSE
                       IF     (WRK-KYUSEI6-YMD     =   SPA-SRYYMD) OR
                              (WRK-KYUSEI-ST (6)   =   "1"    )
                           MOVE    "8040"      TO  WRK-ERRCD
                           MOVE    "1"         TO  WRK-COM-CUR(IDX-Z)
                       END-IF
                   END-IF
      *        障害者リハビリ
               WHEN    "099800171"
                   MOVE    "1"             TO  WRK-KYUSEI-ST (7)
               WHEN    "099800172"
                   MOVE    "1"             TO  WRK-KYUSEI-ED (7)
                   IF      WRK-KYUSEI7-CHK     =   SPACE
                       MOVE    "099800171"         TO  WRK-SRYCD
                       PERFORM 10001-KYUSEI-YMD-SEC
                       MOVE    WRK-YMD             TO  WRK-KYUSEI7-YMD
                       MOVE    "1"                 TO  WRK-KYUSEI7-CHK
                   END-IF
                   IF     (WRK-KYUSEI7-YMD     =   SPACE  )  AND
                          (WRK-KYUSEI-ST (7)   =   SPACE  )
                       MOVE    "8041"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   ELSE
                       IF     (WRK-KYUSEI7-YMD     =   SPA-SRYYMD) OR
                              (WRK-KYUSEI-ST (7)   =   "1"    )
                           MOVE    "8042"      TO  WRK-ERRCD
                           MOVE    "1"         TO  WRK-COM-CUR(IDX-Z)
                       END-IF
                   END-IF
      *
               WHEN    "099800401"
                   MOVE    "1"                 TO  WRK-TEIGEN-IN
           END-EVALUATE
      *
           .
       100121-KYSEI-CHK-EXT.
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
                   MOVE    1               TO  FLG-ARI
                   MOVE    IDX-Z           TO  IDX1
               END-IF
           END-PERFORM
      *
           IF      FLG-ARI             =   ZERO
               MOVE    "0013"          TO  WRK-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
           END-IF
      *
      *    早期リハビリ加算チェック
           IF     (SPA-ERRCD           =   SPACE  )
             AND  (LNK-ORCSC-PG        =   "K02"  )
             AND  (WRK-COM-INPUTCD (IDX-Z) =   CON-RIHAKSN )
               IF     (SPA-HKNKBN          =   1   )
                 AND  (SPA-RSI-JUNKYO  NOT =   "1" )
      *            労災はチェックなし
                   CONTINUE
               ELSE
                   PERFORM 20011-RIHAKSN-CHK-SEC
               END-IF
           END-IF
      *
           .
       1005-KASAN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    早期リハビリ加算チェック処理（入院）
      *****************************************************************
       20011-RIHAKSN-CHK-SEC                SECTION.
      *
      *    各リハビリ発症日判定
           IF      WRK-KYUSEI          =   SPACE
               IF     (WRK-COM-KZMFLG (IDX-Z)   =   SPACE)  AND
                      (LNK-ORCSC-KEI-IDX        =   ZERO )
                   MOVE    IDX-Z               TO  LNK-ORCSC-KEI-IDX
                   MOVE    "1"                 TO  WRK-COM-KZMFLG
                                                           (IDX-Z)
                   MOVE    "K839"              TO  WRK-ERRCD
               END-IF
           ELSE
      *        発症日＋３０日
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    WRK-KYUSEI          TO  LNK-DAY6-KIJUN
               MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    30                  TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KOYOMI     TO  WRK-KYUSEI-30
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    LNK-DAY6-KOYOMI     TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               STRING  "発症日の３０日後は"  DELIMITED  BY  SIZE
                       LNK-DAY2-EDTYMD3      DELIMITED  BY  SIZE
                       "です"                DELIMITED  BY  SIZE
                      INTO                 WRK-KYUSEI-30-MSG
               END-STRING
      *        入力日分のチェック
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2    >   31  )   OR
                                  (WRK-ERRCD   NOT =   SPACE )
                   IF      WRK-COM-NYUINDAY (IDX-Z IDX2)   >   ZERO
                       MOVE    IDX2                TO  IDD
                       MOVE    SPA-SRYYMD          TO  WRK-SRYYMD
                       MOVE    IDD                 TO  WRK-SRYYMD (7:2)
                       IF     (WRK-KYUSEI-30       <   WRK-SRYYMD)
      *                 発生日＋３０日以上はエラー
                            MOVE    "8047"          TO  WRK-ERRCD
                            MOVE    "1"             TO  WRK-COM-CUR
                                                              (IDX-Z)
                            MOVE    WRK-KYUSEI-30-MSG
                                                    TO  LNK-ORCSC-ERRMSG
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           .
       20011-RIHAKSN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    施設基準不適合チェック加算処理
      *****************************************************************
       100111-KIJUNN-CHK-SEC           SECTION.
      *
      *    手技の時、適合チェック
           IF      WRK-COM-KIJUNTEIGENKBN (IDX-Z)  =   2  OR  3
               PERFORM 1001111-SSTKIJUN-CHK-SEC
      *        施設基準適応
               IF      FLG-ERR             =   ZERO
                   MOVE    1               TO  FLG-KIJUNN
                   GO      TO      100111-KIJUNN-CHK-EXT
               END-IF
      *        施設基準不適応
               IF     (WRK-COM-KIJUNTEIGENKBN (IDX-Z)  =   2 )  OR
                     ((WRK-COM-KIJUNTEIGENKBN (IDX-Z)  =   3   ) AND
                      (LNK-ORCSC-NENREI-YY             =   ZERO))
                   MOVE    2                   TO  FLG-KIJUNN
      *            施設基準不適合（放射線）（他の加算はないこととする）
                   MOVE    IDX-Z               TO  IDX-S
                   MOVE    "180025270"         TO  WRK-SRYCD
                   IF      WRK-COM-INPUTCD (IDX-Z + 1) =   WRK-SRYCD
                       CONTINUE
                   ELSE
                       MOVE    IDX-Z           TO  IDX-S
                       MOVE    ZERO            TO  FLG-ARI
                       PERFORM 1002-KASAN-SET-SEC
                  END-IF
              END-IF
           END-IF
      *
      *    加算の時、対象の手技が有ること
           IF      WRK-COM-KIJUNTEIGENKBN (IDX-Z)  =   1
               IF      FLG-KIJUNN          =   ZERO
                   MOVE    "0073"          TO  WRK-ERRCD
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
      *
           MOVE    ZERO                TO  FLG-ERR
      *    施設基準情報検索
           MOVE    SPACE               TO  SYS-1006-REC
           MOVE    "1006"              TO  SYS-1006-KANRICD
           IF      IDY                 <=  500
               MOVE    "1"                 TO  SYS-1006-KBNCD
           ELSE
               MOVE    "2"                 TO  SYS-1006-KBNCD
           END-IF
           MOVE    SPA-SRYYMD          TO  SYS-1006-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1006-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1006-HOSPNUM
           MOVE    SYS-1006-REC        TO  MCPDATA-REC
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
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1006-REC
           ELSE
               INITIALIZE                  SYS-1006-REC
           END-IF
      *
      *    基準不適合逓減対象施設基準
           MOVE    WRK-COM-KIJUNTEIGENCD (IDX-Z)
                                       TO  IDY
           IF      IDY                 <=  500
               IF      SYS-1006-SSTKIJUN (IDY) =   1
                   CONTINUE
               ELSE
                   MOVE    1               TO  FLG-ERR
               END-IF
           ELSE
               IF      SYS-1006-SSTKIJUN (IDY - 500) =   1
                   CONTINUE
               ELSE
                   MOVE    1               TO  FLG-ERR
               END-IF
           END-IF
      *
           .
       1001111-SSTKIJUN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリの回数チェック処理
      *****************************************************************
       100112-RIGAKU-CHK-SEC            SECTION.
      *
      *    包括まとめ入力は対象外
           IF      SPA-HKNCOMBINUM     =   "9999"
               GO      TO      100112-RIGAKU-CHK-EXT
           END-IF
      *
           SET     TBL-RIH             TO  1
           SEARCH  TBL-RIHABIRI-OCC    VARYING   TBL-RIH
                   AT  END
                       MOVE    ZERO            TO  FLG-RIHA-OK
                       MOVE    SPACE           TO  WRK-RIHA-KBN
                       MOVE    SPACE           TO  WRK-RIHA-KBN2
                       MOVE    SPACE           TO  WRK-RIHA-GEN
                   WHEN    WRK-COM-INPUTCD (IDX-Z)
                                               =   TBL-RIHA-SRYCD
                                                           (TBL-RIH)
                       MOVE    TBL-RIHA-KBN (TBL-RIH)
                                               TO  WRK-RIHA-KBN
                       MOVE    TBL-RIHA-KBN2 (TBL-RIH)
                                               TO  WRK-RIHA-KBN2
                       MOVE    TBL-RIHA-GEN (TBL-RIH)
                                               TO  WRK-RIHA-GEN
                       MOVE    1               TO  FLG-RIHA-OK
           END-SEARCH
      *    リハビリテーション料の労災コード
           IF     (WRK-COM-INPUTCD   (IDX-Z)(1:3)  =   "101")
              AND (WRK-COM-CDKBN-KBN (IDX-Z)       =   "H"  )
               MOVE    1                   TO  FLG-RIHA-OK
           END-IF
      *
           IF     (FLG-RIHA-OK         =   ZERO )
               GO      TO      100112-RIGAKU-CHK-EXT
           END-IF
      *    労災の時、逓減コードはエラーとする
           IF     (SPA-HKNKBN          =   1   )
             AND  (SPA-RSI-JUNKYO  NOT =   "1" )
               IF      WRK-RIHA-GEN        =   "2"
                   MOVE    "8044"      TO  WRK-ERRCD
                   MOVE    "1"         TO  WRK-COM-CUR (IDX-Z)
                   GO      TO      100112-RIGAKU-CHK-EXT
               END-IF
           END-IF
      *
      *    急性発生日計算処理
           PERFORM 1001121-KYUSEIYMD15-SEC
      *@@
      *    逓減開始日計算処理
      *    PERFORM 100112-TEIGENYMD15-SEC
      *    労災・自賠責は逓減なしとする
      *    IF     (SPA-HKNKBN          =   1   )
     ******  AND  (SPA-RSI-HKNKBN  NOT =   "5" )
      *      AND  (SPA-RSI-JUNKYO  NOT =   "1" )
      *        MOVE    SPACE           TO  WRK-TEIGEN
      *        MOVE    SPACE           TO  WRK-TEIGEN-YMD
      *        MOVE    SPACE           TO  WRK-TEIGEN-MSG
      **** END-IF
      *
      *@@
           IF     (SPA-NYUGAIKBN       =   "2" ) OR
                  (LNK-ORCSC-SYORIFLG  =   1   )
      *        外来か訂正の時の日上限と月上限のチェック
               PERFORM 2002-GAIRAI-ALL-CHK-SEC
           ELSE
      *        入院の日上限と月上限のチェック
               PERFORM 2001-NYUIN-ALL-CHK-SEC
           END-IF
      *
           MOVE    SPA-ERRCD           TO  WRK-ERRCD
           MOVE    SPACE               TO  SPA-ERRCD
      *
           .
       100112-RIGAKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     急性発生日計算処理
      *****************************************************************
       1001121-KYUSEIYMD15-SEC          SECTION.
      *
           MOVE    SPACE           TO  WRK-KYUSEI
           MOVE    SPACE           TO  WRK-KYUSEI-YMD
           MOVE    SPACE           TO  WRK-KYUSEI-MSG
      *    急性発生日計算処理
           EVALUATE    WRK-RIHA-KBN
               WHEN    "1"
                   MOVE    WRK-KYUSEI1-YMD     TO  WRK-KYUSEI
                   IF      WRK-KYUSEI-ST (1)   =   "1"
                       MOVE    SPA-SRYYMD          TO  WRK-KYUSEI
                   END-IF
                   MOVE    150                 TO  WRK-ZOGEN
               WHEN    "2"
                   MOVE    WRK-KYUSEI2-YMD     TO  WRK-KYUSEI
                   IF      WRK-KYUSEI-ST (2)   =   "1"
                       MOVE    SPA-SRYYMD          TO  WRK-KYUSEI
                   END-IF
                   MOVE    180                 TO  WRK-ZOGEN
               WHEN    "3"
                   MOVE    WRK-KYUSEI3-YMD     TO  WRK-KYUSEI
                   IF      WRK-KYUSEI-ST (3)   =   "1"
                       MOVE    SPA-SRYYMD          TO  WRK-KYUSEI
                   END-IF
                   MOVE    150                 TO  WRK-ZOGEN
               WHEN    "4"
                   MOVE    WRK-KYUSEI4-YMD     TO  WRK-KYUSEI
                   IF      WRK-KYUSEI-ST (4)   =   "1"
                       MOVE    SPA-SRYYMD          TO  WRK-KYUSEI
                   END-IF
                   MOVE    90                  TO  WRK-ZOGEN
           END-EVALUATE
      *
           IF      WRK-KYUSEI      NOT =   SPACE
      *    日後計算
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    WRK-KYUSEI          TO  LNK-DAY6-KIJUN
               MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KOYOMI     TO  WRK-KYUSEI-YMD
      *        発生日
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-KYUSEI-YMD      TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               STRING  "算定終了日："      DELIMITED  BY  SIZE
                       LNK-DAY2-EDTYMD3    DELIMITED  BY  SIZE
                      INTO                 WRK-KYUSEI-MSG
               END-STRING
           END-IF
      *
           .
       1001121-KYUSEIYMD15-EXT.
           EXIT.
      *@@
      *****************************************************************
      *     逓減開始日計算処理
      *****************************************************************
       100112-TEIGENYMD15-SEC          SECTION.
      *
           MOVE    SPACE           TO  WRK-TEIGEN
           MOVE    SPACE           TO  WRK-TEIGEN-YMD
           MOVE    SPACE           TO  WRK-TEIGEN-MSG
      *    急性発生日計算処理
           EVALUATE    WRK-RIHA-KBN
               WHEN    "1"
                   MOVE    WRK-KYUSEI1-YMD     TO  WRK-TEIGEN
                   IF      WRK-KYUSEI-ST (1)   =   "1"
                       MOVE    SPA-SRYYMD          TO  WRK-TEIGEN
                   END-IF
                   MOVE    120                 TO  WRK-ZOGEN
               WHEN    "2"
                   MOVE    WRK-KYUSEI2-YMD     TO  WRK-TEIGEN
                   IF      WRK-KYUSEI-ST (2)   =   "1"
                       MOVE    SPA-SRYYMD          TO  WRK-TEIGEN
                   END-IF
                   MOVE    140                 TO  WRK-ZOGEN
               WHEN    "3"
                   MOVE    WRK-KYUSEI3-YMD     TO  WRK-TEIGEN
                   IF      WRK-KYUSEI-ST (3)   =   "1"
                       MOVE    SPA-SRYYMD          TO  WRK-TEIGEN
                   END-IF
                   MOVE    120                 TO  WRK-ZOGEN
               WHEN    "4"
                   MOVE    WRK-KYUSEI4-YMD     TO  WRK-TEIGEN
                   IF      WRK-KYUSEI-ST (4)   =   "1"
                       MOVE    SPA-SRYYMD          TO  WRK-TEIGEN
                   END-IF
                   MOVE    80                  TO  WRK-ZOGEN
           END-EVALUATE
      *    逓減対象外のコード算定の時、逓減としない
           IF    ((WRK-TEIGEN-GAI  NOT =   SPACE )
              AND (WRK-TEIGEN-GAI      >=  WRK-TEIGEN ))
               OR (WRK-TEIGEN-IN       =   "1"   )
               MOVE    SPACE               TO  WRK-TEIGEN
           END-IF
      *
           IF      WRK-TEIGEN      NOT =   SPACE
      *    日後計算
      *        逓減開始日
               ADD     1                   TO  WRK-ZOGEN
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    WRK-TEIGEN          TO  LNK-DAY6-KIJUN
               MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KOYOMI     TO  WRK-TEIGEN-YMD
      *        発生日
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-TEIGEN-YMD      TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               STRING  "逓減開始日："      DELIMITED  BY  SIZE
                       LNK-DAY2-EDTYMD3    DELIMITED  BY  SIZE
                      INTO                 WRK-TEIGEN-MSG
               END-STRING
           END-IF
           .
       100112-TEIGENYMD15-EXT.
           EXIT.
      *@@
      *
      *****************************************************************
      *     精神通院の加算チェック処理
      *****************************************************************
       100113-SEISIN-CHK-SEC            SECTION.
      *
      *    包括まとめ入力は対象外
           IF      SPA-HKNCOMBINUM     =   "9999"
               CONTINUE
           ELSE
               IF      SPA-NYUGAIKBN       =   "1"
      *            入院
                   PERFORM 1001131-NYUIN-SEISIN-SEC
               ELSE
      *            入院外
                   PERFORM 1001132-GAIRI-SEISIN-SEC
               END-IF
           END-IF
           .
       100113-SEISIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     外来　精神通院の加算チェック処理
      *****************************************************************
       1001132-GAIRI-SEISIN-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-NENREI
      *
           IF      LNK-ORCSC-SYORIFLG      =   1
      *        訂正時点の初診算定日
               MOVE    LNK-ORCSC-SYOYMD-OLD    TO  WRK-SYOYMD
           ELSE
               MOVE    LNK-ORCSC-SYOYMD        TO  WRK-SYOYMD
           END-IF
      *    同じ科で同日初診がある時、同日初診
           IF      WRK-COM-SRYKA (IDX-Z)   =   SPACE
               MOVE    SPA-SRYKA           TO  WRK-SRYKA
           ELSE
               MOVE    WRK-COM-SRYKA (IDX-Z)   TO  WRK-SRYKA
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   10  )   OR
                              (WRK-DOU-SRYKA (IDX) =   SPACE)
               IF      WRK-DOU-SRYKA (IDX) =   WRK-SRYKA
                   IF      WRK-DOU-SRYYMD(IDX) >   WRK-SYOYMD
                       MOVE    WRK-DOU-SRYYMD(IDX)
                                               TO  WRK-SYOYMD
                   END-IF
                   MOVE    10          TO  IDX
               END-IF
           END-PERFORM
      *    初診算定日から６月以内
           IF     (WRK-SYOYMD          =   SPACE  )  OR
                  (LNK-ORCSC-SYOSIN2   =   1      )
               MOVE    SPA-SRYYMD          TO  WRK-SYOYMD
           END-IF
           MOVE    WRK-SYOYMD          TO  WRK-SYOYMD2
      *    初診料算定日＋６月後
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-SYOYMD          TO  LNK-DAY6-KIJUN
           MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    6                   TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KOYOMI     TO  WRK-SYOYMD
      *    ６ヶ月以上
      *    IF      SPA-SRYYMD          >   WRK-SYOYMD
      *        MOVE    1               TO  FLG-NENREI
      **** END-IF
      *    Ｈ２０．４から１年未満とする
           COMPUTE WRK-SYO2-YY         =   WRK-SYO2-YY   +   1
           IF      SPA-SRYYMD          >=  WRK-SYOYMD2
               MOVE    1               TO  FLG-NENREI
           END-IF
      *
           EVALUATE    WRK-COM-INPUTCD(IDX-Z)
               WHEN    "180020570"
      *            逓減チェック
                   IF      WRK-COM-INPUTCD (IDX-Z - 1)
                                               =   "180012210"  OR
                                                   "180012310"
                                               OR  "180020410"
      *            家族
                                               OR  "180007250"
                                               OR  "180007350"
      *            ３０分未満
                                               OR  "180031010"
                                               OR  "180031110"
                                               OR  "180031210"
                                               OR  "180031310"
                       IF      SPA-SRYYMD      >=  WRK-SYOYMD2
                           IF     (WRK-COM-KZMFLG (IDX-Z)   =   SPACE)
                           AND    (LNK-ORCSC-KEI-IDX        =   ZERO )
                               MOVE    IDX-Z       TO  LNK-ORCSC-KEI-IDX
                               MOVE    "1"         TO  WRK-COM-KZMFLG
                                                           (IDX-Z)
                               MOVE    "0215"      TO  WRK-ERRCD
                               MOVE    "1"         TO  WRK-COM-CUR
                                                           (IDX-Z)
                           END-IF
                       END-IF
                   ELSE
                       MOVE    "0083"      TO  WRK-ERRCD
                       MOVE    "1"         TO  WRK-COM-CUR (IDX-Z)
                   END-IF
           END-EVALUATE
      *
           .
       1001132-GAIRI-SEISIN-EXT.
           EXIT.
      *
      *****************************************************************
      *     入院　精神通院の加算チェック処理
      *****************************************************************
       1001131-NYUIN-SEISIN-SEC            SECTION.
      *
      *    警告チェック済みの時、チェックなし
           IF     (WRK-COM-KZMFLG (IDX-Z)  NOT =   SPACE)
               GO      TO      1001131-NYUIN-SEISIN-EXT
           END-IF
      *
           MOVE    ZERO                TO  WRK-TUKI-9
           MOVE    SPA-K02N-NYUINYMD   TO  WRK-NYUINYMD
           MOVE    ZERO                TO  FLG-SEISIN
           EVALUATE    WRK-COM-INPUTCD(IDX-Z)
      *        入院精神療法（１）
               WHEN    "180018110"
                   MOVE    3                   TO  WRK-TUKI-9
      *        入院精神療法（２）（６月以内）
               WHEN    "180012010"
                   MOVE    6                   TO  WRK-TUKI-9
      *        入院集団精神療法
               WHEN    "180016710"
                   MOVE    6                   TO  WRK-TUKI-9
      *        入院生活技能訓練療法（６月以内）
               WHEN    "180018210"
                   MOVE    6                   TO  WRK-TUKI-9
      *        家族入院精神療法（１）
               WHEN    "180028850"
                   MOVE    3                   TO  WRK-TUKI-9
                   MOVE    1                   TO  FLG-SEISIN
      *        家族入院精神療法（２）（６月以内）
               WHEN    "180028950"
                   MOVE    6                   TO  WRK-TUKI-9
                   MOVE    1                   TO  FLG-SEISIN
      *        家族入院精神療法（２）（６月超）
               WHEN    "180020950"
                   MOVE    1                   TO  FLG-SEISIN
                   PERFORM 10011310-NYUINYMD-HEN-SEC
           END-EVALUATE
           IF      WRK-TUKI-9          =   ZERO
               GO      TO      1001131-NYUIN-SEISIN-EXT
           END-IF
      *    初回入院日
           PERFORM 10011310-NYUINYMD-HEN-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      1001131-NYUIN-SEISIN-EXT
           END-IF
      *    入院日＋Ｎ月後
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-NYUINYMD        TO  LNK-DAY6-KIJUN
           MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-TUKI-9          TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KOYOMI     TO  WRK-NYUIN-YMD
      *TEST
      *    DISPLAY "WRK-COM-INPUTCD(IDX-Z):" WRK-COM-INPUTCD(IDX-Z)
      *      ",WRK-NYUINYMD:"  WRK-NYUINYMD
      *      ", WRK-NYUIN-YMD:"  WRK-NYUIN-YMD
      *TEST
      *    入力日分のチェック
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   31  )   OR
                              (WRK-ERRCD   NOT =   SPACE )
               IF      WRK-COM-NYUINDAY (IDX-Z IDX2)   >   ZERO
                   MOVE    IDX2                TO  IDD
                   MOVE    SPA-SRYYMD          TO  WRK-SRYYMD
                   MOVE    IDD                 TO  WRK-SRYYMD (7:2)
                   IF      WRK-SRYYMD          >   WRK-NYUIN-YMD
                       MOVE    "KXXX"              TO  WRK-ERRCD
                   END-IF
               END-IF
           END-PERFORM
           IF      WRK-ERRCD       NOT =   SPACE
               IF     (WRK-COM-KZMFLG (IDX-Z)  =   SPACE)
               AND    (LNK-ORCSC-KEI-IDX       =   ZERO )
                   IF      WRK-TUKI-9          =   6
                       IF      FLG-SHOKAIKBN       =   ZERO
                           MOVE    "K840"              TO  WRK-ERRCD
                       ELSE
                           MOVE    "K842"              TO  WRK-ERRCD
                       END-IF
                   ELSE
                       IF      FLG-SHOKAIKBN       =   ZERO
                           MOVE    "K841"              TO  WRK-ERRCD
                       ELSE
                           MOVE    "K843"              TO  WRK-ERRCD
                       END-IF
                   END-IF
                   MOVE    IDX-Z               TO  LNK-ORCSC-KEI-IDX
                   MOVE    "1"                 TO  WRK-COM-KZMFLG
                                                           (IDX-Z)
                   MOVE    "1"                 TO  WRK-COM-CUR
                                                           (IDX-Z)
               ELSE
                   MOVE    SPACE               TO  WRK-ERRCD
               END-IF
           END-IF
      *
           .
       1001131-NYUIN-SEISIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初回入院日判定処理
      *****************************************************************
       10011310-NYUINYMD-HEN-SEC        SECTION.
      *
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
           MOVE    SPA-K02N-NYUINYMD   TO  PTNYUINRRK-NYUINYMD
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key12"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key12"             TO  MCP-PATHNAME
               PERFORM 910-PTNYUINRRK-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
           MOVE    ZERO                TO  WRK-SHONUM
           MOVE    ZERO                TO  FLG-SHOKAIKBN
           PERFORM
                   UNTIL      (FLG-PTNYUINRRK      =   1 )
               IF     (PTNYUINRRK-NYUINYMD     =   SPA-K02N-NYUINYMD)
                  AND (PTNYUINRRK-TAIINYMD     =   SPA-K02N-TAIINYMD)
                  IF      PTNYUINRRK-SHOKAIKBN     =   "2"
      *                継続
                       MOVE    PTNYUINRRK-SHONUM   TO  WRK-SHONUM
                       MOVE    1                   TO  FLG-SHOKAIKBN
                   END-IF
                   MOVE    1                   TO  FLG-PTNYUINRRK
               END-IF
               IF      FLG-PTNYUINRRK          =   ZERO
                   MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
                   MOVE    "key12"             TO  MCP-PATHNAME
                   PERFORM 910-PTNYUINRRK-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key12"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF     (FLG-SHOKAIKBN       =   1   )
           AND    (FLG-SEISIN          =   1   )
      *        継続の時、家族は算定不可
               IF     (WRK-COM-KZMFLG (IDX-Z)  =   SPACE)
               AND    (LNK-ORCSC-KEI-IDX       =   ZERO )
                   MOVE    "K844"              TO  WRK-ERRCD
                   MOVE    IDX-Z               TO  LNK-ORCSC-KEI-IDX
                   MOVE    "1"                 TO  WRK-COM-KZMFLG
                                                           (IDX-Z)
                   MOVE    "1"                 TO  WRK-COM-CUR
                                                           (IDX-Z)
               END-IF
           END-IF
      *    継続の時、初回入院日検索
           IF     (WRK-SHONUM          >   ZERO )
           AND    (FLG-SEISIN          =   ZERO )
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
               MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
               MOVE    SPA-K02N-NYUINYMD   TO  PTNYUINRRK-NYUINYMD
               MOVE    WRK-SHONUM          TO  PTNYUINRRK-SHONUM
      *
               MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key13"             TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "key13"             TO  MCP-PATHNAME
                   PERFORM 910-PTNYUINRRK-READ-SEC
               ELSE
                   MOVE    1               TO  FLG-PTNYUINRRK
                   INITIALIZE                  PTNYUINRRK-REC
               END-IF
               IF      FLG-PTNYUINRRK      =   ZERO
                   MOVE    PTNYUINRRK-NYUINYMD     TO  WRK-NYUINYMD
               END-IF
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key13"             TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-IF
      *
           .
       10011310-NYUINYMD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災加算の算定処理
      *****************************************************************
       100113-RSIHKN-SYORI-SEC        SECTION.
      *
           MOVE    IDX-Z               TO  HZN-IDX
      *
      *    回数チェク
           IF     (WRK-COM-DATAKBN (IDX-Z) =   "1"  )
               IF      LNK-ORCSC-PG        =   "K02"
                   PERFORM 100112-RIGAKU-CHK-SEC
                   MOVE    HZN-IDX             TO  IDX-Z
               END-IF
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      100113-RSIHKN-SYORI-EXT
           END-IF
      *    健保のリハビリ料はエラー
           MOVE    WRK-COM-INPUTCD (IDX-Z)     TO  WRK-SRYCD2
           PERFORM 10011312-RIGAKU-SERCH-SEC
           IF      FLG-RIHA-OK         =   1
               MOVE    "8046"          TO  WRK-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
               GO      TO      100113-RSIHKN-SYORI-EXT
           END-IF
      *
      *    リハビリテーション総合計画評価料は対象外
           IF      WRK-COM-INPUTCD (IDX-Z) =   "180024710"
               GO      TO      100113-RSIHKN-SYORI-EXT
           END-IF
      *
      *    労災の時、加算なし（リハビリ料は区分に’Ｈ’）
           IF     (WRK-COM-INPUTCD(IDX-Z)(1:3) =   "101" )
      *        四肢加算チェック
               IF      WRK-COM-INPUTCD (IDX-Z) =   "101800010" 
                   MOVE    2               TO  WRK-COM-SANFLG (IDX-Z)
               END-IF
               IF     (WRK-COM-CDKBN-KBN (IDX-Z)   NOT =   "H" )
                   OR (WRK-COM-INPUTCD (IDX-Z) =   "101800010" )
                   OR (WRK-COM-DATAKBN (IDX-Z) =   "2"  )
                   GO      TO      100113-RSIHKN-SYORI-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE                TO  WRK-SRYCD
           MOVE    ZERO                 TO  FLG-ROUSAI
      *    労災用　区分番号
           IF      WRK-COM-CDKBN-KBN (IDX-Z)  =   "H"
             AND  (WRK-COM-DATAKBN (IDX-Z) =   "1"  )
               EVALUATE    WRK-COM-CDKBN-KBNNUM (IDX-Z)
                   WHEN    "000"
                   WHEN    "001"
                   WHEN    "002"
                   WHEN    "003"
                   WHEN    "004"
                   WHEN    "005"
                       EVALUATE    SPA-RSI-SISIKBN
                           WHEN    1
                           WHEN    2
      *                        １．５倍
                               MOVE    "101800010"      TO  WRK-SRYCD
                       END-EVALUATE
                       MOVE    1                    TO  FLG-ROUSAI
               END-EVALUATE
           END-IF
      *    労災コードのリハビリ料
           IF     (WRK-COM-INPUTCD(IDX-Z)(1:3) =   "101" )
              AND (WRK-COM-CDKBN-KBN (IDX-Z)   =   "H"  )
              AND (WRK-COM-DATAKBN   (IDX-Z)   =   "1"  )
               EVALUATE    SPA-RSI-SISIKBN
                   WHEN    1
                   WHEN    2
      *                １．５倍
                       MOVE    "101800010"      TO  WRK-SRYCD
               END-EVALUATE
               MOVE    1                    TO  FLG-ROUSAI
           END-IF
      *??  老人のコードでリハビリは対象とする
      *    IF      FLG-ROUSAI          =   ZERO
      *        MOVE    WRK-COM-INPUTCD (IDX-Z)     TO  WRK-SRYCD2
      *        PERFORM 10011312-RIGAKU-SERCH-SEC
      *        IF      FLG-RIGAKU          =   1
      *            EVALUATE    SPA-RSI-SISIKBN
      *                WHEN    1
      *                WHEN    2
      *                    １．５倍とする
      *                    MOVE    "101800010"     TO  WRK-SRYCD
      *            END-EVALUATE
      *            MOVE    1                    TO  FLG-ROUSAI
      *        END-IF
      *??  END-IF
      *
      *    既に他の労災加算が入力されている時は自動算定しない
           MOVE    ZERO                TO  FLG-ROU-ARI
           COMPUTE IDX-Z2              =   IDX-Z     +   1
           PERFORM VARYING     IDX1    FROM    IDX-Z2    BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE  )   OR
                              (FLG-ROU-ARI             =   1    ) OR
                              (WRK-GMN-INPUTCD (IDX1)  =   SPACE )
               IF       WRK-COM-INPUTCD (IDX1) =   "101800010"  OR
                                                   "099509901"
                   IF      WRK-COM-AUTOKBN (IDX1)  =   SPACE
                        MOVE    1                  TO  FLG-ROU-ARI
                   END-IF
               END-IF
               IF     (WRK-COM-INPUTCD (IDX1)(1:1) =  "1" ) AND
                      (WRK-COM-DATAKBN (IDX1)      =  "1" )
                  MOVE    SPA-MAX-LINE             TO  IDX1
               END-IF
           END-PERFORM
           IF      FLG-ROU-ARI         =   1
               IF      FLG-ROUSAI          =   ZERO
                   MOVE    "8004"          TO  WRK-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (IDX1 - 1)
               END-IF
               GO      TO      100113-RSIHKN-SYORI-EXT
           END-IF
           IF      WRK-SRYCD           =   SPACE
               GO      TO      100113-RSIHKN-SYORI-EXT
           END-IF
      *
      *    加算料チェック処理（次ぎの手技料の前に）
           MOVE    ZERO                TO  FLG-ARI
           MOVE    ZERO                TO  IDX-T
           COMPUTE IDX-Z2              =   IDX-Z     +   1
           PERFORM VARYING     IDX1    FROM    IDX-Z2    BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE   )   OR
                              (FLG-ARI                 =   1    ) OR
                              (WRK-GMN-INPUTCD (IDX1)  =   SPACE )
               IF       WRK-COM-INPUTCD (IDX1) =   WRK-SRYCD
                   MOVE    IDX1            TO  IDX-T
                   MOVE    1               TO  FLG-ARI
               ELSE
      *!四肢加算は手技料の直下とする
      *!           IF     (WRK-COM-INPUTCD (IDX1)(1:1) =  "1" ) AND
      *                   (WRK-COM-DATAKBN (IDX1)      =  "2" )
      *               MOVE    IDX1             TO  IDX-T
      *!           END-IF
                   IF     (WRK-COM-INPUTCD (IDX1)(1:1) =  "1" ) AND
                          (WRK-COM-DATAKBN (IDX1)      =  "1" )
                      MOVE    SPA-MAX-LINE          TO  IDX1
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      FLG-ARI             =   ZERO
               IF      IDX-T           =   ZERO
                   MOVE    IDX-Z           TO  IDX-S
               ELSE
                   MOVE    IDX-T           TO  IDX-S
               END-IF
           END-IF
      *
      *    加算料追加処理
           PERFORM 1002-KASAN-SET-SEC
           IF      FLG-ADD             =   1
               MOVE    2               TO  WRK-COM-SANFLG (IDX-S)
           END-IF
           .
       100113-RSIHKN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリテーション料判定処理
      *****************************************************************
       10011312-RIGAKU-SERCH-SEC            SECTION.
      *
      *
           SET     TBL-RIH             TO  1
           SEARCH  TBL-RIHABIRI-OCC    VARYING   TBL-RIH
                   AT  END
                       MOVE    ZERO            TO  FLG-RIHA-OK
                   WHEN    WRK-COM-INPUTCD (IDX-Z)
                                               =   TBL-RIHA-SRYCD
                                                           (TBL-RIH)
                       MOVE    1               TO  FLG-RIHA-OK
           END-SEARCH
      *
           .
       10011312-RIGAKU-SERCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    年齢加算追加処理（共通）
      *****************************************************************
       10011-NENREI-SET-SEC            SECTION.
      *
      *    通院精神の時、初診日から６ヶ月以内チェック
           IF     (WRK-COM-CDKBN-KBN (IDX-Z)   =   "I" )  AND
                  (WRK-COM-CDKBN-KBNNUM(IDX-Z) =   "002")
               IF      (LNK-ORCSC-PG        =   "K02"
                                            OR  "KT01"  )
                   PERFORM 100113-SEISIN-CHK-SEC
               ELSE
                   MOVE    1               TO  FLG-NENREI
               END-IF
           ELSE
               MOVE    ZERO            TO  FLG-NENREI
           END-IF
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2        >   4       )   OR
                              (FLG-NENREI  =   1       )   OR
                              (WRK-COM-AGEKSNSRYCD(IDX-Z IDX2)
                                               =   SPACE  OR
                                                   ZERO )
      *        加算追加
               MOVE    WRK-COM-AGEKSNSRYCD (IDX-Z IDX2)  TO  WRK-SRYCD
      *        加算料チェック処理
               PERFORM 100111-KASAN-CHK-SEC
      *        加算料追加処理
               MOVE    IDX-Z               TO  IDX-S
               PERFORM 1002-KASAN-SET-SEC
               IF      FLG-ADD             =   1
                   MOVE    4                   TO  IDX2
               END-IF
           END-PERFORM
      *
           .
       10011-NENREI-SET-EXT.
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
                   UNTIL      (IDX1        >   SPA-MAX-LINE   )   OR
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
           MOVE    WRK-SCR-REC         TO  HZN-SCR-REC
           INITIALIZE                  ORCSC92AREA
           IF      FLG-ARI             =   ZERO
               MOVE    "1"                 TO  LNK-SC92-KBN
           ELSE
               MOVE    SPACE               TO  WRK-COM-CHKFLG (IDX-S)
           END-IF
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC92-SYORIFLG
           MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
           MOVE    IDX-S               TO  LNK-SC92-GMN-IDX
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    LNK-ORCSC-NENREI    TO  LNK-SC92-NENREI
           MOVE    LNK-ORCSC-ROUJIN    TO  LNK-SC92-ROUJIN
           MOVE    LNK-ORCSC-LSTSRYKA  TO  LNK-SC92-LSTSRYKA
           MOVE    LNK-ORCSC-LSTSRYKAMEI   TO  LNK-SC92-LSTSRYKAMEI
           MOVE    LNK-ORCSC-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
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
               MOVE    HZN-SCR-REC         TO  WRK-SCR-REC
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
      *    点数計算編集処理
      *****************************************************************
       1002-TENSU-KEI-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDX-MAX
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  ORCSC90AREA
           MOVE    1                   TO  IDX-STR
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE )   OR
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
      *        手技毎
               INITIALIZE                   WRK-COM-PLUS-AREA(IDX-Z)
      *
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1)     =   "."  OR  "S"
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
      *                自賠責
                       PERFORM 10021-JIHI-KEI-SEC
                   WHEN    "8"
                   WHEN    "."
                   WHEN    "S"
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
      *    割合計算加算
           IF      IDX-SYU             >   ZERO
               COMPUTE IDX-Z           =   IDX-MAX    +  1
               PERFORM 100212-TENSUKEI-SEC
           END-IF
      *    合計点数計算・編集
           MOVE    "2"                 TO  LNK-SC90-KBN
           MOVE    IDX-SYU             TO  LNK-SC90-IDX
           MOVE    IDX-STR             TO  LNK-SC90-IDX-STR
           MOVE    IDX-MAX             TO  LNK-SC90-IDX-END
           CALL    "ORCSC90TEN"        USING
                                       ORCSC90AREA
                                       WRK-SCR-REC
      *    計
           COMPUTE WRK-COM-KEI (IDX-MAX)   =   WRK-COM-TENSU  (IDX-MAX)
                                           *   LNK-ORCSC-KAISU
      *    セット用合計点数
           MOVE    WRK-COM-KEI  (IDX-MAX)  TO  LNK-ORCSC-ZAIKEI
           MOVE    WRK-COM-TENSU(IDX-MAX)  TO  LNK-ORCSC-ZAITEN
      *
           .
       1002-TENSU-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自賠責計算処理
      *****************************************************************
       10021-JIHI-KEI-SEC                  SECTION.
      *
           IF      WRK-COM-INPUTCD(IDX-Z)(1:5) =   "09591"  OR
                                                   "09592"  OR
                                                   "09593"  OR
                                                   "09594"
               CONTINUE
           ELSE
               GO      TO      10021-JIHI-KEI-EXT
           END-IF
      *
      *    自賠責明細料・診断料
           COMPUTE WRK-COM-KISOTEN(IDX-Z)  =   WRK-COM-TEN (IDX-Z)
                                           /   10
      *
           ADD     WRK-COM-KISOTEN (IDX-Z) TO  WRK-SUM-TENGAKEI
           MOVE    WRK-SUM-TENGAKEI        TO  LNK-SC90-TENSU
      *    回数
           MOVE    LNK-ORCSC-KAISU         TO  WRK-COM-KAISU (IDX-Z)
      *
           MOVE    SPACE                   TO  WRK-COM-KEIFLG (IDX-Z)
      *
           .
       10021-JIHI-KEI-EXT.
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
               IF      IDX-SYU             >   ZERO
      *            手技料の計算
                   MOVE    1               TO  FLG-SYUGI
                   PERFORM 100212-TENSUKEI-SEC
      *            点数計算
                   PERFORM 1007-TENSU-SUM-SEC
               END-IF
               MOVE    IDX-Z               TO  IDX-SYU
           END-IF
      *
      *    手技点数計算
           EVALUATE    WRK-MOJI
               WHEN    "1"
                   PERFORM 100211-SYUGITEN-KEI-SEC
           END-EVALUATE
      *    回数
           MOVE    LNK-ORCSC-KAISU     TO  WRK-COM-KAISU (IDX-Z)
      *
           .
       10021-KEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    手技点数計算処理
      *****************************************************************
       100211-SYUGITEN-KEI-SEC              SECTION.
      *
           IF      WRK-COM-TENSIKIBETU (IDX-Z)     =   5
                                                   OR  6
               CONTINUE
           ELSE
      *        通則加算対象外区分判定
               IF      WRK-COM-TUSOKUGAIKBN(IDX-Z) =   "1"
                   ADD     WRK-COM-KISOTEN (IDX-Z) TO  WRK-SUM-TENGAKEI
               ELSE
                   ADD     WRK-COM-KISOTEN(IDX-Z)  TO  WRK-SUM-TENSUKEI
               END-IF
           END-IF
           .
       100211-SYUGITEN-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    手技料の計算処理
      *****************************************************************
       100212-TENSUKEI-SEC             SECTION.
      *
           MOVE    IDX-Z               TO  HZN-IDX
           PERFORM 1006-KASAN-SET-SEC
      *    手技料の合計を編集する
           MOVE    LNK-SC90-TENSU      TO  WRK-COM-SYUTEN2 (IDX-SYU)
      *
      *    労災の外来管理加算読み替え処理
           IF      SPA-HKNKBN          =   1
      *****  AND  (SPA-RSI-HKNKBN  NOT =   "5" )
             AND  (SPA-RSI-JUNKYO  NOT =   "1" )
      *!       回数を掛けた点数で判定する
      *!         （２単位以上の時、読み替えはないはず）
      *!       COMPUTE WRK-SYOKEI      =   WRK-COM-SYUTEN2 (IDX-SYU)
      *!                               *   LNK-ORCSC-KAISU
               MOVE    WRK-COM-SYUTEN2 (IDX-SYU)
                                       TO  WRK-SYOKEI
               IF     (WRK-COM-RSIGIRI (IDX-SYU)
                                           =   1  )  AND
                      (WRK-SYOKEI          <   LNK-ORCSC-GAIRAI-TEN)
                   MOVE     LNK-ORCSC-GAIRAI-TEN
                                           TO  LNK-SC90-TENSU
               END-IF
           END-IF
           COMPUTE LNK-SC90-TENSU      =   LNK-SC90-TENSU
                                       +   WRK-SUM-TENGAKEI
      *
           .
       100212-TENSUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    割合加算計算処理
      *****************************************************************
       1006-KASAN-SET-SEC              SECTION.
      *
           MOVE    ZERO                TO  WRK-TENSUKEI-G
           MOVE    ZERO                TO  WRK-TENSUKEI-K
      *
           PERFORM VARYING     IDX     FROM    IDX-SYU  BY  1
                   UNTIL       IDX         >=  HZN-IDX
      *      手技料の点数を所定点数とする
      *      ％加算
             IF       (WRK-COM-INPUTCD(IDX)(1:3)   NOT =   "101") AND
                      (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   5   )  AND
                      (WRK-COM-SANFLG     (IDX)    =   ZERO)
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   COMPUTE WRK-TENSUKEI        =   WRK-COM-KISOTEN
                                                             (IDX-SYU)
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.5
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-K
               END-IF
      *        ％減算（労災減算も計算する）
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   6   )  AND
                      (WRK-COM-SANFLG     (IDX)    =   ZERO)
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   COMPUTE WRK-TENSUKEI        =   WRK-COM-KISOTEN
                                                             (IDX-SYU)
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-G
               END-IF
           END-PERFORM
      *
           COMPUTE LNK-SC90-TENSU      =   WRK-SUM-TENSUKEI
                                       +   WRK-TENSUKEI-K
                                       -   WRK-TENSUKEI-G
      *
      *    労災の時、労災加算
           IF      SPA-HKNKBN          =   1
      *****  AND  (SPA-RSI-HKNKBN  NOT =   "5" )
             AND  (SPA-RSI-JUNKYO  NOT =   "1" )
               PERFORM 10061-RSIKASAN-SET-SEC
           END-IF
      *
           .
       1006-KASAN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災割合加算計算処理
      *****************************************************************
       10061-RSIKASAN-SET-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-TENSUKEI-RSI
           MOVE    ZERO                TO  IDX-RSI
      *    労災加算対象点数集計
           PERFORM VARYING     IDX     FROM    IDX-SYU  BY  1
                   UNTIL       IDX         >=  HZN-IDX
      *
      *        手技料の点数を所定点数とする
      *        労災１．５ ％加算
               IF     (WRK-COM-INPUTCD(IDX)(1:3)   =   "101")  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   5   )  AND
                      (WRK-COM-SANFLG     (IDX)    =   2   )
                   MOVE    ZERO                TO  WRK-TENSUKEI
      *****        COMPUTE WRK-TENSUKEI        =   LNK-SC90-TENSU
                   COMPUTE WRK-TENSUKEI        =  (WRK-COM-KISOTEN
                                                             (IDX-SYU)
                                               -   WRK-TENSUKEI-G )
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.5
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-RSI
               END-IF
      *        労災リハ（１．２）加算
               IF     (WRK-COM-INPUTCD(IDX)(1:3)   =   "101")  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   5   )  AND
                      (WRK-COM-SANFLG     (IDX)    =   0   )
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   COMPUTE WRK-TENSUKEI        =  (WRK-COM-KISOTEN
                                                             (IDX-SYU)
                                               -   WRK-TENSUKEI-G )
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.5
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-RSI
               END-IF
           END-PERFORM
      *
           COMPUTE LNK-SC90-TENSU      =   LNK-SC90-TENSU
                                       +   WRK-TENSUKEI-RSI
      *
           .
       10061-RSIKASAN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    手技毎の計算処理
      *****************************************************************
       1007-TENSU-SUM-SEC            SECTION.
      *
           MOVE    "1"                 TO  LNK-SC90-KBN
           MOVE    IDX-SYU             TO  LNK-SC90-IDX
           MOVE    IDX-STR             TO  LNK-SC90-IDX-STR
           COMPUTE LNK-SC90-IDX-END    =   IDX-Z   -   1
           CALL    "ORCSC90TEN"        USING
                                       ORCSC90AREA
                                       WRK-SCR-REC
      *
           MOVE    IDX-Z               TO  IDX-STR
           INITIALIZE                  WRK-TENSU-KEISAN
           MOVE    ZERO                TO  LNK-SC90-TENSU
           .
       1007-TENSU-SUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来の日上限と月上限のチェックと自動逓減処理
      *****************************************************************
       2002-GAIRAI-ALL-CHK-SEC                SECTION.
      *
           MOVE    SPA-SRYYMD(7:2)    TO  IDD
           MOVE    SPA-SRYYMD         TO  WRK-SRYYMD
      *@@
      *    逓減チェック処理
      *    IF      WRK-SRYYMD         >=  "20070401"
      *        PERFORM 20020-TEIGEN-CHEK-SEC
      *    END-IF
      *@@
      *
           MOVE    ZERO               TO  WRK-DAYCNT
      *
      *    当日上限チェック
           COMPUTE WRK-KAISU           =   WRK-COM-SURYO(IDX-Z)
                                       *   WRK-COM-KAISU(IDX-Z)
      *
           PERFORM 20021-DAYMAX-CHK-SEC
      *
           .
       2002-GAIRAI-ALL-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    逓減チェック処理（外来）
      *****************************************************************
       20020-TEIGEN-CHEK-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYCD
           IF     (WRK-TEIGEN-YMD  NOT =   SPACE )  AND
                  (WRK-TEIGEN-YMD      <=  WRK-SRYYMD)
      *        逓減
               IF      WRK-RIHA-GEN        =   "2"
                   CONTINUE
               ELSE
      *        逓減コードに変更
                   SET     TBL-RIH             TO  1
                   SEARCH  TBL-RIHABIRI-OCC    VARYING   TBL-RIH
                       AT  END
                           MOVE    SPACE           TO  WRK-SRYCD
                       WHEN   (TBL-RIHA-KBN (TBL-RIH)
                                               =   WRK-RIHA-KBN )
                       AND    (TBL-RIHA-KBN2(TBL-RIH)
                                               =   WRK-RIHA-KBN2 )
                       AND    (TBL-RIHA-GEN (TBL-RIH)
                                               =   "2"    )
                           MOVE    TBL-RIHA-SRYCD (TBL-RIH)
                                                   TO  WRK-SRYCD
                   END-SEARCH
               END-IF
           END-IF
           IF     (WRK-TEIGEN-YMD  NOT =   SPACE )  AND
                  (WRK-TEIGEN-YMD      >   WRK-SRYYMD)
      *        逓減期間外
               IF      WRK-RIHA-GEN        =   "2"
      *        逓減を変更
                   SET     TBL-RIH             TO  1
                   SEARCH  TBL-RIHABIRI-OCC    VARYING   TBL-RIH
                       AT  END
                           MOVE    SPACE           TO  WRK-SRYCD
                       WHEN   (TBL-RIHA-KBN (TBL-RIH)
                                               =   WRK-RIHA-KBN )
                       AND    (TBL-RIHA-KBN2(TBL-RIH)
                                               =   WRK-RIHA-KBN2 )
                       AND    (TBL-RIHA-GEN (TBL-RIH)
                                               =   SPACE  )
                           MOVE    TBL-RIHA-SRYCD (TBL-RIH)
                                                   TO  WRK-SRYCD
                   END-SEARCH
               END-IF
           END-IF
      *
           IF      (WRK-SRYCD     NOT =   SPACE )  AND
                   (WRK-SRYCD     NOT =   WRK-COM-INPUTCD (IDX-Z))
               PERFORM 200201-TENGEN-HEN-SEC
           END-IF
      *
           .
       20020-TEIGEN-CHEK-EXT.
           EXIT.
      *****************************************************************
      *    リハビリコード置換え処理
      *****************************************************************
       200201-TENGEN-HEN-SEC                SECTION.
      *
      *    点数マスタ編集・基本チェック
           MOVE    WRK-SCR-REC         TO  HZN-SCR-REC
      *
           MOVE    WRK-SRYCD           TO  WRK-COM-INPUTCD (IDX-Z)
           MOVE    WRK-SRYCD           TO  WRK-GMN-INPUTCD (IDX-Z)
           INITIALIZE                  ORCSC92AREA
           MOVE    SPACE               TO  LNK-SC92-KBN
           MOVE    SPACE               TO  WRK-COM-CHKFLG (IDX-Z)
           MOVE    SPACE               TO  WRK-COM-HCHKFLG (IDX-Z)
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC92-SYORIFLG
           MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
           MOVE    IDX-Z               TO  LNK-SC92-GMN-IDX
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    LNK-ORCSC-NENREI    TO  LNK-SC92-NENREI
           MOVE    LNK-ORCSC-ROUJIN    TO  LNK-SC92-ROUJIN
           MOVE    LNK-ORCSC-LSTSRYKA  TO  LNK-SC92-LSTSRYKA
           MOVE    LNK-ORCSC-LSTSRYKAMEI   TO  LNK-SC92-LSTSRYKAMEI
           MOVE    LNK-ORCSC-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       WRK-SCR-REC
                                       SPA-AREA
      *
      *    エラーの時、変更なし
           IF     (SPA-ERRCD           NOT =   SPACE )  OR
                  (LNK-SC92-ERRAREA    NOT =   SPACE )
               MOVE    SPACE               TO  SPA-ERRCD
               MOVE    HZN-SCR-REC         TO  WRK-SCR-REC
           ELSE
      *    画面再編集・基本チェック
               INITIALIZE                  ORCSC94AREA
               MOVE    "1"                 TO  LNK-SC94-KBN
               MOVE    LNK-SC92-PG         TO  LNK-SC94-PG
               MOVE    IDX-Z               TO  LNK-SC94-IDX
               CALL    "ORCSC94"           USING
                                           ORCSC94AREA
                                           WRK-SCR-REC
                                           SPA-AREA
      *
           END-IF
           .
       200201-TENGEN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    当日上限チェック処理
      *****************************************************************
       20021-DAYMAX-CHK-SEC         SECTION.
      *
      *    急性発生日以降の月上限
           MOVE    ZERO                TO  WRK-TUKCNT
           IF     (WRK-KYUSEI-YMD  NOT =   SPACE )  AND
                  (WRK-KYUSEI-YMD      <   WRK-SRYYMD)
               IF      WRK-KYUSEI-YMD(1:6)    =   WRK-SRYYMD(1:6)
                   MOVE    WRK-KYUSEI-YMD(7:2)    TO  IDE
                   ADD     1                      TO  IDE
               ELSE
                   MOVE    1                      TO  IDE
               END-IF
               MOVE    ZERO                TO  WRK-TUKI-CNT
               PERFORM VARYING     IDE2    FROM    IDE   BY  1
                       UNTIL       IDE2    >   31
                   ADD     WRK-RIHA-DAY (IDE2) TO  WRK-TUKI-CNT
               END-PERFORM
               COMPUTE WRK-TUKCNT          =   WRK-TUKI-CNT
                                           +   WRK-KAISU
           END-IF
      *    急性発生日以降
           IF     (WRK-KYUSEI-YMD  NOT =   SPACE )  AND
                  (WRK-KYUSEI-YMD      <   WRK-SRYYMD)
      *        上限日数越えで 月１３単位以上
               IF     (WRK-COM-KZMFLG (IDX-Z)   =   SPACE)  AND
                      (LNK-ORCSC-KEI-IDX        =   ZERO )
                   MOVE    IDX-Z           TO  LNK-ORCSC-KEI-IDX
                   MOVE    "1"             TO  WRK-COM-KZMFLG (IDX-Z)
                   EVALUATE    WRK-RIHA-KBN
                       WHEN    "1"
                           MOVE    "K831"          TO  SPA-ERRCD
                           IF      WRK-TUKCNT          >   13
                               MOVE    "K835"          TO  SPA-ERRCD
                           END-IF
                       WHEN    "2"
                           MOVE    "K832"          TO  SPA-ERRCD
                           IF      WRK-TUKCNT          >   13
                               MOVE    "K836"          TO  SPA-ERRCD
                           END-IF
                       WHEN    "3"
                           MOVE    "K833"          TO  SPA-ERRCD
                           IF      WRK-TUKCNT          >   13
                               MOVE    "K837"          TO  SPA-ERRCD
                           END-IF
                       WHEN    "4"
                           MOVE    "K834"          TO  SPA-ERRCD
                           IF      WRK-TUKCNT          >   13
                               MOVE    "K838"          TO  SPA-ERRCD
                           END-IF
                   END-EVALUATE
                   MOVE    WRK-KYUSEI-MSG  TO  LNK-ORCSC-ERRMSG
                   MOVE    "1"             TO  WRK-COM-CUR  (IDX-Z)
               END-IF
           END-IF
      *
           COMPUTE WRK-DAYCNT          =   WRK-RIHA-CNT (IDD)
                                       +   WRK-KAISU
      *    １日９単位でエラー
           IF      SPA-ERRCD           =   SPACE
               IF     (WRK-RIHA-DAY (IDD) +   WRK-DAYCNT)
                                           >    9
                   IF      SPA-NYUGAIKBN   =   "1"
                       MOVE    SPACE           TO  LNK-ORCSC-ERRMSG
                       STRING  IDD             DELIMITED  BY  SIZE
                               "日の"
                                               DELIMITED  BY  SIZE
                                           INTO    LNK-ORCSC-ERRMSG
                       END-STRING
                   END-IF
                   IF      SPA-HKNKBN      =   1
      *******        AND  (SPA-RSI-HKNKBN  NOT =   "5" )
                     AND  (SPA-RSI-JUNKYO  NOT =   "1" )
                       IF     (WRK-COM-KZMFLG (IDX-Z)   =   SPACE)  AND
                              (LNK-ORCSC-KEI-IDX        =   ZERO )
                           MOVE    IDX-Z       TO  LNK-ORCSC-KEI-IDX
                           MOVE    "1"         TO  WRK-COM-KZMFLG(IDX-Z)
                           MOVE    "8027"      TO  SPA-ERRCD
                           MOVE    "1"         TO  WRK-COM-CUR (IDX-Z)
                       END-IF
                   ELSE
                       MOVE    "8027"          TO  SPA-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR  (IDX-Z)
                   END-IF
               END-IF
           END-IF
      *    １日６単位で警告
           IF      SPA-ERRCD           =   SPACE
               IF     (WRK-RIHA-DAY (IDD) +   WRK-DAYCNT)
                                          >    6
                   IF     (WRK-COM-KZMFLG (IDX-Z)   =   SPACE)  AND
                          (LNK-ORCSC-KEI-IDX        =   ZERO )
                       MOVE    IDX-Z       TO  LNK-ORCSC-KEI-IDX
                       MOVE    "1"         TO  WRK-COM-KZMFLG (IDX-Z)
                       MOVE    "K827"          TO  SPA-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR  (IDX-Z)
                   END-IF
               END-IF
           END-IF
      *    今回分集計
           IF      SPA-ERRCD           =   SPACE
               PERFORM 20023-KAISU-HEN-SEC
           END-IF
      *
           .
       20021-DAYMAX-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    今回分集計処理
      *****************************************************************
       20023-KAISU-HEN-SEC         SECTION.
      *
           ADD     WRK-KAISU       TO  WRK-RIHA-CNT (IDD)
      *
           .
       20023-KAISU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院全体チェック処理
      *****************************************************************
       2001-NYUIN-ALL-CHK-SEC         SECTION.
      *
      *@@
      *    逓減チェック処理
      *    IF     (SPA-SRYYMD         >=  "20070401" )
      *      AND  (WRK-TEIGEN-YMD NOT =   SPACE      )
      *        PERFORM  20010-TEIGEN-CHEKN-SEC
      *    END-IF
      *@@
      *    入力日分のチェック
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   31  )   OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      WRK-COM-NYUINDAY (IDX-Z IDX2)   >   ZERO
                   ADD     1                   TO  WRK-NYU-KAI
                   MOVE    IDX2                TO  IDD
                   MOVE    SPA-SRYYMD          TO  WRK-SRYYMD
                   MOVE    IDD                 TO  WRK-SRYYMD (7:2)
      *            当日上限チェック
                   COMPUTE WRK-KAISU           =   WRK-COM-SURYO(IDX-Z)
                                               *   WRK-COM-KAISU(IDX-Z)
                   PERFORM 20021-DAYMAX-CHK-SEC
               END-IF
           END-PERFORM
      *
           .
       2001-NYUIN-ALL-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    逓減チェック処理（入院）
      *****************************************************************
       20010-TEIGEN-CHEKN-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYCD
           MOVE    SPACE               TO  WRK-SRYYMD
           MOVE    SPACE               TO  WRK-EDSRYYMD
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   31  )   OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      WRK-COM-NYUINDAY (IDX-Z IDX2)   >   ZERO
                   MOVE    IDX2                TO  IDD
                   IF      WRK-SRYYMD          =   SPACE
                       MOVE    SPA-SRYYMD          TO  WRK-SRYYMD
                       MOVE    IDD                 TO  WRK-SRYYMD (7:2)
                   END-IF
                   MOVE    SPA-SRYYMD          TO  WRK-EDSRYYMD
                   MOVE    IDD                 TO  WRK-EDSRYYMD (7:2)
               END-IF
           END-PERFORM
      *
      *    逓減開始日の範囲チェック
           IF     (WRK-TEIGEN-YMD      <=  WRK-SRYYMD)
      *        逓減
               IF      WRK-RIHA-GEN        =   "2"
                   CONTINUE
               ELSE
      *        逓減コードに変更
                   SET     TBL-RIH             TO  1
                   SEARCH  TBL-RIHABIRI-OCC    VARYING   TBL-RIH
                       AT  END
                           MOVE    SPACE           TO  WRK-SRYCD
                       WHEN   (TBL-RIHA-KBN (TBL-RIH)
                                               =   WRK-RIHA-KBN )
                       AND    (TBL-RIHA-KBN2(TBL-RIH)
                                               =   WRK-RIHA-KBN2 )
                       AND    (TBL-RIHA-GEN (TBL-RIH)
                                               =   "2"    )
                           MOVE    TBL-RIHA-SRYCD (TBL-RIH)
                                                   TO  WRK-SRYCD
                   END-SEARCH
               END-IF
           END-IF
           IF     (WRK-TEIGEN-YMD      >   WRK-SRYYMD )  AND
                  (WRK-TEIGEN-YMD      >   WRK-EDSRYYMD)
      *        逓減期間外
               IF      WRK-RIHA-GEN        =   "2"
      *        逓減を変更
                   SET     TBL-RIH             TO  1
                   SEARCH  TBL-RIHABIRI-OCC    VARYING   TBL-RIH
                       AT  END
                           MOVE    SPACE           TO  WRK-SRYCD
                       WHEN   (TBL-RIHA-KBN (TBL-RIH)
                                               =   WRK-RIHA-KBN )
                       AND    (TBL-RIHA-KBN2(TBL-RIH)
                                               =   WRK-RIHA-KBN2 )
                       AND    (TBL-RIHA-GEN (TBL-RIH)
                                               =   SPACE  )
                           MOVE    TBL-RIHA-SRYCD (TBL-RIH)
                                                   TO  WRK-SRYCD
                   END-SEARCH
               END-IF
           END-IF
      *    逓減期間が混在する時、エラー
           IF     (WRK-TEIGEN-YMD      >   WRK-SRYYMD )  AND
                  (WRK-TEIGEN-YMD      <=  WRK-EDSRYYMD)
               MOVE    "8043"          TO  SPA-ERRCD
               MOVE    WRK-TEIGEN-MSG  TO  LNK-ORCSC-ERRMSG
               MOVE    "1"             TO  WRK-COM-CUR  (IDX-Z)
           END-IF
      *    逓減コード置換え
           IF      (WRK-SRYCD     NOT =   SPACE )  AND
                   (WRK-SRYCD     NOT =   WRK-COM-INPUTCD (IDX-Z))
               PERFORM 200201-TENGEN-HEN-SEC
           END-IF
           .
       20010-TEIGEN-CHEKN-EXT.
           EXIT.
      *
      *****************************************************************
      *    材料再計算編集処理
      *****************************************************************
       1007-KIZAI-TEN-SEC                SECTION.
      *
      *    酸素の点数数計算
           IF      WRK-SANSOKIN-SUM    NOT =   ZERO
               PERFORM 10071-SANSO-KEISAN-SEC
           END-IF
      *
           .
       1007-KIZAI-TEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     酸素の点数数計算処理
      *****************************************************************
       10071-SANSO-KEISAN-SEC                SECTION.
      *
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   IDX-MAX
               EVALUATE    WRK-COM-INPUTCD (IDX)
      *            酸素補正率自動加算
                   WHEN    "770020070"
      *                酸素金額＋（酸素金額×補正率／１００）　四捨五入
                       COMPUTE WRK-SYOKEIS     =   WRK-SANSOKIN-SUM
                                               *   WRK-COM-KISOTEN (IDX)
                                               /   100
                       COMPUTE WRK-SANSOKIN    =   WRK-SANSOKIN-SUM
                                               +   WRK-SYOKEIS
               END-EVALUATE
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   IDX-MAX
               EVALUATE    WRK-COM-INPUTCD (IDX)
      *            高気圧酸素計算
                   WHEN    "770030070"
      *                酸素金額＋（酸素金額×補正率／１００）
      *                                           ×気圧数　四捨五入
                       COMPUTE WRK-SYOKEIS     =   WRK-SANSOKIN
                                               *   WRK-COM-SURYO (IDX)
                       MOVE    WRK-SYOKEIS     TO  WRK-SANSOKIN
               END-EVALUATE
           END-PERFORM
      *    酸素の点数数計算
      *    金額計　四捨五入
           COMPUTE WRK-SYOKEI          =   WRK-SANSOKIN
                                       +   0.5
      *    金額計÷１０　四捨五入
           COMPUTE WRK-SYOKEIS2        =   WRK-SYOKEI
                                       /   10
                                       +   0.5
           MOVE    WRK-SYOKEIS2        TO  WRK-SANTEN
      *    酸素の点数を器材点数に加算
           ADD     WRK-SANTEN          TO  LNK-SC90-KIZTEN
      *
           .
       10071-SANSO-KEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    全診療行為チェック処理
      *****************************************************************
       200-ALL-SYORI-SEC                SECTION.
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE     )   OR
                              (SPA-ERRCD           NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *        リハビリ発生日と終了日の判定
               IF      WRK-COM-INPUTCD(IDX-Z)(1:7) =   "0998001"
                   PERFORM 2001-RIHACHK-SEC
               END-IF
           END-PERFORM
           .
       200-ALL-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリ発生日と終了日の判定処理
      *****************************************************************
       2001-RIHACHK-SEC              SECTION.
      *
           COMPUTE IDX-Z1              =   IDX-Z    +   1
           PERFORM VARYING     IDX-Z2  FROM    IDX-Z1  BY  1
                   UNTIL      (IDX-Z2      >   SPA-MAX-LINE )   OR
                              (SPA-ERRCD   NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z2)
                                               =   SPACE )
      *        リハビリ発生日と終了日の判定
               IF      WRK-COM-INPUTCD(IDX-Z)(1:8)
                                       =   WRK-COM-INPUTCD(IDX-Z2)(1:8)
                   EVALUATE    WRK-COM-INPUTCD(IDX-Z)(8:1)
                   WHEN    "1"
                       MOVE    "8030"          TO  SPA-ERRCD
                   WHEN    "2"
                       MOVE    "8032"          TO  SPA-ERRCD
                   WHEN    "3"
                       MOVE    "8034"          TO  SPA-ERRCD
                   WHEN    "4"
                       MOVE    "8036"          TO  SPA-ERRCD
                   WHEN    "5"
                       MOVE    "8038"          TO  SPA-ERRCD
                   WHEN    "6"
                       MOVE    "8040"          TO  SPA-ERRCD
                   WHEN    "7"
                       MOVE    "8042"          TO  SPA-ERRCD
                   END-EVALUATE
                   MOVE    "1"             TO  WRK-COM-CUR (IDX-Z2)
               END-IF
           END-PERFORM
           .
       2001-RIHACHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動算定処理
      *****************************************************************
       300-AUTO-SYORI-SEC              SECTION.
      *
           .
       300-AUTO-SYORI-EXT.
           EXIT.
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
      *    診療行為マスター読込
      *****************************************************************
       930-SRYACT-READ-SEC         SECTION.
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       930-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスター読込（主キー）
      *****************************************************************
       940-SRYACCT-READ-SEC           SECTION.
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  SRYACCT-REC
                   MOVE    ZERO                TO  FLG-SRYACCT
               ELSE
                   MOVE    1                   TO  FLG-SRYACCT
                   INITIALIZE                  SRYACCT-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
               INITIALIZE                  SRYACCT-REC
           END-IF
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       940-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴マスター読込
      *****************************************************************
       910-PTNYUINRRK-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTNYUINRRK-REC
               MOVE    ZERO                TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                      PTNYUINRRK-REC
               MOVE    1                   TO  FLG-PTNYUINRRK
           END-IF
           .
       910-PTNYUINRRK-READ-EXT.
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
