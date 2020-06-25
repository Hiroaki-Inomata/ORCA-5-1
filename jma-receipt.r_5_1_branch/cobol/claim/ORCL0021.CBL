      ******************************************************************
      * ORCA -- Online Receipt Computer Advantage
      *
      * Copyright (C)  2000-2001 JMA.         
      *
      * This module is part of ORCA.
      *
      *     ORCA is distributed in the hope that it will be useful, but
      * WITHOUT ANY WARRANTY.  No author or distributor accepts 
      * responsibility to anyone for the consequences of using it or for
      * whether it serves any particular purpose or works at all, unless
      * he says so in writing.
      * Refer to the GNU General Public License for full details. 
      *
      *     Everyone is granted permission to copy, modify and
      * redistribute ORCA, but only under the conditions described in
      * the GNU General Public License.  A copy of this license is
      * supposed to have been given to you along with ORCA so you can
      * know your rights and responsibilities.  It should be in a file
      * named COPYING.  Among other things, the copyright notice and 
      * this notice must be preserved on all copies. 
      ******************************************************************      *
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCL0021.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : ＣＬＡＩＭ
      *  コンポーネント名  : 点数金額モジュール用中間ファイル作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC-藤原      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  06/05/09  自費他追加
      *  01.00.02    NACL-多々納  06/08/08  収納変更対応
      *  03.03.00    NACL-多々納  06/10/23  返金対応追加
      *  03.05.00    NACL-多々納  07/05/09  グループ診療対応
      *  03.05.00    NACL-多々納  07/05/25  UIDテーブル対応
      *                                     明細数３０へ
      *                                     自費コード、画像診断対応
      *                                     新規・訂正追加
      *  03.05.01    NACL-多々納  07/08/16  患者メモ追加
      *  04.02.01    NACL-多々納  08/03/14  Ｈ２０年４月改正対応
      *  04.05.00    NACL-多々納  09/06/XX  数量小数５桁対応
      *  04.05.00    NACL-多々納  10/01/XX  治験・拡張漢字対応
      *  04.06.00    NACL-多々納  10/11/16  マル長負担額を保険負担額反映
      *  04.08.00    NACL-多々納  13/01/15  診療回数テーブル１０対応
      *  04.08.00    NACL-多々納  17/05/12  用法コード名称編集対応
      *  04.08.00    NACL-多々納  17/12/XX  剤オーバー対応
      *  05.01.00    NACL-多々納  20/02/XX  ユーザー単位対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      **** SELECT  LF03-FILE       ASSIGN  "/var/tmp/LF03V01"
      *                            ORGANIZATION    IS  INDEXED
      *                            ACCESS  MODE    IS  DYNAMIC
      *                            RECORD  KEY     IS  LF03-KEY
      ****                         FILE    STATUS  IS  STS-LF03.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *FD  LF03-FILE.
      *01  LF03-REC. 
      ***  COPY    "CPRLF003.INC".
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPCLAIM002.INC".
      *
           COPY    "CPCLAIM005.INC".
      *
           COPY    "CPCLAIM007.INC".
      *
           COPY    "CPCLAIM006.INC".
      *
      *    フラグ領域
      *01  STS-AREA.
      *    03  STS-LF03            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-SRYKARRK        PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-SYUMEI          PIC 9(01).
      ***  03  FLG-LF03            PIC 9(01).
           03  FLG-CLMUID          PIC 9(01).
           03  FLG-PTMEMOINF       PIC 9(01).
           03  FLG-SRYACCTPLUS     PIC 9(01).
           03  FLG-TENSUPLUS       PIC 9(01).
      *
           03  FLG-KAISU           PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
           03  FLG-SYOHOU          PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-GENTEN          PIC 9(01).
      *    剤オーバー
           03  FLG-ERROVER         PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-Y               PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-MAX             PIC 9(04).
           03  IDXA                PIC 9(04).
           03  IDX-ACT             PIC 9(02).
           03  IDX-REN             PIC 9(04).
           03  IDX-JYU             PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDXS                PIC 9(04).
      *
           03  IDX-LF              PIC 9(04).
           03  IDX-LF2             PIC 9(04).
           03  IDX-JI              PIC 9(04).
           03  IDX-IJ              PIC 9(04).
           03  IDX-IJ2             PIC 9(04).
           03  IDX-SINSATU         PIC 9(04).
           03  IDX-BUI             PIC 9(04).
           03  IDX-SATU            PIC 9(04).
           03  HZN-IJ              PIC 9(04).
      *
           03  IDX2                PIC 9(02).
           03  IDX3                PIC 9(02).
           03  IDXP                PIC 9(02).
           03  IDXM                PIC 9(02).
           03  IDK                 PIC 9(04).
           03  IDW                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-RENNUM          PIC 9(01).
           03  WRK-ZAINUM          PIC 9(08).
      *
           03  WRK-KAISU           PIC 9(05).
           03  WRK-SRYSYUKBN       PIC X(03).
           03  WRK-SRYKBN          PIC X(02).
           03  WRK-SRYKBN3         PIC X(03).
           03  WRK-SRYCD           PIC X(08).
           03  WRK-YOUHOU          PIC X(04).
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WYY         PIC X(04).
               05  WRK-WMM         PIC X(02).
               05  WRK-WDD         PIC X(02).
           03  WRK-IN              PIC X(01).
           03  WRK-TEN             PIC 9(13)V9(05).
      *
           03  WRK-KINGAK          PIC S9(08). 
           03  WRK-GOKTEN          PIC S9(08)V99.
      *
           03  WRK-SRYYMD          PIC X(08). 
           03  WRK-FILMCNT         PIC 9(02). 
           03  WRK-SURYO           PIC ZZZZZZZ9.999.
           03  WRK-SURYO-5         PIC ZZZZZ9.99999.
           03  WRK-FILMNUM         PIC ZZZZZZ.
           03  WRK-FILMDEV         PIC ZZ.
           03  WRK-Z9              PIC ----------------.
           03  WRK-RATE            PIC ---------------9.
           03  WRK-TANKA           PIC ZZZZZZZZZZZZZ.ZZ.
           03  WRK-TANKA9          PIC ZZZZZZZZZZZZ9.99.
           03  WRK-TANKA9N         PIC ------------9.99.
      *
           03  WRK-SYSYMD.
               05  WRK-SYSYY         PIC 9(04).
               05  WRK-SYSMM         PIC 9(02).
               05  WRK-SYSDD         PIC 9(02).
      *   きざみ値行為再計算用
           03  WRK-KEI-TEN         PIC 9(08)V9(03).
           03  WRK-KEI-TEN1        PIC 9(08)V9(03).
           03  WRK-KEI-TEN2        PIC 9(08).
           03  WRK-SURYO9          PIC 9(05)V9(05).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO9.
               05  WRK-SURYO-S1    PIC 9(05).
               05  WRK-SURYO-S2    PIC 9(05).
      *
           03  WRK-ZZZ             PIC Z99.
           03  WRK-999             PIC 9(03).
      *    今回入金充当額
           03  WRK-JYUTO-MONEY     PIC S9(07).
      *
      *    患者メモ分解
           03  WRK-MEMO                PIC X(1000).
           03  WRK-MEMO-G.
               05  WRK-MEMO-01         PIC X(1000).
               05  WRK-MEMO-02         PIC X(1000).
               05  WRK-MEMO-03         PIC X(1000).
               05  WRK-MEMO-04         PIC X(1000).
               05  WRK-MEMO-05         PIC X(1000).
               05  WRK-MEMO-OK         PIC X(1000).
      *
           03  WRK-KOUMOKU             PIC X(200).
      *
           03  WRK-ACCT-ZAITEN         PIC S9(07).
           03  WRK-ACCT-SYUTEN1        PIC S9(07).
           03  WRK-ACCT-KIZAITEN       PIC S9(07).
           03  WRK-ACCT-YKZTEN         PIC S9(07).
      *
      *    自費名称テーブル
       01  TBL-JIHIMSG-AREA.
           03  TBL-JIHIMSG             PIC X(20)  OCCURS   10.
      *
      *    集計用テーブル
       01  TBL-AREA.
           03  TBL-REC-01              OCCURS   30.
               05  TBL-SRYCD            PIC X(03).
               05  TBL-SRYNAME          PIC X(40).
               05  TBL-TENNSUALL        PIC S9(07).
               05  TBL-GINGAKUALL       PIC S9(07).
               05  TBL-ADMINISTRATION   PIC X(50).
      *用法コード
               05  TBL-ADMINCODE        PIC  X(09).
      *用法コメント
               05  TBL-ADMMEMO          PIC  X(100).
      *回数又は日数(Z編集)
               05  TBL-KAISU            PIC  9(03).
      *手技料合計点数(Z編集)
               05  TBL-SYUGI            PIC S9(07).
      *材料料合計点数(Z編集)
               05  TBL-ZAIRYOU          PIC S9(07).
      *薬剤料合計点数(Z編集)
               05  TBL-YAKUZAI          PIC S9(07).
      *包括区分包括フラグ(true OR false)
               05  TBL-PPSFLG           PIC  X(05).
      *明細テーブル
               05  TBL-SRYTBL           OCCURS  30.
      *診療種別区分コード(0:手技、1:材料,2:薬剤)
                   07  TBL-SRYSYUBETSU
                                       PIC  X(01).
      *医事請求コード(S + 診療行為コード)
                   07  TBL-IJICD      PIC  X(10).
      *医事請求コード体系名
                   07  TBL-TBLID      PIC  X(20).
      *医事請求コード
                   07  TBL-ALSCODE    PIC  X(10).
      *医事請求コード体系名
                   07  TBL-ALSTBLID   PIC  X(20).
      *医事請求コード名称
      *@@@@        07  TBL-ALSNAME    PIC  X(40).
                   07  TBL-ALSNAME    PIC  X(80).
      *数量(Z編集)
                   07  TBL-SURYO      PIC  S9(07)V9(5).
      *数量コード
                   07  TBL-SURYOCD    PIC  X(02).
      *単位
                   07  TBL-TANI        PIC  X(20).
      *請求点数(Z編集)
                   07  TBL-SEIKYUTEN   PIC  S9(07).
      *明細単価(Z編集)
                   07  TBL-MEISAITANKA
                                       PIC S9(07)V99.
      *所要時間(PTnHnM,5時間23分→PT5H23M)
                   07  TBL-DURATION    PIC  X(19).
      *部位
                   07  TBL-LOCATION    PIC  X(50).
      *フィルム
                   07  TBL-FILM       OCCURS  1.
      *フィルムサイズ
                     09  TBL-FILMSIZE PIC  X(50).
      *フィルムサイズコード
                     09  TBL-FILMSIZECD
                                       PIC  X(02).
      *フィルム分割
                     09  TBL-FILMDEV   PIC  9(02).
      *フィルム枚数(Z編集)
                     09  TBL-FILMNUM   PIC  9(06).
      *メモ
                   07  TBL-SRYMEMO     PIC  X(100).
      *括りメモ
               05  TBL-MEMO            PIC  X(1000).
      *
       01  WRK-FILMMAX             PIC 9(02)   VALUE   1.     
      *  
       01  WRK-HENYMD.
           03  WRK-HENYY           PIC X(04).
           03  FILLER              PIC X(01)   VALUE   "-".  
           03  WRK-HENMM           PIC X(02).
           03  FILLER              PIC X(01)   VALUE   "-".
           03  WRK-HENDD           PIC X(02).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *R02.2
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    診療科履歴
       01  SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    収納マスタワーク
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    収納明細マスタ−
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *    収納明細マスタ−
       01  HZN-SYUMEI-REC.
           COPY    "CPSYUMEI.INC"      REPLACING
                                       //SMEI-// BY //HZN-SMEI-//.
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1001.INC".
      *
           COPY    "CPSK1002.INC".
      *
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK1010.INC".
      *
           COPY    "CPSK1013.INC".
      *
      *    ＵＩＤテーブル
      *01  CLMUID-REC.
           COPY    "CPCLMUID.INC".
      *
      *    患者メモ
       01  PTMEMOINF-REC.
           COPY    "CPPTMEMOINF.INC".
      *
      *    診療会計附加情報マスタ−
       01  SRYACCTPLUS-REC.
           COPY    "CPSRYACCTPLUS.INC".
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
           03  SYS-TIME.
               05  SYS-HH          PIC 9(02).
               05  SYS-MI          PIC 9(02).
               05  SYS-SS          PIC 9(02).
               05  SYS-TT          PIC 9(02).
      *H28.4
      * 湿布薬（減）
       01  CONS-SPCM-SRYCD         PIC X(09)   VALUE    "820000047".
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *    ＵＩＤ取得用エリア
       01 UIDIO-AREA.
           03  C-RET   PIC X(2).
           03  C-UID   PIC X(36).
       01  ST          PIC 9(2).
      * 
           COPY    "CPMML0031.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      * 
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  LNK-KBN              PIC X(01).
       01  LNK-PTID             PIC 9(10).
       01  LNK-SRYKA            PIC X(02).
       01  LNK-HOSPNUM          PIC 9(02).
       01  LNK-SYSYMD           PIC 9(08).
       01  LNK-SRYYMD           PIC X(08).
       01  LNK-NYUGAIKBN        PIC X(01).
       01  LNK-HKNCOMBINUM      PIC X(04).
       01  LNK-DRCD             PIC X(05).
       01  LNK-DENPNUM          PIC 9(07).
       01  LNK-SYORIFLG         PIC X(01).
      *
       01  WRK-LF02-REC. 
           COPY    "CPRLF002.INC"    REPLACING  //LF02//
                                     BY         //WRK-LF02//.
      *
      ******************************************************************
      *
       PROCEDURE                       DIVISION
                                       USING   LNK-KBN
                                               LNK-PTID
                                               LNK-SRYKA
                                               LNK-HOSPNUM
                                               LNK-SYSYMD
                                               LNK-SRYYMD
                                               LNK-NYUGAIKBN
                                               LNK-HKNCOMBINUM
                                               LNK-DRCD
                                               LNK-DENPNUM
                                               LNK-SYORIFLG
                                               WRK-LF02-REC.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
            PERFORM 100-INIT-SEC
      *
            PERFORM 200-MAIN-SEC
      *
            PERFORM 300-END-SEC
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      ***  OPEN    I-O     LF03-FILE
      *
      *    MOVE    LNK-HOSPNUM     TO  LF03-HOSPNUM
      *    MOVE    LNK-PTID        TO  LF03-PTID
      *    MOVE    LNK-SRYKA       TO  LF03-SRYKA
      *    PERFORM 900-LF03-READ-SEC
      *    IF      FLG-LF03        =   1
      *        DISPLAY "*** ORCL0021 LF03 INVALID ***"
      *        MOVE    1               TO  FLG-END
      *****END-IF
      *
           INITIALIZE                  IDX-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    UID検索
           INITIALIZE                      CLMUID-REC
           MOVE    LNK-HOSPNUM         TO  SPA-HOSPNUM
                                           CLMUID-HOSPNUM
           MOVE    LNK-PTID            TO  CLMUID-PTID
           MOVE    LNK-SRYKA           TO  CLMUID-SRYKA
           PERFORM 903-CLMUID-READ-SEC
           IF      FLG-CLMUID          =   1
               DISPLAY "*** ORCL0021 CLMUID INVALID ***"
               MOVE    1               TO  FLG-END
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           INITIALIZE              SPA-AREA
      *
           MOVE    SPACE           TO  WRK-LF02-REC
           INITIALIZE              WRK-LF02-REC
      *    システム日付セット
           ACCEPT  SYS-YMD         FROM    DATE
           ACCEPT  SYS-TIME        FROM    TIME
           COMPUTE WRK-SYSYY       =   SYS-YY      +   2000
           MOVE    SYS-MM              TO  WRK-SYSMM
           MOVE    SYS-DD              TO  WRK-SYSDD
           MOVE    WRK-SYSYMD          TO  WRK-WYMD
           PERFORM 500-HIZUKE-HENSYU-SEC
           MOVE    WRK-HENYMD      TO  WRK-LF02-CONFIRMDATE1
      *
           MOVE    "facility"      TO  WRK-LF02-IDTYPE1
           MOVE    "medical"       TO  WRK-LF02-SRYKA-IDCODE
      *    ＳＰＡ設定
           MOVE    WRK-SYSYMD          TO  SPA-SYSYMD
           MOVE    LNK-HOSPNUM         TO  SPA-HOSPNUM
           MOVE    LNK-PTID            TO  SPA-PTID
           MOVE    LNK-SRYYMD          TO  SPA-SRYYMD
      *
      *    MOVE    LNK-PTID        TO  WRK-LF02-PTID1
      *    患者番号変換
           INITIALIZE                       ORCSPTIDAREA
           MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
           MOVE    LNK-PTID        TO  SPTID-PTID
           CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                           SPA-AREA
           MOVE    SPTID-PTNUM     TO  WRK-LF02-PTID1
      *
           MOVE    SPTID-PTNUM         TO  SPA-PTNUM
      *
           PERFORM 500-UUID-SET-SEC
           MOVE    C-UID           TO  WRK-LF02-UID1
           DISPLAY "L0021 C-UID = " "[" C-UID "]"
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX     >       20
                   OR      CLMUID-HKNCOMBI (IDX)  =   ZERO
               IF      LNK-HKNCOMBINUM     =   CLMUID-HKNCOMBI (IDX)
      *            MOVE    CLMUID-UID (IDX)    TO  WRK-LF02-UID1
                   MOVE    CLMUID-UID (IDX)    TO  WRK-LF02-HOKEN-UID
      *            MOVE    IDX                 TO  WRK-LF02-UID1
                   MOVE    20                  TO  IDX 
               END-IF
           END-PERFORM 
      *    保険組合せ
           PERFORM 2009-HKNCOMBI-HEN-SEC
      * 
           MOVE    "account"       TO  WRK-LF02-STATUS
      * 
           MOVE    LNK-SRYYMD      TO  WRK-WYMD
           PERFORM 500-HIZUKE-HENSYU-SEC
           MOVE    WRK-HENYMD      TO  WRK-LF02-ORDER-TIME(1:10)
           MOVE    "T"             TO  WRK-LF02-ORDER-TIME(11:1)
           MOVE    SYS-TIME(1:2)   TO  WRK-LF02-ORDER-TIME(12:2)
           MOVE    ":"             TO  WRK-LF02-ORDER-TIME(14:1)
           MOVE    SYS-TIME(3:2)   TO  WRK-LF02-ORDER-TIME(15:2)
           MOVE    ":"             TO  WRK-LF02-ORDER-TIME(17:1)
           MOVE    SYS-TIME(5:2)   TO  WRK-LF02-ORDER-TIME(18:2)
      *
      *    診療科名称
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                      SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    LNK-SRYKA           TO  SYS-1005-KBNCD
           MOVE    LNK-SYSYMD          TO  SYS-1005-STYUKYMD
           MOVE    LNK-SYSYMD          TO  SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1005-REC
               MOVE    SYS-1005-SRYKANAME2 TO  WRK-LF02-SRYKA-NAME
                                           WRK-LF02-CRE001-DPERTNAME
                                           WRK-LF02-CRE401-DPERTNAME
               MOVE    "I"                 TO  WRK-LF02-SRYKA-REPCODE
               MOVE    LNK-SRYKA           TO  WRK-LF02-SRYKA-ID
               MOVE    LNK-SRYKA           TO  WRK-LF02-CRE001-DPERTID
                                           WRK-LF02-CRE401-DPERTID
           END-IF
      *
      *    入外区分
           MOVE    "false"         TO  WRK-LF02-NYUGAIKBN
      *
      *    受診履歴の連番確定処理
           MOVE    ZERO                TO  WRK-RENNUM
      *    PERFORM 2001-RENNUM-SET-SEC
      *???
      *    DISPLAY "RENNUM=" WRK-RENNUM "##"
      *???     
      *    受診履歴なし
      *    IF      WRK-RENNUM          =   ZERO
      *        GO      TO      200-MAIN-EXT
      *    END-IF
      *
      *
           MOVE    SPACE           TO  TBL-AREA
           INITIALIZE              TBL-AREA
           MOVE    ZERO                TO  IDX-LF
           MOVE    ZERO                TO  IDX-LF2
           MOVE    ZERO                TO  IDX-SINSATU
      *
      *    受診履歴検索
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    LNK-PTID            TO  JYURRK-PTID
           MOVE    LNK-SRYKA           TO  JYURRK-SRYKA
           MOVE    LNK-SRYYMD          TO  JYURRK-SRYYMD
           MOVE    LNK-DENPNUM         TO  JYURRK-DENPNUM
           MOVE    "2"                 TO  JYURRK-NYUGAIKBN
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
      *
           PERFORM
                   UNTIL       FLG-JYURRK      =   1
             IF      LNK-HKNCOMBINUM     =   JYURRK-HKNCOMBINUM
               PERFORM VARYING     IDX-JYU     FROM    1   BY  1
                       UNTIL      (IDX-JYU         >   15  )   OR
                                  (JYURRK-ZAINUM (IDX-JYU) =   ZERO )
                   MOVE    JYURRK-ZAINUM (IDX-JYU) TO  WRK-ZAINUM
                   COMPUTE IDX-REN         =   JYURRK-RENNUM
                                           +   1
      *ver.4.8
      ****         IF      IDX-REN         >   4
      *                MOVE    4               TO  IDX-REN
      *****        END-IF
                   PERFORM 2002-SRYACT-SYORI-SEC
               END-PERFORM
             END-IF
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    収納より請求書項目編集
           PERFORM 2005-SYUNOU-SET-SEC
      *
      *    受付メモ編集
           PERFORM 2009-PTMEMOINF-SEC
      *
      *    ＴＢＬより再編集
           PERFORM 2003-TBL-FL02-SET-SEC
      *    医療機関情報−所在地、連絡先
           MOVE    SPACE               TO  SYS-1002-REC
           INITIALIZE                  SYS-1002-REC
           MOVE    "1002"              TO  SYS-1002-KANRICD
           MOVE    "*"                 TO  SYS-1002-KBNCD
           MOVE    LNK-SYSYMD          TO  SYS-1002-STYUKYMD
           MOVE    LNK-SYSYMD          TO  SYS-1002-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1002-HOSPNUM
           MOVE    SYS-1002-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1002-REC
           END-IF
      *    医療機関ＩＤ編集
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    LNK-SYSYMD          TO  SYS-1001-STYUKYMD
           MOVE    LNK-SYSYMD          TO  SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
      * 作成者 施設情報名
      *        拡張漢字変換
               MOVE    SYS-1001-HOSPNAME   TO  WRK-KOUMOKU
               PERFORM 800-ORCSKANACHK-SEC
               MOVE    WRK-KOUMOKU         TO  SYS-1001-HOSPNAME
      *
               MOVE  SYS-1001-HOSPNAME      TO  
                       WRK-LF02-CRE001-FACILITYNAME
                       WRK-LF02-CRE401-FACILITYNAME
      *管理者名
      *        拡張漢字変換
               MOVE    SYS-1001-KANRINAME   TO  WRK-KOUMOKU
               PERFORM 800-ORCSKANACHK-SEC
               MOVE    WRK-KOUMOKU          TO  SYS-1001-KANRINAME
      *
               MOVE  SYS-1001-KANRINAME     TO  
                       WRK-LF02-CRE001-FULLNAME
                       WRK-LF02-CRE401-FULLNAME  
      *医療機関コード
               MOVE  SYS-1001-HOSPCD        TO  
                       WRK-LF02-CRE001-PERSONALID
                       WRK-LF02-CRE401-PERSONALID
               MOVE  "doctor"               TO  
                       WRK-LF02-CRE001-LICENSE
                       WRK-LF02-CRE401-LICENSE
               MOVE  SYS-1001-HOSPID        TO  
                       WRK-LF02-CRE001-FACILITYID
                       WRK-LF02-CRE401-FACILITYID
      *郵便番号
               MOVE  SYS-1002-POST(1:3)     TO  
                       WRK-LF02-CRE001-ZIP(1:3)
                       WRK-LF02-CRE401-ZIP(1:3)
               MOVE  "-"                    TO  
                       WRK-LF02-CRE001-ZIP(4:1)
                       WRK-LF02-CRE401-ZIP(4:1)
               MOVE  SYS-1002-POST(4:4)     TO  
                       WRK-LF02-CRE001-ZIP(5:4)
                       WRK-LF02-CRE401-ZIP(5:4)
      *住所
               MOVE  SYS-1002-ADRS          TO  
                       WRK-LF02-CRE001-ADRFULLNAME
                       WRK-LF02-CRE401-ADRFULLNAME
               PERFORM VARYING IDX FROM    1   BY  1
      *D                       UNTIL   IDX >       20
                               UNTIL   IDX >       30
                 IF    WRK-LF02-SRYCD(IDX) NOT =   SPACE 
                       MOVE  "ORCA"      
                             TO  WRK-LF02-ADMINCODETBL(IDX)
                       MOVE  "Claim007"      
                             TO  WRK-LF02-SRYCDTBL(IDX)
                       MOVE  "Claim007"      
                             TO  WRK-LF02-SRYCDTBL(IDX)
                    PERFORM VARYING IDY FROM    1   BY  1
      *@                         UNTIL   IDY >       20
                                 UNTIL   IDY >       30
                      IF    WRK-LF02-SRYSYUBETSU(IDX IDY) 
                            NOT =   SPACE 
                            MOVE  "Claim003"      
                            TO  WRK-LF02-SRYSYUBETSUTBL(IDX IDY)
                            MOVE  "Claim004"      
                            TO  WRK-LF02-SURYOCDTBL(IDX IDY)
                      END-IF 
                   END-PERFORM
                  END-IF
               END-PERFORM
           END-IF     
      *    ドクター
           IF      LNK-DRCD            =   "@@@@@"
               CONTINUE
           ELSE
               PERFORM 2001-DRCD-NAME-SEC
               MOVE    SYS-1010-NAME   TO  WRK-LF02-CRE401-FULLNAME
               MOVE    LNK-DRCD        TO  WRK-LF02-CRE401-PERSONALID
           END-IF
      *    処理内容
           MOVE    ALL "?"             TO  WRK-LF02-CRE001-PHONEAREA
           MOVE    ALL "?"             TO  WRK-LF02-CRE001-PHONECITY
           MOVE    ALL "?"             TO  WRK-LF02-CRE001-PHONENUM
           IF      LNK-SYORIFLG        =   "1"
               MOVE    "Mod"           TO  WRK-LF02-CRE001-PHONEMEMO
           ELSE
               MOVE    "New"           TO  WRK-LF02-CRE001-PHONEMEMO
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    ドクター設定処理
      *****************************************************************
       2001-DRCD-NAME-SEC              SECTION.
      *
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                      SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    LNK-DRCD            TO  SYS-1010-KBNCD
           MOVE    LNK-SYSYMD          TO  SYS-1010-STYUKYMD
           MOVE    LNK-SYSYMD          TO  SYS-1010-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
      *
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1010-REC
           ELSE
               INITIALIZE                      SYS-1010-REC
           END-IF
      *    拡張漢字変換
           MOVE    SYS-1010-NAME       TO  WRK-KOUMOKU
           PERFORM 800-ORCSKANACHK-SEC
           MOVE    WRK-KOUMOKU         TO  SYS-1010-NAME
      *
           .
       2001-DRCD-NAME-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ処理
      *****************************************************************
       2009-HKNCOMBI-HEN-SEC                SECTION.
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    LNK-PTID            TO  COMB-PTID
           MOVE    LNK-HKNCOMBINUM     TO  COMB-HKNCOMBINUM
           PERFORM 910-HKNCOMBI-READ-SEC
      *    主保険あり
           IF      COMB-HKNNUM     NOT =   SPACE
               PERFORM VARYING IDW     FROM    1   BY  1
                       UNTIL   IDW     >       MML0031-TMAX
                   IF    COMB-HKNNUM       =   MML0031-HOKEN (IDW)
                       MOVE    MML0031-NAME(IDW) 
                                           TO  WRK-LF02-HKNSBT
                       MOVE    MML0031-TMAX    TO  IDW
                   END-IF
               END-PERFORM
               PERFORM 20091-HKNNUM-SET-SEC
           ELSE
      *         公費単独
                MOVE    "公費単独"         TO  WRK-LF02-HKNSBT
                MOVE    "XX"               TO  WRK-LF02-HKNSBTCODE
           END-IF
      *
           MOVE    "MML0031"           TO  WRK-LF02-HKNSBTTBL
           .
       2009-HKNCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険種別編集処理
      *****************************************************************
       20091-HKNNUM-SET-SEC              SECTION.
      *
           EVALUATE    COMB-HKNNUM 
               WHEN    "060"
      *            国保の場合は、ゼロを設定
                   MOVE    "00"        TO  WRK-LF02-HKNSBTCODE
               WHEN    "971"
      *            労災
                   MOVE    "R1"        TO  WRK-LF02-HKNSBTCODE
                   MOVE    "労災"      TO  WRK-LF02-HKNSBT
               WHEN    "973"
      *            自賠責
                   MOVE    "R3"        TO  WRK-LF02-HKNSBTCODE
                   MOVE    "自賠責"    TO  WRK-LF02-HKNSBT
               WHEN    "975"
      *            公害
                   MOVE    "K5"        TO  WRK-LF02-HKNSBTCODE
                   MOVE    "公害"      TO  WRK-LF02-HKNSBT
               WHEN    OTHER
                   EVALUATE    COMB-HKNNUM(1:2)
                   WHEN    "98"
      *                自費
                       MOVE    "Z"         TO  WRK-LF02-HKNSBTCODE(1:1)
                       MOVE    COMB-HKNNUM(3:1)
                                           TO  WRK-LF02-HKNSBTCODE(2:1)
                       MOVE    "自費"
                                           TO  WRK-LF02-HKNSBT
                   WHEN    "90"
      *                治験
                       MOVE    "A"         TO  WRK-LF02-HKNSBTCODE(1:1)
                       MOVE    COMB-HKNNUM(3:1)
                                           TO  WRK-LF02-HKNSBTCODE(2:1)
                       MOVE    "治験"
                                           TO  WRK-LF02-HKNSBT
                   WHEN    "91"
      *                治験
                       MOVE    "B"         TO  WRK-LF02-HKNSBTCODE(1:1)
                       MOVE    COMB-HKNNUM(3:1)
                                           TO  WRK-LF02-HKNSBTCODE(2:1)
                       MOVE    "治験"
                                           TO  WRK-LF02-HKNSBT
                   WHEN    OTHER
      *                その他（下２桁）
                       MOVE    COMB-HKNNUM     TO  WRK-999
                       MOVE    WRK-999         TO  WRK-ZZZ
                       MOVE    WRK-ZZZ(2:2)    TO  WRK-LF02-HKNSBTCODE
                   END-EVALUATE
           END-EVALUATE
      *
           .
       20091-HKNNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴の連番確定処理
      *****************************************************************
       2001-RENNUM-SET-SEC                SECTION.
      *
      *
      *    受診履歴検索
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    LNK-PTID            TO  JYURRK-PTID
           MOVE    LNK-SRYKA           TO  JYURRK-SRYKA
           MOVE    LNK-SRYYMD          TO  JYURRK-SRYYMD
           MOVE    "2"                 TO  JYURRK-NYUGAIKBN
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           PERFORM
                   UNTIL       FLG-JYURRK      =   1
               IF      LNK-HKNCOMBINUM     =   JYURRK-HKNCOMBINUM
                   MOVE    JYURRK-RENNUM       TO  WRK-RENNUM
               END-IF
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       2001-RENNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *        診療行為より編集処理
      *****************************************************************
       2002-SRYACT-SYORI-SEC      SECTION.
      *
      *???  
           DISPLAY "ZAINUM=" WRK-ZAINUM "##"
      *???     
           MOVE    ZERO                TO  WRK-FILMCNT
      *              
           INITIALIZE                  SRYACT-REC
           MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    LNK-NYUGAIKBN       TO  SRY-NYUGAIKBN
           MOVE    LNK-PTID            TO  SRY-PTID
           MOVE    LNK-SRYKA           TO  SRY-SRYKA
           MOVE    LNK-SRYYMD(1:6)     TO  SRY-SRYYM
           MOVE    WRK-ZAINUM          TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-READ-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
      *???
           DISPLAY "SRYACT=" FLG-SRYACT "##" SRY-SRYKBN "##"
      *???     
           MOVE    SPACE               TO  WRK-SRYSYUKBN
           MOVE    SPACE               TO  WRK-SRYKBN
           MOVE    SPACE               TO  WRK-SRYKBN3
           MOVE    SPACE               TO  WRK-SRYCD
           MOVE    ZERO                TO  WRK-KAISU
           MOVE    ZERO                TO  IDX-S
           MOVE    ZERO                TO  FLG-KAISU
      *
           MOVE    ZERO                TO  IDX-IJ
           MOVE    ZERO                TO  IDX-BUI
           MOVE    ZERO                TO  IDX-SATU
           PERFORM
                   UNTIL   FLG-SRYACT      =   1
      *H29.12  剤オーバー
                     OR   (FLG-ERROVER     NOT =   ZERO )
               MOVE    SRY-SRYSYUKBN       TO  WRK-SRYSYUKBN
               MOVE    SRY-SRYKBN          TO  WRK-SRYKBN
               MOVE    SRY-SRYSYUKBN       TO  WRK-SRYKBN3
      *        自費の時処理
               IF      SRY-SRYKBN          =   "95"  OR  "96"
                   PERFORM 20020-JIHI-SYORI-SEC
               ELSE
                   IF      SRY-RENNUM          =   1
      *                診療会計より、点数チェック
                       PERFORM 20021-SRYACCT-SYORI-SEC
                   END-IF
                   PERFORM VARYING     IDXA    FROM    1   BY  1
                           UNTIL       IDXA    >   5
      *H29.12  剤オーバー
                     OR   (FLG-ERROVER     NOT =   ZERO )
                       IF      SRY-SRYCD (IDXA)    NOT =   SPACE
                           PERFORM 20022-SRYACT-HANTEI-SEC
                       END-IF
                   END-PERFORM
               END-IF
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       2002-SRYACT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計より回数、診療行為区分コード、合計点数編集処理
      *****************************************************************
       20021-SRYACCT-SYORI-SEC         SECTION.
      *
      *    継続管理加算を再診料に追加する
           IF      SRY-SRYCD (01)      =   "112007170"  OR
                                           "112702270"
               IF      IDX-SINSATU         =   ZERO
                   DISPLAY "??? 再診療料なし？？？" 
               ELSE
                   MOVE    IDX-SINSATU         TO  IDX-LF
                   MOVE    ZERO                TO  IDX-IJ
                   PERFORM VARYING     IDXS    FROM    1   BY  1
      *@                   UNTIL       IDXS    >   20
                           UNTIL       IDXS    >   30
                       IF      TBL-IJICD (IDX-LF IDXS)
                                                NOT =   SPACE
                           MOVE    IDXS            TO  IDX-IJ
                       END-IF
                   END-PERFORM
                   GO      TO      20021-SRYACCT-SYORI-EXT
               END-IF
           END-IF
      *
      *    診療会計マスター読み込み
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    LNK-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    LNK-PTID            TO  ACCT-PTID
           MOVE    LNK-SRYKA           TO  ACCT-SRYKA
           MOVE    SRY-SRYYM           TO  ACCT-SRYYM
           MOVE    SRY-SRYKBN          TO  ACCT-SRYKBN
           MOVE    SRY-ZAINUM          TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           ADD     1                   TO  IDX-LF2
      *@   ２０剤以上の時エラー
      *@   IF      IDX-LF2             >   20
      *    ３０剤以上の時エラー
           IF      IDX-LF2             >   30
               DISPLAY "ZAI 30 OVER ???"
      *!!!     MOVE    30              TO  IDX-LF2
               MOVE    1               TO  FLG-ERROVER
               GO      TO      20021-SRYACCT-SYORI-EXT
           END-IF
      *
           MOVE    IDX-LF2             TO  IDX-LF
      *
      *    回数
           MOVE    LNK-SRYYMD(7:2)     TO  IDX-ACT
           MOVE    ACCT-DAY (IDX-REN IDX-ACT)
                                       TO  TBL-KAISU (IDX-LF)
      *
      *    診療行為区分コード
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       58
               IF      WRK-SRYKBN      =   CLAIM002-ORCA  (IDX)
                   MOVE    CLAIM002-CLAIM(IDX) 
                                       TO  TBL-SRYCD    (IDX-LF)
                   MOVE    58          TO  IDX
               END-IF
           END-PERFORM
           IF      TBL-SRYCD (IDX-LF)  =   SPACE
               MOVE    WRK-SRYKBN          TO  TBL-SRYCD (IDX-LF)(1:2)
               MOVE    ZERO                TO  TBL-SRYCD (IDX-LF)(3:1)
           END-IF
           IF      TBL-SRYCD (IDX-LF)  =   "340"
               MOVE    "331"               TO  TBL-SRYCD (IDX-LF)
           END-IF
      *
      *    ＣＬＡＩＭ用診療行為区分コード
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       CLAIM007-MAX
               IF      TBL-SRYCD (IDX-LF)  =   CLAIM007-CLAIM  (IDX)
                   MOVE    CLAIM007-CLAIM(IDX) 
                                           TO  TBL-SRYCD  (IDX-LF)
                   MOVE    CLAIM007-NAME (IDX)
                                           TO  TBL-SRYNAME(IDX-LF)
                   MOVE    CLAIM007-MAX    TO  IDX
               END-IF
           END-PERFORM
      *    付加情報検索
           MOVE    ZERO                TO  FLG-GENTEN
           IF      ACCT-ZAIREQFLG      =   "1"
               PERFORM 950-SRYACCTPLUS-READ-SEC
               IF      ACCTP-PLUSKBN       =   1
                   MOVE    1               TO  FLG-GENTEN
               END-IF
           END-IF
      *
      *    請求点数
           IF      FLG-GENTEN          =   ZERO
               MOVE    ACCT-ZAITEN         TO  WRK-ACCT-ZAITEN
               MOVE    ACCT-SYUTEN1        TO  WRK-ACCT-SYUTEN1
               MOVE    ACCT-KIZAITEN       TO  WRK-ACCT-KIZAITEN
               MOVE    ACCT-YKZTEN         TO  WRK-ACCT-YKZTEN
           ELSE
               COMPUTE WRK-ACCT-ZAITEN     =   ACCT-ZAITEN   *   -1
               COMPUTE WRK-ACCT-SYUTEN1    =   ACCT-SYUTEN1  *   -1
               COMPUTE WRK-ACCT-KIZAITEN   =   ACCT-KIZAITEN *   -1
               COMPUTE WRK-ACCT-YKZTEN     =   ACCT-YKZTEN   *   -1
           END-IF
           MOVE    WRK-ACCT-ZAITEN         TO  TBL-TENNSUALL (IDX-LF)
           MOVE    WRK-ACCT-SYUTEN1        TO  TBL-SYUGI (IDX-LF)
           MOVE    WRK-ACCT-KIZAITEN       TO  TBL-ZAIRYOU (IDX-LF)
           MOVE    WRK-ACCT-YKZTEN         TO  TBL-YAKUZAI (IDX-LF)
      *    請求金額
           COMPUTE WRK-ACCT-ZAITEN         =   WRK-ACCT-ZAITEN * 10
           MOVE    WRK-ACCT-ZAITEN         TO  TBL-GINGAKUALL(IDX-LF)
      *
      *
      *    診察の時、添え字保存
           EVALUATE    SRY-SRYKBN
               WHEN    "11"
               WHEN    "12"
                   IF      IDX-SINSATU     =   ZERO
                        MOVE    IDX-LF         TO  IDX-SINSATU
                   END-IF
           END-EVALUATE
           .
       20021-SRYACCT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為チェック編集処理
      *****************************************************************
       20022-SRYACT-HANTEI-SEC         SECTION.
      *
      *    点数検索
           MOVE    SRY-SRYCD (IDXA)    TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
      *
           EVALUATE    SRY-SRYKBN
      *        診察
               WHEN    "11"
               WHEN    "12"
               WHEN    "13"
               WHEN    "14"
                   PERFORM 20024-SRYKBN-10-SEC
      *        投薬
               WHEN    "21"
               WHEN    "22"
               WHEN    "23"
      *            用法コメント
                   IF      SRY-SRYCD (IDXA)(1:3)   =   "001"
                       PERFORM 20025-YOUHOU-HEN-SEC
                   ELSE
      *                （減）を削除
                       IF      SRY-SRYCD (IDXA)    =   "820000047"
      *H26.10            (精減）
                                                   OR  "820000166"
      *H28.4             湿布薬（減）
                                                   OR  CONS-SPCM-SRYCD
                           CONTINUE
                       ELSE
                           ADD     1               TO  IDX-IJ
                           PERFORM 20022-SRYACT-SYORI-SEC
                       END-IF
                   END-IF
      *        処方料
               WHEN    "24"
               WHEN    "25"
               WHEN    "26"
               WHEN    "27"
                   ADD     1               TO  IDX-IJ
                   PERFORM 20022-SRYACT-SYORI-SEC
      *        注射
               WHEN    "31"
               WHEN    "32"
               WHEN    "33"
               WHEN    "34"
      *            残量廃棄コメント
                   IF      SRY-SRYCD (IDXA)(1:2)   =   "09"
                       CONTINUE
                   ELSE
                       ADD     1               TO  IDX-IJ
                       PERFORM 20022-SRYACT-SYORI-SEC
                   END-IF
      *        処置
               WHEN    "40"
                   ADD     1               TO  IDX-IJ
                   PERFORM 20022-SRYACT-SYORI-SEC
      *        手術
               WHEN    "50"
                   ADD     1               TO  IDX-IJ
                   PERFORM 20022-SRYACT-SYORI-SEC
      *        麻酔
               WHEN    "54"
                   ADD     1               TO  IDX-IJ
                   PERFORM 20022-SRYACT-SYORI-SEC
      *        検査
               WHEN    "60"
      *        病理診断
               WHEN    "64"
                   ADD     1               TO  IDX-IJ
                   PERFORM 20022-SRYACT-SYORI-SEC
      *        画像診断
               WHEN    "70"
      *@@@@@@@@@@  PERFORM 20024-SRYKBN-70-SEC
      *            部位・フィルムをコメント・器材とする
                   ADD     1               TO  IDX-IJ
                   PERFORM 20022-SRYACT-SYORI-SEC
      *        検査
               WHEN    "80"
                   ADD     1               TO  IDX-IJ
                   PERFORM 20022-SRYACT-SYORI-SEC
      ******   入院
      *        WHEN    "90"
      *            ADD     1               TO  IDX-IJ
      *****        PERFORM 20022-SRYACT-SYORI-SEC
      *        コメント
               WHEN    "98"
               WHEN    "99"
                   ADD     1               TO  IDX-IJ
                   PERFORM 20022-SRYACT-SYORI-SEC
           END-EVALUATE
      *
      *
           .
       20022-SRYACT-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診察チェック編集処理
      *****************************************************************
       20025-YOUHOU-HEN-SEC         SECTION.
      *
           IF      LNK-KBN             =   "1"
               MOVE    SPACE               TO  WRK-YOUHOU
               PERFORM VARYING     IDX-Y   FROM    1   BY  1
                       UNTIL       IDX-Y   >   55
                   IF      CLAIM006-ORCA (IDX-Y)  =    SRY-SRYCD(IDXA)
                       MOVE    CLAIM006-CLAIM (IDX-Y)  TO  WRK-YOUHOU
                       MOVE    55                 TO  IDX-Y
                   END-IF
               END-PERFORM 
               IF      WRK-YOUHOU          =   SPACE
                   MOVE    "G001"              TO  TBL-ADMINCODE
                                                            (IDX-LF)
                   MOVE    TNS-NAME            TO  TBL-ADMINISTRATION
                                                            (IDX-LF)
               ELSE
                   MOVE    WRK-YOUHOU          TO  TBL-ADMINCODE
                                                            (IDX-LF)
                   MOVE    TNS-NAME            TO  TBL-ADMINISTRATION
                                                            (IDX-LF)
               END-IF
           ELSE
               MOVE    SRY-SRYCD(IDXA)     TO  TBL-ADMINCODE
                                                            (IDX-LF)
               MOVE    TNS-NAME            TO  TBL-ADMINISTRATION
                                                            (IDX-LF)
           END-IF
      *H29.5
      *    フリーコメントの内容セット
           IF      SRY-INPUTNUM (IDXA)  NOT =   ZERO
               PERFORM 200251-PTCOM-SET02-SEC
           END-IF
      *
           .
       20025-YOUHOU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診察チェック編集処理
      *****************************************************************
       20024-SRYKBN-10-SEC         SECTION.
      *
           MOVE    SPACE              TO  WRK-SRYKBN3
      *    再診の時間外加算は別剤とする
           IF      (SRY-SRYKBN         =   "12"  )  AND
                   (SRY-SRYCD (IDXA)(1:1)   =   "1"  )  AND
                   (TNS-DATAKBN        =   "2"  )  AND
                   (TNS-TIMEKSNKBN     >=  1    AND  <=  7  )
               EVALUATE    TNS-TIMEKSNKBN
      *            時間外加算処理
                   WHEN    1
      *            時間外特例加算処理
                   WHEN    5
                       MOVE    "123"           TO  WRK-SRYKBN3
      *            休日加算処理
                   WHEN    2
      *            休日加算処理（初診）
                   WHEN    3
                       MOVE    "124"           TO  WRK-SRYKBN3
      *            深夜加算処理
                   WHEN    4
                       MOVE    "125"           TO  WRK-SRYKBN3
      *            夜間・早朝加算
                   WHEN    6
      *            夜間加算
                   WHEN    7
                       MOVE    "126"           TO  WRK-SRYKBN3
               END-EVALUATE
               ADD     1               TO  IDX-LF
               ADD     1               TO  IDX-LF2
               MOVE    1               TO  IDX-IJ
               PERFORM 20022-SRYACT-SYORI-SEC
      *        明細見出しセット
               MOVE    WRK-SRYKBN3     TO  TBL-SRYCD     (IDX-LF)
               MOVE    TNS-TEN         TO  TBL-TENNSUALL (IDX-LF)
               MOVE    TNS-TEN         TO  TBL-SYUGI     (IDX-LF)
               COMPUTE TBL-GINGAKUALL (IDX-LF)
                                       =   TNS-TEN     *   10
               MOVE    1               TO  TBL-KAISU     (IDX-LF)
      *        請求金額減算
               COMPUTE TBL-TENNSUALL (IDX-SINSATU)
                                       =   TBL-TENNSUALL (IDX-SINSATU)
                                       -   TBL-TENNSUALL (IDX-LF)
               COMPUTE TBL-GINGAKUALL (IDX-SINSATU)
                                       =   TBL-GINGAKUALL (IDX-SINSATU)
                                       -   TBL-GINGAKUALL (IDX-LF)
               COMPUTE TBL-SYUGI (IDX-SINSATU)
                                       =   TBL-SYUGI (IDX-SINSATU)
                                       -   TBL-SYUGI (IDX-LF)
           ELSE
      *        外来管理加算の診療区分を変更
               IF     (SRY-SRYCD (IDXA)    =   "112005610"   OR
                                               "112701410"   OR
                                               "112701510"  )
                   MOVE    "122"           TO  TBL-SRYCD    (IDX-LF)
                   MOVE    "122"           TO  WRK-SRYKBN3
               END-IF
               ADD     1                   TO  IDX-IJ
               PERFORM 20022-SRYACT-SYORI-SEC
      *       継続管理加算の時請求金額加算
               IF      SRY-SRYCD (IDXA)    =   "112007170"  OR
                                               "112702270"
                   COMPUTE TBL-TENNSUALL (IDX-SINSATU)
                                       =   TBL-TENNSUALL (IDX-SINSATU)
                                       +   TNS-TEN
                   COMPUTE TBL-GINGAKUALL (IDX-SINSATU)
                                       =   TBL-GINGAKUALL (IDX-SINSATU)
                                       +  (TNS-TEN   *   10 )
               END-IF
           END-IF
      *
      *    診療行為区分コード
           IF      WRK-SRYKBN3(3:1)    NOT =   ZERO
             PERFORM VARYING IDX FROM    1   BY  1
                     UNTIL   IDX >       CLAIM007-MAX
                 IF      WRK-SRYKBN3     =   CLAIM007-CLAIM  (IDX)
                     MOVE    CLAIM007-CLAIM(IDX) 
                                         TO  TBL-SRYCD  (IDX-LF)
                     MOVE    CLAIM007-NAME (IDX)
                                         TO  TBL-SRYNAME(IDX-LF)
                     MOVE    CLAIM007-MAX    TO  IDX
                 END-IF
             END-PERFORM
           END-IF
      *          IF TNS-CHUKSNTUBAN   =   "0"
      *---->    １２０番の子どもに格納する
      *          ELSE
      *             EVALUATE   TNS-CHUKSNTUBAN
      *    年齢関係の注加算通番
      *             WHEN    "1"
      *    栄養指導関係の注加算通番
      *             WHEN    "2"
      *---->    １２０番の子どもに格納する
      *
           .
       20024-SRYKBN-10-EXT.
           EXIT.
      *
      *****************************************************************
      *    画像診断チェック編集処理
      *****************************************************************
       20024-SRYKBN-70-SEC         SECTION.
      *
      *    包括逓減算定用は処理なし
           IF      SRY-SRYCD (IDXA)    =   "099700101" OR
                                           "099700102" OR
                                           "099700103"
               GO      TO      20024-SRYKBN-70-EXT
           END-IF
      *
           IF      SRY-SRYCD (IDXA)(1:3)   =   "002"
      *        部位
               ADD     1               TO  IDX-IJ
               PERFORM 20022-SRYACT-SYORI-SEC
               MOVE    IDX-IJ          TO  IDX-BUI
           ELSE
      *
               EVALUATE    SRY-SRYCD (IDXA)(1:1)
                   WHEN    "1"
      *            撮影料の位置を保存
                       EVALUATE    TNS-SRYSYUKBN(1:2)
                           WHEN    "72"
                               IF      IDX-BUI     NOT =   ZERO
                                   MOVE    IDX-IJ          TO  HZN-IJ
                                   MOVE    IDX-BUI         TO  IDX-IJ
                                   PERFORM 20022-SRYACT-SYORI-SEC
                                   MOVE    IDX-IJ          TO  IDX-SATU
                                   MOVE    HZN-IJ          TO  IDX-IJ
                                   MOVE    ZERO            TO  IDX-BUI
                               ELSE
                                   ADD     1               TO  IDX-IJ
                                   PERFORM 20022-SRYACT-SYORI-SEC
                                   MOVE    IDX-IJ          TO  IDX-SATU
                               END-IF
                               MOVE    ZERO            TO  WRK-FILMCNT
                           WHEN    OTHER
                               ADD     1               TO  IDX-IJ
                               PERFORM 20022-SRYACT-SYORI-SEC
                               IF      IDX-SATU        =   ZERO
                                   MOVE    IDX-IJ      TO  IDX-SATU
                               END-IF
                       END-EVALUATE
      *            フィルム
                   WHEN    "7"
                       MOVE    IDX-IJ          TO  HZN-IJ
                       MOVE    IDX-SATU        TO  IDX-IJ
                       PERFORM 20022-SRYACT-SYORI-SEC
                       MOVE    HZN-IJ          TO  IDX-IJ
      *            その他
                   WHEN    OTHER
                       ADD     1               TO  IDX-IJ
                       PERFORM 20022-SRYACT-SYORI-SEC
               END-EVALUATE
           END-IF
      *
           .
       20024-SRYKBN-70-EXT.
           EXIT.
      *      
      *****************************************************************
      *    処方内容編集処理
      *****************************************************************
       20022-SRYACT-SYORI-SEC         SECTION.
      *
      *    １剤で３０行以上の時
      *@   IF      IDX-IJ              >   20
           IF      IDX-IJ              >   30
               DISPLAY "*** ORCL0021 INDEX OVER ****"
               GO      TO  20022-SRYACT-SYORI-EXT
           END-IF
      *
      *     フィルム
      *@@  IF      (SRY-SRYCD (IDXA)(1:1)   =  "7"    )  AND
      *            (SRY-SRYKBN              =   "70"  )
      *        ADD     1                   TO  WRK-FILMCNT
      *        IF      WRK-FILMCNT         <=  WRK-FILMMAX
      *            MOVE    SRY-SRYSURYO(IDXA)  TO  TBL-FILMNUM
      *                                     (IDX-LF  IDX-IJ WRK-FILMCNT)
      *            フィルム分割
      *            MOVE    SRY-SRYKAISU(IDXA)  TO  TBL-FILMDEV
      *                                     (IDX-LF  IDX-IJ WRK-FILMCNT)
      *            PERFORM VARYING IDZ FROM    1   BY  1
      *                    UNTIL   IDZ >       7
      *                IF      SRY-SRYCD(IDXA) =
      *                                        CLAIM005-ORCA (IDZ)
      *                    MOVE    CLAIM005-CLAIM (IDZ)
      *                                        TO  TBL-FILMSIZECD
      *                                     (IDX-LF  IDX-IJ WRK-FILMCNT)
      *                    MOVE    CLAIM005-NAME  (IDZ)
      *                                        TO  TBL-FILMSIZE
      *                                     (IDX-LF  IDX-IJ WRK-FILMCNT)
      *                    MOVE    7                      TO  IDZ
      *                 END-IF
      *            END-PERFORM
      *        END-IF
      *        GO      TO      20022-SRYACT-SYORI-EXT
      *@@  END-IF
      *
           EVALUATE    SRY-SRYCD (IDXA)(1:1)
      *        薬剤／器剤
               WHEN    "6"
               WHEN    "7"
      *             薬剤料逓減の時、点数を−とする
                   IF     (SRY-SRYCD (IDXA)    =   "630010002")
      *H26.10          減点
                      OR  (TNS-TENSIKIBETU     =   7          )
      *                診療コード
                       MOVE    SRY-SRYCD(IDXA)     TO  TBL-IJICD
                                                       (IDX-LF IDX-IJ)
                       MOVE    TNS-NAME            TO  TBL-ALSNAME
                                                       (IDX-LF IDX-IJ)
                       MOVE    SRY-SRYSURYO(IDXA)  TO  TBL-SURYO
                                                       (IDX-LF IDX-IJ)
                       MOVE    TNS-TANINAME        TO  TBL-TANI
                                                       (IDX-LF IDX-IJ)
      *                診療種別区分コード
                       MOVE    "2"                 TO  TBL-SRYSYUBETSU
                                                       (IDX-LF IDX-IJ)
      *
                        MOVE    ZERO        TO  TBL-SURYO
                                                       (IDX-LF IDX-IJ)
                        MOVE    SPACE       TO  TBL-SURYOCD
                                                       (IDX-LF IDX-IJ)
                   ELSE
      *                診療コード
                       MOVE    SRY-SRYCD(IDXA)     TO  TBL-IJICD
                                                       (IDX-LF IDX-IJ)
                       MOVE    TNS-NAME            TO  TBL-ALSNAME
                                                       (IDX-LF IDX-IJ)
                       MOVE    SRY-SRYSURYO(IDXA)  TO  TBL-SURYO
                                                       (IDX-LF IDX-IJ)
                       MOVE    TNS-TANINAME        TO  TBL-TANI
                                                       (IDX-LF IDX-IJ)
      *                診療種別区分コード
                       IF      SRY-SRYCD (IDXA)(1:1)   =   "6"
                           MOVE    "2"             TO  TBL-SRYSYUBETSU
                                                       (IDX-LF IDX-IJ)
                       ELSE
                           MOVE    "1"             TO  TBL-SRYSYUBETSU
                                                       (IDX-LF IDX-IJ)
                       END-IF
      *                数量コード
                       IF      SRY-SRYCD (IDXA)(1:1)   =   "6"
                           MOVE    "10"            TO  TBL-SURYOCD
                                                       (IDX-LF IDX-IJ)
                       ELSE
      *                    酸素等区分
                           IF      TNS-SANSOKBN        =   1
                               MOVE    "30"            TO  TBL-SURYOCD
                                                       (IDX-LF IDX-IJ)
                           ELSE
                               MOVE    "21"            TO  TBL-SURYOCD
                                                       (IDX-LF IDX-IJ)
                           END-IF
                       END-IF
      *                明細単価
                       MOVE    TNS-TEN             TO  TBL-MEISAITANKA
                                                       (IDX-LF IDX-IJ)
      *                請求点数
                       IF      SRY-SRYCD (IDXA)(1:1)   =   "6"
      *                    （薬剤）
                           COMPUTE WRK-TEN     =   TNS-TEN
                                               *   SRY-SRYSURYO(IDXA)
                           COMPUTE WRK-TEN     =   WRK-TEN
                                               /   10
                                               +   0.5
                           MOVE    WRK-TEN         TO  TBL-SEIKYUTEN
                                                       (IDX-LF IDX-IJ)
                       ELSE
      *                    （器材）
                           COMPUTE WRK-TEN     =   TNS-TEN
                                               *   SRY-SRYSURYO(IDXA)
                           COMPUTE WRK-TEN     =   WRK-TEN
                                               /   10
                                               +   0.5
                           MOVE    WRK-TEN         TO  TBL-SEIKYUTEN
                                                       (IDX-LF IDX-IJ)
                       END-IF
      *
      *                %加算の時点数なし
                       IF      TNS-TENSIKIBETU     =   5  OR  6
                           MOVE    ZERO            TO  TBL-MEISAITANKA
                                                       (IDX-LF IDX-IJ)
                           MOVE    ZERO            TO  TBL-SEIKYUTEN
                                                       (IDX-LF IDX-IJ)
                       END-IF
                   END-IF
      *
               WHEN    "1"
      *            診療行為
                   MOVE    "0"                 TO  TBL-SRYSYUBETSU
                                                      (IDX-LF IDX-IJ)
      *            MOVE    "S"                 TO  TBL-IJICD
      *                                            (IDX-LF IDX-IJ)(1:1) 
                   MOVE    SRY-SRYCD(IDXA)     TO  TBL-IJICD
                                                   (IDX-LF IDX-IJ)
                   MOVE    TNS-NAME            TO  TBL-ALSNAME
                                                   (IDX-LF IDX-IJ)
                   MOVE    SRY-SRYSURYO(IDXA)  TO  TBL-SURYO
                                                       (IDX-LF IDX-IJ)
      *            数量コード（あてはまるコードがないので、材料個数）
                   IF      SRY-SRYSURYO(IDXA)  >   ZERO
                       MOVE    "22"            TO  TBL-SURYOCD
                                                       (IDX-LF IDX-IJ)
                   END-IF
      *
      *            %加算の時点数なし
                   IF      TNS-TENSIKIBETU     =   5  OR  6
      *                減点
                                               OR  7
                       CONTINUE
                   ELSE
      *                請求点数
                       MOVE    TNS-TEN             TO  TBL-SEIKYUTEN
                                                   (IDX-LF IDX-IJ)
      *                数量のある時、きざみ値の計算
                       IF     (SRY-SRYSURYO(IDXA)  >   1  )  AND
                              (TNS-KZMCOMPSIKIBETU =   1       )
                           PERFORM 200222-KIZAMICHI-SEC
                       END-IF
      *                明細単価
                       COMPUTE WRK-TEN             =   TNS-TEN
                                                   *   10
                       MOVE    WRK-TEN             TO  TBL-MEISAITANKA
                                                   (IDX-LF IDX-IJ)
                   END-IF
      *            包括区分
                   IF      SRY-SRYKBN      =   "60"
                       IF      TNS-HOUKNSKBN   =   ZERO
                           MOVE    "false"     TO  TBL-PPSFLG
                                                          (IDX-LF)
                       ELSE
                           MOVE    "true"      TO  TBL-PPSFLG
                                                          (IDX-LF)
                       END-IF
                   END-IF
      *        コメント
               WHEN    "8"
      *            診療行為
                   MOVE    "0"                 TO  TBL-SRYSYUBETSU
                                                      (IDX-LF IDX-IJ)
      *            MOVE    "S"                 TO  TBL-IJICD
      *                                            (IDX-LF IDX-IJ)(1:1) 
                   MOVE    SRY-SRYCD(IDXA)     TO  TBL-IJICD
                                                   (IDX-LF IDX-IJ)
                   MOVE    TNS-NAME            TO  TBL-ALSNAME
                                                   (IDX-LF IDX-IJ)
                   MOVE    TNS-NAME            TO  TBL-SRYMEMO
                                                       (IDX-LF IDX-IJ)
                   MOVE    SRY-SRYSURYO(IDXA)  TO  TBL-SURYO
                                                       (IDX-LF IDX-IJ)
      *
      *            メモにフリーコメントの内容セット
                   IF      SRY-INPUTNUM (IDXA)  NOT =   ZERO
                       PERFORM 200221-PTCOM-SET-SEC
                   END-IF
      *        その他
               WHEN    "0"
      ***          部位
      *            IF      SRY-SRYCD(IDXA)(1:3)    =   "002"
      *                MOVE    TNS-NAME            TO  TBL-LOCATION
      *                                            (IDX-LF IDX-IJ)
      ****         END-IF
      *            コメント
      ************ IF      SRY-SRYCD(IDXA)(1:3)    =   "008"
                   IF      SRY-SRYCD(IDXA)(1:3)    =   "099"
                       CONTINUE
                   ELSE
      *                システム予約コード以外は対象とする
                       MOVE    "0"                 TO  TBL-SRYSYUBETSU
                                                      (IDX-LF IDX-IJ)
                       MOVE    SRY-SRYCD(IDXA)     TO  TBL-IJICD
                                                   (IDX-LF IDX-IJ)
                       MOVE    TNS-NAME            TO  TBL-ALSNAME
                                                   (IDX-LF IDX-IJ)
                       MOVE    TNS-NAME            TO  TBL-SRYMEMO 
                                                       (IDX-LF IDX-IJ)
                       MOVE    SRY-SRYSURYO(IDXA)  TO  TBL-SURYO
                                                       (IDX-LF IDX-IJ)
      *                メモにフリーコメントの内容セット
                       IF      SRY-INPUTNUM (IDXA)  NOT =   ZERO
                           PERFORM 200221-PTCOM-SET-SEC
                       END-IF
                   END-IF
           END-EVALUATE
      *
      *
           .
       20022-SRYACT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    きざみ値の計算
      *****************************************************************
       200222-KIZAMICHI-SEC              SECTION.
      *
      *    きざみ値計算
           MOVE    SRY-SRYSURYO(IDXA)  TO  WRK-SURYO9
           MOVE    ZERO                TO  WRK-KEI-TEN2
      *    きざみ値判定
           EVALUATE    TRUE
      *            データ≦（下限値−きざみ値）
               WHEN    WRK-SURYO9      <=  (TNS-KZMKGN  -  TNS-KZM)
                   CONTINUE
      *
      *            下限値＜データ≦上限値
               WHEN     (TNS-KZMKGN        <   WRK-SURYO9)   AND
                        (TNS-KZMJGN        >=  WRK-SURYO9)
      *            点数計算式１
                   COMPUTE WRK-KEI-TEN1    =  (WRK-SURYO9 - TNS-KZMKGN)
                                           /   TNS-KZM
                                           +   0.999
                   MOVE    WRK-KEI-TEN1    TO  WRK-KEI-TEN2
                   COMPUTE WRK-KEI-TEN     =   TNS-TEN
                                           + ( WRK-KEI-TEN2
                                           *   TNS-KZMTEN )
                   COMPUTE WRK-KEI-TEN2    =   WRK-KEI-TEN    +  0.5
      *
      *            上限値＜データ
               WHEN    TNS-KZMJGN        <   WRK-SURYO9
                   IF      TNS-KZMERR        =   0  OR  2
      *            点数計算式１をせず、エラーとする
                       MOVE    ZERO          TO  WRK-KEI-TEN2
                   ELSE
      *            点数計算式２
                       COMPUTE WRK-KEI-TEN1    =  (TNS-KZMJGN
                                               -   TNS-KZMKGN)
                                               /   TNS-KZM
                                               +   0.999
                       MOVE    WRK-KEI-TEN1    TO  WRK-KEI-TEN2
                       COMPUTE WRK-KEI-TEN     =   TNS-TEN
                                               + ( WRK-KEI-TEN2
                                               *   TNS-KZMTEN )
                       COMPUTE WRK-KEI-TEN2    =   WRK-KEI-TEN  +  0.5
                   END-IF
           END-EVALUATE
      *
           IF      WRK-KEI-TEN2        >   ZERO
               MOVE    WRK-KEI-TEN2    TO TBL-SEIKYUTEN
                                                   (IDX-LF IDX-IJ)
           END-IF
           .
       200222-KIZAMICHI-EXT.
           EXIT.
      *****************************************************************
      *    患者コメントから編集する 
      *****************************************************************
       200221-PTCOM-SET-SEC              SECTION.
      *
           MOVE    SRY-HOSPNUM         TO  PTCOM-HOSPNUM
           MOVE    SRY-PTID            TO  PTCOM-PTID
           MOVE    SRY-ZAINUM          TO  PTCOM-ZAINUM
           MOVE    SRY-SRYCD (IDXA)    TO  PTCOM-SRYCD
           MOVE    SRY-INPUTNUM(IDXA)  TO  PTCOM-RENNUM
      *主キーで検索
           PERFORM 950-PTCOM-READ-SEC
           IF      FLG-PTCOM           =   ZERO
     ********* MOVE    PTCOM-INPUTCOMENT   TO  TBL-SRYMEMO 
     ********                                          (IDX-LF IDX-IJ)
               MOVE    PTCOM-INPUTCOMENT   TO  TBL-ALSNAME
                                                   (IDX-LF IDX-IJ)
           END-IF
           .
       200221-PTCOM-SET-EXT.
           EXIT.
      *****************************************************************
      *    患者コメントから編集する 
      *****************************************************************
       200251-PTCOM-SET02-SEC              SECTION.
      *
           MOVE    SRY-HOSPNUM         TO  PTCOM-HOSPNUM
           MOVE    SRY-PTID            TO  PTCOM-PTID
           MOVE    SRY-ZAINUM          TO  PTCOM-ZAINUM
           MOVE    SRY-SRYCD (IDXA)    TO  PTCOM-SRYCD
           MOVE    SRY-INPUTNUM(IDXA)  TO  PTCOM-RENNUM
      *主キーで検索
           PERFORM 950-PTCOM-READ-SEC
           IF      FLG-PTCOM           =   ZERO
               MOVE    PTCOM-INPUTCOMENT   TO  TBL-ADMINISTRATION
                                                            (IDX-LF)
           END-IF
           .
       200251-PTCOM-SET02-EXT.
           EXIT.
      *****************************************************************
      *    受付メモ編集処理
      *****************************************************************
       2009-PTMEMOINF-SEC        SECTION.
      *
      *    診療日のメモを対象とする
           MOVE    SPACE               TO  PTMEMOINF-REC
           INITIALIZE                  PTMEMOINF-REC
           MOVE    SPA-HOSPNUM         TO  PTMEMOINF-HOSPNUM
           MOVE    LNK-PTID            TO  PTMEMOINF-PTID
           MOVE    2                   TO  PTMEMOINF-MEMOKBN
           MOVE    LNK-SRYYMD          TO  PTMEMOINF-SYSYMD
      *
           MOVE    PTMEMOINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-PTMEMOINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTMEMOINF
               INITIALIZE                  PTMEMOINF-REC
           END-IF
           MOVE    SPACE               TO  WRK-MEMO
           PERFORM UNTIL    FLG-PTMEMOINF  =   1
               IF      PTMEMOINF-SRYKA     =   "00"
                   IF      WRK-MEMO        =   SPACE
                       MOVE    PTMEMOINF-MEMO  TO  WRK-MEMO
                   END-IF
               ELSE
                   IF      PTMEMOINF-SRYKA     =   LNK-SRYKA
                       MOVE    PTMEMOINF-MEMO  TO  WRK-MEMO
                   END-IF
               END-IF
               MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-PTMEMOINF-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    SPACE               TO  WRK-MEMO-G
           MOVE    WRK-MEMO            TO  WRK-MEMO-OK
           IF      WRK-MEMO        NOT =   SPACE
      *        改行を"@"とする
      *        改行を"/"とする
               INSPECT     WRK-MEMO    REPLACING  ALL  X"0A"
                                                  BY   "/"
               UNSTRING    WRK-MEMO    DELIMITED BY "＊＊＊＊＊"
                     INTO  WRK-MEMO-01
                           WRK-MEMO-02
                           WRK-MEMO-03
                           WRK-MEMO-04
                           WRK-MEMO-05
               END-UNSTRING
               IF      WRK-MEMO-01     NOT =   SPACE
                   MOVE    WRK-MEMO-01     TO  WRK-MEMO-OK
               END-IF
               IF      WRK-MEMO-02     NOT =   SPACE
                   IF      WRK-MEMO-02(1:1)    =   "/"
                       MOVE    WRK-MEMO-02(2:) TO  WRK-MEMO-OK
                   ELSE
                       MOVE    WRK-MEMO-02     TO  WRK-MEMO-OK
                   END-IF
               END-IF
               IF      WRK-MEMO-03     NOT =   SPACE
                   IF      WRK-MEMO-03(1:1)    =   "/"
                       MOVE    WRK-MEMO-03(2:) TO  WRK-MEMO-OK
                   ELSE
                       MOVE    WRK-MEMO-03     TO  WRK-MEMO-OK
                   END-IF
               END-IF
               IF      WRK-MEMO-04     NOT =   SPACE
                   IF      WRK-MEMO-04(1:1)    =   "/"
                       MOVE    WRK-MEMO-04(2:) TO  WRK-MEMO-OK
                   ELSE
                       MOVE    WRK-MEMO-04     TO  WRK-MEMO-OK
                   END-IF
               END-IF
               IF      WRK-MEMO-05     NOT =   SPACE
                   IF      WRK-MEMO-05(1:1)    =   "/"
                       MOVE    WRK-MEMO-05(2:) TO  WRK-MEMO-OK
                   ELSE
                       MOVE    WRK-MEMO-05     TO  WRK-MEMO-OK
                   END-IF
               END-IF
           END-IF
      *    括りメモ
           MOVE    WRK-MEMO-OK         TO  TBL-MEMO (1)
           .
        2009-PTMEMOINF-EXT.
           EXIT.
      *
      *****************************************************************
      *   ＴＢＬより再編集　　処理
      *****************************************************************
       2003-TBL-FL02-SET-SEC                 SECTION.
      *
      *
           PERFORM VARYING     IDX-LF   FROM   1   BY  1
      *D           UNTIL       IDX-LF   >      20
                   UNTIL       IDX-LF   >      30
               IF      TBL-SRYCD (IDX-LF)     NOT =   SPACE
      *
                   MOVE    TBL-SRYCD (IDX-LF)      TO  WRK-LF02-SRYCD
                                                            (IDX-LF)
                   MOVE    TBL-SRYNAME(IDX-LF)     TO  WRK-LF02-SRYNAME
                                                            (IDX-LF)
                   IF      TBL-KAISU (IDX-LF)      =   ZERO
                       MOVE    1               TO  TBL-KAISU (IDX-LF)
                   END-IF
      *            合計請求点数
                   COMPUTE WRK-KINGAK          =   TBL-TENNSUALL(IDX-LF)
                                               *   TBL-KAISU (IDX-LF)
                   MOVE    WRK-KINGAK          TO  WRK-RATE
                   MOVE    WRK-RATE            TO  WRK-LF02-TENNSUALL
                                                             (IDX-LF)
                   IF      TBL-SRYCD (IDX-LF)  =   "000"
      *                伝票番号
                       MOVE    LNK-DENPNUM     TO  WRK-LF02-GINGAKUALL
                                                             (IDX-LF)
                   ELSE
      *            合計請求金額
                       COMPUTE WRK-KINGAK      =   TBL-GINGAKUALL
                                                             (IDX-LF)
                                               *   TBL-KAISU (IDX-LF)
                       MOVE    WRK-KINGAK      TO  WRK-RATE
                       MOVE    WRK-RATE        TO  WRK-LF02-GINGAKUALL
                                                             (IDX-LF)
                   END-IF
      *
                   COMPUTE WRK-KINGAK          =   TBL-SYUGI (IDX-LF)
                                               *   TBL-KAISU (IDX-LF)
                   MOVE    WRK-KINGAK          TO  WRK-Z9
                   MOVE    WRK-Z9              TO  WRK-LF02-SYUGI
                                                             (IDX-LF)
                   COMPUTE WRK-KINGAK          =   TBL-ZAIRYOU
                                                             (IDX-LF)
                                               *   TBL-KAISU (IDX-LF)
                   MOVE    WRK-KINGAK          TO  WRK-Z9
                   MOVE    WRK-Z9              TO  WRK-LF02-ZAIRYOU
                                                             (IDX-LF)
                   COMPUTE WRK-KINGAK          =   TBL-YAKUZAI
                                                             (IDX-LF)
                                               *   TBL-KAISU (IDX-LF)
                   MOVE    WRK-KINGAK          TO  WRK-Z9
                   MOVE    WRK-Z9              TO  WRK-LF02-YAKUZAI
                                                             (IDX-LF)
      *
      *用法
                   MOVE    TBL-ADMINISTRATION (IDX-LF)
                                           TO  WRK-LF02-ADMINISTRATION
                                                             (IDX-LF)
                   MOVE    TBL-ADMINCODE     (IDX-LF)
                                           TO  WRK-LF02-ADMINCODE
                                                             (IDX-LF)
                   MOVE    TBL-ADMMEMO     (IDX-LF)
                                           TO  WRK-LF02-ADMMEMO
                                                             (IDX-LF)
      *回数又は日数(Z編集)
                   MOVE    TBL-KAISU (IDX-LF)  TO  WRK-Z9
                   MOVE    WRK-Z9              TO  WRK-LF02-KAISU
                                                             (IDX-LF)
      *包括区分包括フラグ(true OR false)
                   MOVE    TBL-PPSFLG (IDX-LF)
                                               TO  WRK-LF02-PPSFLG
                                                             (IDX-LF)
                   PERFORM VARYING     IDX-S  FROM    1   BY  1
      *@                   UNTIL       IDX-S  >   20
                           UNTIL       IDX-S  >   30
                       IF      TBL-IJICD (IDX-LF IDX-S)   NOT =   SPACE
                           PERFORM 20031-MEISAI-SET-SEC
                       END-IF
                   END-PERFORM
      *
      *            括りメモ
                   MOVE    TBL-MEMO (IDX-LF)   TO  WRK-LF02-MEMO(IDX-LF)
      *H29.12
      *           剤オーバー
                  IF     (FLG-ERROVER         =   1 )
                     AND (IDX-LF              =   30 )
                       MOVE    "明細数がオーバーしました"
                                              TO  WRK-LF02-EVENT
                                                          (IDX-LF 1)
                  END-IF
      *
               END-IF
           END-PERFORM
      *
           .
       2003-TBL-FL02-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   ＴＢＬより再編集明細処理
      *****************************************************************
       20031-MEISAI-SET-SEC                 SECTION.
      *
      *診療種別区分コード(0:手技、1:材料,2:薬剤)
           MOVE    TBL-SRYSYUBETSU (IDX-LF IDX-S)
                                       TO  WRK-LF02-SRYSYUBETSU
                                                        (IDX-LF IDX-S)
      *
      *医事請求コード(S + 診療行為コード)
           MOVE    TBL-IJICD (IDX-LF IDX-S)
                                       TO  WRK-LF02-IJICD
                                                        (IDX-LF IDX-S)
      *医事請求コード体系名
           MOVE    TBL-TBLID (IDX-LF IDX-S)
                                       TO  WRK-LF02-TBLID
                                                        (IDX-LF IDX-S)
      *医事請求コード
           MOVE    TBL-ALSCODE (IDX-LF IDX-S)
                                       TO  WRK-LF02-ALSCODE
                                                        (IDX-LF IDX-S)
      *医事請求コード体系名
           MOVE    TBL-ALSTBLID (IDX-LF IDX-S)
                                       TO  WRK-LF02-ALSTBLID
                                                        (IDX-LF IDX-S)
      *医事請求コード名称
           MOVE    TBL-ALSNAME (IDX-LF IDX-S)
                                       TO  WRK-LF02-ALSNAME
                                                        (IDX-LF IDX-S)
      *数量(Z編集)
           IF     (TBL-SURYO (IDX-LF IDX-S)    =   ZERO    )  OR
                 ((TBL-SURYO (IDX-LF IDX-S)    =   1  )  AND
                  (TBL-IJICD (IDX-LF IDX-S)(1:1)
                                           NOT =   "6"  AND "7" ))
               MOVE    SPACE               TO  WRK-LF02-SURYO
                                                        (IDX-LF IDX-S)
           ELSE
      *        小数点以下４桁以上判定
               MOVE    TBL-SURYO (IDX-LF IDX-S)    TO  WRK-SURYO9
               IF      WRK-SURYO-S2(4:2)       =   ZERO
      *            小数点以下３桁まで
                   MOVE    TBL-SURYO (IDX-LF IDX-S)    TO  WRK-SURYO
                   MOVE    WRK-SURYO           TO  WRK-LF02-SURYO
                                                        (IDX-LF IDX-S)
               ELSE
      *            小数点以下４桁以上
                   MOVE    TBL-SURYO (IDX-LF IDX-S)    TO  WRK-SURYO-5
                   MOVE    WRK-SURYO-5         TO  WRK-LF02-SURYO
                                                        (IDX-LF IDX-S)
               END-IF
           END-IF
      *数量コード
           MOVE    TBL-SURYOCD (IDX-LF IDX-S) 
                                       TO  WRK-LF02-SURYOCD
                                                        (IDX-LF IDX-S)
      *単位
           MOVE    TBL-TANI (IDX-LF IDX-S) 
                                       TO  WRK-LF02-TANI
                                                        (IDX-LF IDX-S)
      *請求点数(Z編集)
           IF      TBL-SEIKYUTEN (IDX-LF IDX-S)    =   ZERO
               MOVE    SPACE           TO  WRK-LF02-SEIKYUTEN
                                                        (IDX-LF IDX-S)
           ELSE
               COMPUTE WRK-GOKTEN          =   TBL-SEIKYUTEN
                                                        (IDX-LF IDX-S)
                                           *   TBL-KAISU (IDX-LF)
               MOVE    WRK-GOKTEN          TO  WRK-TANKA
               MOVE    WRK-TANKA           TO  WRK-LF02-SEIKYUTEN
                                                        (IDX-LF IDX-S)
           END-IF
      *明細単価(Z編集)
           IF      TBL-MEISAITANKA (IDX-LF IDX-S)    <   ZERO
               MOVE    TBL-MEISAITANKA (IDX-LF IDX-S)    TO  WRK-TANKA9N
               MOVE    WRK-TANKA9N           TO  WRK-LF02-MEISAITANKA
                                                        (IDX-LF IDX-S)
           ELSE
               MOVE    TBL-MEISAITANKA (IDX-LF IDX-S)    TO  WRK-TANKA9
               MOVE    WRK-TANKA9            TO  WRK-LF02-MEISAITANKA
                                                        (IDX-LF IDX-S)
           END-IF
      *所要時間(PTnHnM,5時間23分→PT5H23M)
           MOVE    TBL-DURATION (IDX-LF IDX-S) 
                                       TO  WRK-LF02-DURATION
                                                        (IDX-LF IDX-S)
      *部位
           MOVE    TBL-LOCATION (IDX-LF IDX-S) 
                                      TO  WRK-LF02-LOCATION
                                                       (IDX-LF IDX-S)
      ***
      *----（以下フィルム保留） ---------------------------------------*
      *    IF      TBL-SRYCD (IDX-LF)(1:2) =   "70"
      *フィルムサイズ
      *        MOVE    TBL-FILMSIZE (IDX-LF IDX-S 1) 
      *                                TO  WRK-LF02-FILMSIZE
      *                                                 (IDX-LF IDX-S 1)
      *フィルムサイズコード
      *        MOVE    TBL-FILMSIZECD (IDX-LF IDX-S 1) 
      *                                TO  WRK-LF02-FILMSIZECD
      *                                                 (IDX-LF IDX-S 1)
      *フィルム分割
      *        MOVE    TBL-FILMDEV (IDX-LF IDX-S 1) 
      *                                TO  WRK-FILMDEV
      *        MOVE    WRK-FILMDEV     TO  WRK-LF02-FILMDEV
      *                                                 (IDX-LF IDX-S 1)
      *フィルム枚数(Z編集)
      *        IF      TBL-FILMNUM (IDX-LF IDX-S 1)   >   ZERO
      *            MOVE    TBL-FILMNUM (IDX-LF IDX-S 1)
      *                                    TO  WRK-FILMNUM
      *            MOVE    WRK-FILMNUM     TO  WRK-LF02-FILMNUM
      *                                                 (IDX-LF IDX-S 1)
      *        END-IF
      *    END-IF
      *----（ここまで） ---------------------------------------*
      *メモ
           MOVE    TBL-SRYMEMO (IDX-LF IDX-S) 
                                       TO  WRK-LF02-SRYMEMO
                                                        (IDX-LF IDX-S)
      *
           .
       20031-MEISAI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納より請求書項目編集処理
      *****************************************************************
       2005-SYUNOU-SET-SEC                 SECTION.
      *
      *    収納テーブル読み込み
           MOVE    SPACE               TO  SYUNOU-REC
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    "2"                 TO  SYU-NYUGAIKBN
           MOVE    LNK-PTID            TO  SYU-PTID
           MOVE    LNK-DENPNUM         TO  SYU-DENPNUM
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYUNOU-REC
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SYUNOU          =   1
               GO      TO      2005-SYUNOU-SET-EXT
           END-IF
      *
      *    マル長の公費負担を保険適用金額へ
           IF      SYU-HKNTEKKBN   =   "1"
               MOVE    SYU-KOH-FTN         TO  SYU-HKNTEKMONEY
               MOVE    ZERO                TO  SYU-KOH-FTN
           END-IF
      *
      *    請求点数
           ADD     1                   TO  IDX-LF2
      *    ２０剤以上の時エラー
      *@   IF      IDX-LF2             >   20
           IF      IDX-LF2             >   30
               DISPLAY "ZAI 30 OVER ???"
               MOVE    1               TO  FLG-ERROVER
           ELSE
               MOVE    IDX-LF2             TO  IDX-LF
               PERFORM 20051-SEIKYU-TEN-SEC
           END-IF
      *
           PERFORM 20050-JIHIMEISYO-SET-SEC
      *    自費（消費税なし）
           IF     (SYU-TOTAL-TGMONEY   >   ZERO )  OR
                  (SYU-JIHI-TOTAL      >   ZERO )
               ADD     1                   TO  IDX-LF2
      *@       IF      IDX-LF2             >   20
               IF      IDX-LF2             >   30
                   DISPLAY "ZAI 30 OVER ???"
                   MOVE    1               TO  FLG-ERROVER
               ELSE
                   MOVE    IDX-LF2             TO  IDX-LF
                   PERFORM 20052-JIHI-TEN-SEC
               END-IF
           END-IF
      *
      *    自費（消費税あり）
           IF     (SYU-TOTAL-TGMONEY-TAX   >   ZERO )  OR
                  (SYU-JIHI-TOTAL-TAX      >   ZERO )
               ADD     1                   TO  IDX-LF2
      *@       IF      IDX-LF2             >   20
               IF      IDX-LF2             >   30
                   DISPLAY "ZAI 30 OVER ???"
                   MOVE    1               TO  FLG-ERROVER
               ELSE
                   MOVE    IDX-LF2             TO  IDX-LF
                   PERFORM 20053-JIHITEX-TEN-SEC
               END-IF
           END-IF
           .
       2005-SYUNOU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求点数他処理
      *****************************************************************
       20051-SEIKYU-TEN-SEC                 SECTION.
      *
      *    回数
           MOVE    1                   TO  TBL-KAISU (IDX-LF)
      *
      *    診療行為区分コード
           MOVE    "000"               TO  TBL-SRYCD (IDX-LF)
      *    ＣＬＡＩＭ用診療行為区分コード
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       CLAIM007-MAX
               IF      TBL-SRYCD (IDX-LF)  =   CLAIM007-CLAIM  (IDX)
                   MOVE    CLAIM007-CLAIM(IDX) 
                                           TO  TBL-SRYCD  (IDX-LF)
                   MOVE    CLAIM007-NAME (IDX)
                                           TO  TBL-SRYNAME(IDX-LF)
                   MOVE    CLAIM007-MAX    TO  IDX
               END-IF
           END-PERFORM
      *    合計請求点数
           MOVE    SYU-TOTAL-HKNTEN     TO  TBL-TENNSUALL(IDX-LF)
      *    請求点数
           MOVE    ZERO                TO  IDX-IJ
           MOVE    ZERO                TO  IDX-IJ2
           MOVE    ZERO                TO  FLG-ERR
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       16
               ADD     1                   TO  IDX-IJ
               ADD     1                   TO  IDX-IJ2
      *        請求コード
               MOVE    IDX                 TO  TBL-IJICD(IDX-LF IDX-IJ)
               MOVE    "0"                 TO  TBL-SRYSYUBETSU
                                                        (IDX-LF IDX-IJ)
      *        請求点数
               MOVE    SYU-HKNTEN (IDX)    TO  TBL-SEIKYUTEN
                                                        (IDX-LF IDX-IJ)
               EVALUATE    IDX
                   WHEN    1
                       MOVE    "初・再診料"    TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    2
                       MOVE    "医学管理等"    TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    3
                       MOVE    "在宅医療"      TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    4
                       MOVE    "投薬"          TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    5
                       MOVE    "注射"          TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    6
                       MOVE    "処置"          TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    7
                       MOVE    "手術"          TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    8
                       MOVE    "麻酔"          TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    9
                       MOVE    "検査"          TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    10
                       MOVE    "画像診断"      TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    11
                       MOVE    "リハビリ"      TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    12
                       MOVE    "精神科専門"    TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    13
                       MOVE    "放射線治療"    TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    14
                       MOVE    "病理診断"      TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    15
                       MOVE    "入院料等"      TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    16
                       MOVE    "療養担当手当"  TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
               END-EVALUATE
           END-PERFORM
      *    保険適用金額
      *
           ADD     1                   TO  IDX-IJ
           ADD     1                   TO  IDX-IJ2
           MOVE    IDX-IJ2             TO  TBL-IJICD(IDX-LF IDX-IJ)
           MOVE    "0"                 TO  TBL-SRYSYUBETSU
                                                        (IDX-LF IDX-IJ)
           MOVE    SYU-HKNTEKMONEY     TO  TBL-MEISAITANKA
                                                    (IDX-LF IDX-IJ)
           MOVE    "負担金額"          TO  TBL-ALSNAME
                                                      (IDX-LF IDX-IJ)
      *    薬剤一部負担金
           ADD     1                   TO  IDX-IJ
           ADD     1                   TO  IDX-IJ2
           MOVE    IDX-IJ2             TO  TBL-IJICD(IDX-LF IDX-IJ)
           MOVE    "0"                 TO  TBL-SRYSYUBETSU
                                                        (IDX-LF IDX-IJ)
           MOVE    SYU-YKZ-FTN         TO  TBL-MEISAITANKA
                                                    (IDX-LF IDX-IJ)
           MOVE    "薬剤一部負担金"    TO  TBL-ALSNAME
                                                      (IDX-LF IDX-IJ)
      *    老人一部負担金
           ADD     1                   TO  IDX-IJ
           ADD     1                   TO  IDX-IJ2
           MOVE    IDX-IJ2             TO  TBL-IJICD(IDX-LF IDX-IJ)
           MOVE    "0"                 TO  TBL-SRYSYUBETSU
                                                        (IDX-LF IDX-IJ)
           MOVE    SYU-RJN-FTN         TO  TBL-MEISAITANKA
                                                    (IDX-LF IDX-IJ)
           MOVE    "老人一部負担金"    TO  TBL-ALSNAME
                                                      (IDX-LF IDX-IJ)
      *    公費一部負担金
           ADD     1                   TO  IDX-IJ
           ADD     1                   TO  IDX-IJ2
           MOVE    IDX-IJ2             TO  TBL-IJICD(IDX-LF IDX-IJ)
           MOVE    "0"                 TO  TBL-SRYSYUBETSU
                                                        (IDX-LF IDX-IJ)
           MOVE    SYU-KOH-FTN         TO  TBL-MEISAITANKA
                                                    (IDX-LF IDX-IJ)
           MOVE    "公費一部負担金"    TO  TBL-ALSNAME
                                                      (IDX-LF IDX-IJ)
      *    調整金
           ADD     1                   TO  IDX-IJ
           ADD     1                   TO  IDX-IJ2
           MOVE    IDX-IJ2             TO  TBL-IJICD(IDX-LF IDX-IJ)
           MOVE    "0"                 TO  TBL-SRYSYUBETSU
                                                     (IDX-LF IDX-IJ)
           MOVE    SYU-CHOSEI          TO  TBL-MEISAITANKA
                                                    (IDX-LF IDX-IJ)
           MOVE    "調整金"            TO  TBL-ALSNAME
                                                      (IDX-LF IDX-IJ)
      *
      *    請求金額
           ADD     1                   TO  IDX-IJ
           ADD     1                   TO  IDX-IJ2
      *@   IF      IDX-IJ              >   20
           IF      IDX-IJ              >   30
               PERFORM 200531-IDX-CNT-SEC
               IF      FLG-ERR            =   1
                   GO      TO      2005-SYUNOU-SET-EXT
               END-IF
           END-IF
           MOVE    IDX-IJ2             TO  TBL-IJICD(IDX-LF IDX-IJ)
           MOVE    "0"                 TO  TBL-SRYSYUBETSU
                                                        (IDX-LF IDX-IJ)
           MOVE    SYU-SKYMONEY        TO  TBL-MEISAITANKA
                                                    (IDX-LF IDX-IJ)
           MOVE    "請求金額"          TO  TBL-ALSNAME
                                                      (IDX-LF IDX-IJ)
      *    収納明細（入金額）
           INITIALIZE                      SYUMEI-REC
           MOVE    SYU-HOSPNUM         TO  SMEI-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID            TO  SMEI-PTID
           MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
      *
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syumei"        TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 980-SYUMEI-READ-SEC
           ELSE
               INITIALIZE                      SYUMEI-REC
               MOVE    1                   TO  FLG-SYUMEI
           END-IF
           MOVE    SPACE               TO  HZN-SYUMEI-REC
           MOVE    ZERO                TO  WRK-JYUTO-MONEY
      *    新規で複数の伝票の場合
           IF      SMEI-JOUTAIKBN      NOT =   "8"
               IF      SMEI-MEISAIJOUTAIKBN    =   "1"
                   MOVE    SMEI-NYUHEN-MONEY   TO  WRK-JYUTO-MONEY
                   MOVE    ZERO                TO  SMEI-NYUHEN-MONEY
               END-IF
               MOVE    SYUMEI-REC          TO  HZN-SYUMEI-REC
      *
               MOVE    "tbl_syumei"        TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 980-SYUMEI-READ-SEC
               IF      FLG-SYUMEI          =   1
                   MOVE    HZN-SYUMEI-REC      TO  SYUMEI-REC
               ELSE
                   IF      SMEI-MEISAIJOUTAIKBN    =   "1"
                       MOVE    SMEI-NYUHEN-MONEY   TO  WRK-JYUTO-MONEY
                       MOVE    HZN-SMEI-NYUHEN-MONEY
                                                   TO  SMEI-NYUHEN-MONEY
                   END-IF
               END-IF
           END-IF
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    入金合計額
           ADD     1                   TO  IDX-IJ
           ADD     1                   TO  IDX-IJ2
           MOVE    IDX-IJ2             TO  TBL-IJICD(IDX-LF IDX-IJ)
           MOVE    "0"                 TO  TBL-SRYSYUBETSU
                                                        (IDX-LF IDX-IJ)
           MOVE    SMEI-NYUHEN-MONEY   TO  TBL-MEISAITANKA
                                                    (IDX-LF IDX-IJ)
           MOVE    "今回入金額"        TO  TBL-ALSNAME
                                                      (IDX-LF IDX-IJ)
      *
      *    訂正時
           IF      SMEI-JOUTAIKBN      =   "8"
               ADD     1                   TO  IDX-IJ
               ADD     1                   TO  IDX-IJ2
               MOVE    IDX-IJ2             TO  TBL-IJICD(IDX-LF IDX-IJ)
               MOVE    "0"                 TO  TBL-SRYSYUBETSU
                                                        (IDX-LF IDX-IJ)
               MOVE    SMEI-SKYMONEY       TO  TBL-MEISAITANKA
                                                    (IDX-LF IDX-IJ)
               MOVE    "訂正分請求金額"    TO  TBL-ALSNAME
                                                      (IDX-LF IDX-IJ)
           END-IF
      *
      *    新規で充当額がある場合
           IF      WRK-JYUTO-MONEY NOT =   ZERO
               ADD     1                   TO  IDX-IJ
               ADD     1                   TO  IDX-IJ2
               MOVE    IDX-IJ2             TO  TBL-IJICD(IDX-LF IDX-IJ)
               MOVE    "0"                 TO  TBL-SRYSYUBETSU
                                                        (IDX-LF IDX-IJ)
               MOVE    WRK-JYUTO-MONEY     TO  TBL-MEISAITANKA
                                                        (IDX-LF IDX-IJ)
               MOVE    "入金（充当額）"    TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
           END-IF
           .
       2005-SYUNOU-SET-EXT.
           EXIT.
      *****************************************************************
      *    自費名称処理
      *****************************************************************
       20050-JIHIMEISYO-SET-SEC                 SECTION.
      *
           INITIALIZE                  TBL-JIHIMSG-AREA
           MOVE    SPACE               TO  SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    LNK-SYSYMD          TO  SYS-1013-STYUKYMD
           MOVE    LNK-SYSYMD          TO  SYS-1013-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-1013-REC
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               INITIALIZE                  SYS-1013-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM
               UNTIL       FLG-SYSKANRI    =   1
               IF      SYS-1013-KBNCD(1:1)     =   "2"
                   EVALUATE    SYS-1013-KBNCD(2:1)
                       WHEN    "1"
                           MOVE    SYS-1013-JIHINAME
                                               TO  TBL-JIHIMSG (1)
                       WHEN    "2"
                           MOVE    SYS-1013-JIHINAME
                                               TO  TBL-JIHIMSG (2)
                       WHEN    "3"
                           MOVE    SYS-1013-JIHINAME
                                               TO  TBL-JIHIMSG (3)
                       WHEN    "4"
                           MOVE    SYS-1013-JIHINAME
                                               TO  TBL-JIHIMSG (4)
                       WHEN    "5"
                           MOVE    SYS-1013-JIHINAME
                                               TO  TBL-JIHIMSG (5)
                       WHEN    "6"
                           MOVE    SYS-1013-JIHINAME
                                               TO  TBL-JIHIMSG (6)
                       WHEN    "7"
                           MOVE    SYS-1013-JIHINAME
                                               TO  TBL-JIHIMSG (7)
                       WHEN    "8"
                           MOVE    SYS-1013-JIHINAME
                                               TO  TBL-JIHIMSG (8)
                       WHEN    "9"
                           MOVE    SYS-1013-JIHINAME
                                               TO  TBL-JIHIMSG (9)
                       WHEN    "0"
                           MOVE    SYS-1013-JIHINAME
                                               TO  TBL-JIHIMSG (10)
                   END-EVALUATE
               END-IF
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-1013-REC
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       10
               IF      TBL-JIHIMSG (IDX)   =   SPACE
                   MOVE    "未設定"        TO  TBL-JIHIMSG (IDX)
               END-IF
           END-PERFORM
           .
       20050-JIHIMEISYO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    自費（消費税なし）処理
      *****************************************************************
       20052-JIHI-TEN-SEC                 SECTION.
      *
      *    回数
           MOVE    1                   TO  TBL-KAISU (IDX-LF)
      *
      *    診療行為区分コード
           MOVE    "014"               TO  TBL-SRYCD (IDX-LF)
      *    ＣＬＡＩＭ用診療行為区分コード
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       CLAIM007-MAX
               IF      TBL-SRYCD (IDX-LF)  =   CLAIM007-CLAIM  (IDX)
                   MOVE    CLAIM007-CLAIM(IDX) 
                                           TO  TBL-SRYCD  (IDX-LF)
                   MOVE    CLAIM007-NAME (IDX)
                                           TO  TBL-SRYNAME(IDX-LF)
                   MOVE    CLAIM007-MAX    TO  IDX
               END-IF
           END-PERFORM
      *    合計金額
           COMPUTE TBL-GINGAKUALL (IDX-LF)
                                       =   SYU-TOTAL-TGMONEY
                                       +   SYU-JIHI-TOTAL
      *    保険分
           MOVE    ZERO                TO  IDX-IJ
           MOVE    ZERO                TO  FLG-ERR
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       16
               IF      SYU-TGMONEY (IDX)   NOT =   ZERO
                   ADD     1               TO  IDX-IJ
      *            請求コード
                   MOVE    "1"             TO  TBL-IJICD(IDX-LF IDX-IJ)
                                                        (1:1)
                   MOVE    IDX(2:3)        TO  TBL-IJICD(IDX-LF IDX-IJ)
                                                        (2:3)
                   MOVE    "0"             TO  TBL-SRYSYUBETSU
                                                        (IDX-LF IDX-IJ)
      *            自費金額
                   MOVE    SYU-TGMONEY (IDX)   TO  TBL-MEISAITANKA
                                                        (IDX-LF IDX-IJ)
                   EVALUATE    IDX
                   WHEN    1
                       MOVE    "初・再診料"    TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    2
                       MOVE    "医学管理等"    TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    3
                       MOVE    "在宅医療"      TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    4
                       MOVE    "投薬"          TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    5
                       MOVE    "注射"          TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    6
                       MOVE    "処置"          TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    7
                       MOVE    "手術"          TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    8
                       MOVE    "麻酔"          TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    9
                       MOVE    "検査"          TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    10
                       MOVE    "画像診断"      TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    11
                       MOVE    "リハビリ"      TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    12
                       MOVE    "精神科専門"      TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    13
                       MOVE    "放射線治療"      TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    14
                       MOVE    "病理診断"      TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    15
                       MOVE    "入院料等"      TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    16
                       MOVE    "療養担当手当"  TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   END-EVALUATE
               END-IF
           END-PERFORM
      *    自費
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL  (IDX     >       10  )
                       OR (FLG-ERR     =   1   )
               IF      SYU-JIHI-TAXNASI (IDX)  NOT =   ZERO
                   ADD     1               TO  IDX-IJ
      *@           IF      IDX-IJ              >   20
                   IF      IDX-IJ              >   30
                       PERFORM 200531-IDX-CNT-SEC
                   END-IF
                   IF      FLG-ERR            =   ZERO
      *
      *            請求コード
                   MOVE    IDX             TO  TBL-IJICD(IDX-LF IDX-IJ)
                   MOVE    "0"             TO  TBL-SRYSYUBETSU
                                                        (IDX-LF IDX-IJ)
      *            自費金額
                   MOVE    SYU-JIHI-TAXNASI(IDX)   TO  TBL-MEISAITANKA
                                                        (IDX-LF IDX-IJ)
                   MOVE    TBL-JIHIMSG (IDX)   TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   END-IF
               END-IF
           END-PERFORM
           .
       20052-JIHI-TEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルカウント処理
      *****************************************************************
       200531-IDX-CNT-SEC                 SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
           ADD     1                   TO  IDX-LF2
      *@   IF      IDX-LF2             >   20
           IF      IDX-LF2             >   30
               DISPLAY "ZAI 30 OVER ???"
               MOVE    1                   TO  FLG-ERR
               MOVE    1                   TO  FLG-ERROVER
               GO      TO      200531-IDX-CNT-EXT
           END-IF
           MOVE    TBL-KAISU  (IDX-LF) TO  TBL-KAISU (IDX-LF2)
           MOVE    TBL-SRYCD  (IDX-LF) TO  TBL-SRYCD (IDX-LF2)
           MOVE    TBL-SRYNAME(IDX-LF) TO  TBL-SRYNAME (IDX-LF2)
           MOVE    TBL-TENNSUALL(IDX-LF)
                                       TO  TBL-TENNSUALL(IDX-LF2)
           MOVE    TBL-GINGAKUALL(IDX-LF)
                                       TO  TBL-GINGAKUALL(IDX-LF2)
           MOVE    IDX-LF2             TO  IDX-LF
           MOVE    1                   TO  IDX-IJ
           .
       200531-IDX-CNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    自費（消費税あり）処理
      *****************************************************************
       20053-JIHITEX-TEN-SEC                 SECTION.
      *
      *    回数
           MOVE    1                   TO  TBL-KAISU (IDX-LF)
      *
      *    診療行為区分コード
           MOVE    "016"               TO  TBL-SRYCD (IDX-LF)
      *    ＣＬＡＩＭ用診療行為区分コード
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       CLAIM007-MAX
               IF      TBL-SRYCD (IDX-LF)  =   CLAIM007-CLAIM  (IDX)
                   MOVE    CLAIM007-CLAIM(IDX) 
                                           TO  TBL-SRYCD  (IDX-LF)
                   MOVE    CLAIM007-NAME (IDX)
                                           TO  TBL-SRYNAME(IDX-LF)
                   MOVE    CLAIM007-MAX    TO  IDX
               END-IF
           END-PERFORM
      *    合計金額
           IF      LNK-SRYYMD          <   "20070401"
               COMPUTE TBL-GINGAKUALL (IDX-LF)
                                       =   SYU-TOTAL-TGMONEY-TAX
                                       +   SYU-JIHI-TOTAL-TAX
                                       +   SYU-JIHI-TAX
           ELSE
               COMPUTE TBL-GINGAKUALL (IDX-LF)
                                       =   SYU-TOTAL-TGMONEY-TAX
                                       +   SYU-JIHI-TOTAL-TAX
           END-IF
      *    保険分
           MOVE    ZERO                TO  IDX-IJ
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       16
               IF      SYU-TGMONEY-TAX (IDX)   NOT =   ZERO
                   ADD     1               TO  IDX-IJ
      *            請求コード
                   MOVE    "1"             TO  TBL-IJICD(IDX-LF IDX-IJ)
                                                        (1:1)
                   MOVE    IDX(2:3)        TO  TBL-IJICD(IDX-LF IDX-IJ)
                                                        (2:3)
                   MOVE    "0"             TO  TBL-SRYSYUBETSU
                                                        (IDX-LF IDX-IJ)
      *            自費金額
                   MOVE    SYU-TGMONEY-TAX (IDX)   TO  TBL-MEISAITANKA
                                                        (IDX-LF IDX-IJ)
                   EVALUATE    IDX
                   WHEN    1
                       MOVE    "初・再診料"    TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    2
                       MOVE    "医学管理等"    TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    3
                       MOVE    "在宅医療"      TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    4
                       MOVE    "投薬"          TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    5
                       MOVE    "注射"          TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    6
                       MOVE    "処置"          TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    7
                       MOVE    "手術・麻酔"    TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    8
                       MOVE    "麻酔"          TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    9
                       MOVE    "検査"          TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    10
                       MOVE    "画像診断"      TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    11
                       MOVE    "リハビリ"      TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    12
                       MOVE    "精神科専門"      TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    13
                       MOVE    "放射線治療"      TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    14
                       MOVE    "病理診断"      TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    15
                       MOVE    "入院料等"      TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   WHEN    16
                       MOVE    "療養担当手当"  TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                   END-EVALUATE
               END-IF
           END-PERFORM
      *    自費
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL  (IDX     >       10  )
                       OR (FLG-ERR     =   1   )
               IF      SYU-JIHI-TAXARI (IDX)   NOT =   ZERO
                   ADD     1               TO  IDX-IJ
      *@           IF      IDX-IJ              >   20
                   IF      IDX-IJ              >   30
                       PERFORM 200531-IDX-CNT-SEC
                   END-IF
                   IF      FLG-ERR            =   ZERO
      *
      *            請求コード
                   MOVE    IDX             TO  TBL-IJICD(IDX-LF IDX-IJ)
                   MOVE    "0"             TO  TBL-SRYSYUBETSU
                                                        (IDX-LF IDX-IJ)
      *            自費金額
                   MOVE    SYU-JIHI-TAXARI(IDX)
                                               TO  TBL-MEISAITANKA
                                                        (IDX-LF IDX-IJ)
                   MOVE    TBL-JIHIMSG (IDX)   TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
                  END-IF
               END-IF
           END-PERFORM
           IF      SYU-JIHI-TAX    NOT =   ZERO
               ADD     1               TO  IDX-IJ
      *@       IF      IDX-IJ              >   20
               IF      IDX-IJ              >   30
                   PERFORM 200531-IDX-CNT-SEC
               END-IF
               IF      FLG-ERR            =   ZERO
      *
      *        請求コード
               MOVE    "0099"          TO  TBL-IJICD(IDX-LF IDX-IJ)
               MOVE    "0"             TO  TBL-SRYSYUBETSU
                                                        (IDX-LF IDX-IJ)
      *        自費金額消費税
               MOVE    SYU-JIHI-TAX       TO  TBL-MEISAITANKA
                                                        (IDX-LF IDX-IJ)
               MOVE    "自費金額消費税"   TO  TBL-ALSNAME
                                                        (IDX-LF IDX-IJ)
               END-IF
           END-IF
           .
       20053-JIHITEX-TEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自費の時処理
      *****************************************************************
       20020-JIHI-SYORI-SEC                 SECTION.
      *
           IF      SRY-RENNUM          =   1
               PERFORM 200201-JIHI-ACCT-SEC
           END-IF
      *H29.12  剤オーバー
           IF      FLG-ERROVER     NOT =   ZERO
               GO      TO      20020-JIHI-SYORI-EXT
           END-IF
      *
           PERFORM VARYING     IDXA    FROM    1   BY  1
                   UNTIL       IDXA    >   5
               IF      SRY-SRYCD (IDXA)    NOT =   SPACE
                    PERFORM 200202-JIHI-HENSYU-SEC
               END-IF
           END-PERFORM
      *
           .
       20020-JIHI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自費編集処理
      *****************************************************************
       200201-JIHI-ACCT-SEC                 SECTION.
      *
      *    診療会計マスター読み込み
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    LNK-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    LNK-PTID            TO  ACCT-PTID
           MOVE    LNK-SRYKA           TO  ACCT-SRYKA
           MOVE    SRY-SRYYM           TO  ACCT-SRYYM
           MOVE    SRY-SRYKBN          TO  ACCT-SRYKBN
           MOVE    SRY-ZAINUM          TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           ADD     1                   TO  IDX-LF2
      *@   ２０剤以上の時エラー
      *@   IF      IDX-LF2             >   20
      *    ３０剤以上の時エラー
           IF      IDX-LF2             >   30
               DISPLAY "ZAI 30 OVER ???"
      *!!!     MOVE    30              TO  IDX-LF2
               MOVE    1               TO  FLG-ERROVER
               GO      TO      200201-JIHI-ACCT-EXT
           END-IF
      *
           MOVE    IDX-LF2             TO  IDX-LF
      *
      *    回数
           MOVE    LNK-SRYYMD(7:2)     TO  IDX-ACT
           MOVE    ACCT-DAY (IDX-REN IDX-ACT)
                                       TO  TBL-KAISU (IDX-LF)
      *    診療行為区分コード
           IF      WRK-SRYKBN          =   "95"
               MOVE    "J95"               TO  TBL-SRYCD    (IDX-LF)
               MOVE    "保険外（消費税なし）"
                                           TO  TBL-SRYNAME (IDX-LF)
           ELSE
               MOVE    "J96"               TO  TBL-SRYCD    (IDX-LF)
               MOVE    "保険外（消費税あり）"
                                           TO  TBL-SRYNAME (IDX-LF)
           END-IF
      *    請求点数
           MOVE    ZERO                TO  TBL-TENNSUALL (IDX-LF)
           MOVE    ZERO                TO  TBL-SYUGI (IDX-LF)
           MOVE    ZERO                TO  TBL-ZAIRYOU (IDX-LF)
           MOVE    ZERO                TO  TBL-YAKUZAI (IDX-LF)
      *    請求金額
           MOVE    ZERO                TO  TBL-GINGAKUALL(IDX-LF)
      *
           .
       200201-JIHI-ACCT-EXT.
           EXIT.
      *
      *****************************************************************
      *    自費編集処理
      *****************************************************************
       200202-JIHI-HENSYU-SEC                 SECTION.
      *
      *    点数検索
           MOVE    SRY-SRYCD (IDXA)    TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
      *
           ADD     1               TO  IDX-IJ
      *    １剤で３０行以上の時
      *@   IF      IDX-IJ              >   20
           IF      IDX-IJ              >   30
               DISPLAY "*** ORCL0021 INDEX OVER ****"
               GO      TO  200202-JIHI-HENSYU-EXT
           END-IF
      *
           MOVE    "0"                 TO  TBL-SRYSYUBETSU
                                                      (IDX-LF IDX-IJ)
           MOVE    SRY-SRYCD(IDXA)     TO  TBL-IJICD  (IDX-LF IDX-IJ)
           MOVE    TNS-NAME            TO  TBL-ALSNAME
                                                      (IDX-LF IDX-IJ)
           MOVE    SRY-JIHIMONEY(IDXA) TO  TBL-MEISAITANKA
                                                      (IDX-LF IDX-IJ)
           MOVE    1                   TO  TBL-SURYO  (IDX-LF IDX-IJ)
      *H30.2
      *    メモにフリーコメントの内容セット
           IF      SRY-INPUTNUM (IDXA)  NOT =   ZERO
               PERFORM 200221-PTCOM-SET-SEC
           END-IF
      *
      *    自費合計
           ADD     SRY-JIHIMONEY(IDXA) TO  TBL-GINGAKUALL(IDX-LF)
           .
       200202-JIHI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF      WRK-LF02-IJICD(18 1)   =  LOW-VALUE
                DISPLAY "L0021 LOW-VALUE = " 
           END-IF
           IF      WRK-LF02-IJICD(18 1)   =  SPACE
                DISPLAY "L0021 SPACE = " 
           END-IF
      *****CLOSE    LF03-FILE     
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付変換処理
      *****************************************************************
       500-HIZUKE-HENSYU-SEC        SECTION.
      *
           IF      WRK-WYMD         =   SPACE
               MOVE    ALL "0"          TO  WRK-WYMD
           END-IF
      *
           IF      WRK-WYMD         =   ALL "9"
               MOVE    "99991231"       TO  WRK-WYMD
           END-IF
      *
           MOVE    WRK-WYY          TO  WRK-HENYY
           MOVE    WRK-WMM          TO  WRK-HENMM
           MOVE    WRK-WDD          TO  WRK-HENDD
           .
       500-HIZUKE-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＵＩＤ取得処理
      *****************************************************************
       500-UUID-SET-SEC             SECTION.
      *
           INITIALIZE              UIDIO-AREA.
           CALL  "orcuidset"       USING   ST UIDIO-AREA.
      *    WIN-EXEC GRAPH "DLL_orcuidset", UIDIO-AREA, " ", " ",
      *                                    UIDIO-AREA, ST
           .
       500-UUID-SET-EXT.
           EXIT.
      *****************************************************************
      *    全角拡張漢字変換処理
      *****************************************************************
       800-ORCSKANACHK-SEC        SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-KOUMOKU         TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-KOUMOKU
           .
       800-ORCSKANACHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    LNK-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    LNK-SRYYMD          TO  TNS-YUKOEDYMD
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
      *R02.2
      *****************************************************************
      *    点数マスタ付加情報検索処理
      *****************************************************************
       9101-TENSUPLUS-READ-SEC          SECTION.
      *
           MOVE    SPACE               TO  TENSUPLUS-REC
           INITIALIZE                      TENSUPLUS-REC
           MOVE    TNS-SRYCD           TO  TNSP-SRYCD
           MOVE    LNK-SRYYMD          TO  TNSP-YUKOSTYMD
           MOVE    LNK-SRYYMD          TO  TNSP-YUKOEDYMD
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
       900-SRYACT-READ-SEC         SECTION.
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
       900-SRYACT-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスター読込
      *****************************************************************
       930-SRYACCT-NEXT-SEC         SECTION.
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
       930-SRYACCT-NEXT-EXT.
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
      *
      *****************************************************************
      *    収納順読み
      *****************************************************************
       980-SYUNOU-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYUNOU-REC 
               MOVE    ZERO                TO  FLG-SYUNOU
           ELSE
               MOVE    1                   TO  FLG-SYUNOU
           END-IF
      *
           .
       980-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細読込み処理
      *****************************************************************
       980-SYUMEI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYUMEI-REC 
               MOVE    ZERO                TO  FLG-SYUMEI
           ELSE
               MOVE    1                   TO  FLG-SYUMEI
           END-IF
      *
           .
       980-SYUMEI-READ-EXT.
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
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
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
                   MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
                   MOVE    ZERO            TO  FLG-HKNCOMBI
               ELSE
                  INITIALIZE                   HKNCOMBI-REC
                  MOVE    1                TO  FLG-HKNCOMBI
               END-IF
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *****************************************************************
      *    ＵＩＤデータ読込処理
      *****************************************************************
       903-CLMUID-READ-SEC         SECTION.
      *
           MOVE    CLMUID-REC          TO  MCPDATA-REC
           MOVE    "tbl_clm_uid"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_clm_uid"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  CLMUID-REC
                   MOVE    ZERO                TO  FLG-CLMUID
               ELSE
                   MOVE    1                   TO  FLG-CLMUID
                   INITIALIZE                  CLMUID-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-CLMUID
               INITIALIZE                  CLMUID-REC
           END-IF
      *
           MOVE    "tbl_clm_uid"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       903-CLMUID-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者メモ読込処理
      *****************************************************************
       910-PTMEMOINF-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTMEMOINF-REC
               MOVE    ZERO                TO  FLG-PTMEMOINF
           ELSE
               INITIALIZE                      PTMEMOINF-REC
               MOVE    1                   TO  FLG-PTMEMOINF
           END-IF
      *
           .
       910-PTMEMOINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスター読込（主キー）
      *****************************************************************
       950-SRYACCTPLUS-READ-SEC           SECTION.
      *
           MOVE    SPACE               TO  SRYACCTPLUS-REC
           INITIALIZE                  SRYACCTPLUS-REC
           MOVE    ACCT-HOSPNUM        TO  ACCTP-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  ACCTP-NYUGAIKBN
           MOVE    ACCT-PTID           TO  ACCTP-PTID
           MOVE    ACCT-SRYKA          TO  ACCTP-SRYKA
           MOVE    ACCT-SRYYM          TO  ACCTP-SRYYM
           MOVE    ACCT-ZAINUM         TO  ACCTP-ZAINUM
           MOVE    1                   TO  ACCTP-RENNUM
      *
           MOVE    SRYACCTPLUS-REC     TO  MCPDATA-REC
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacctplus"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  SRYACCTPLUS-REC
                   MOVE    ZERO                TO  FLG-SRYACCTPLUS
               ELSE
                   MOVE    1                   TO  FLG-SRYACCTPLUS
                   INITIALIZE                  SRYACCTPLUS-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SRYACCTPLUS
               INITIALIZE                  SRYACCTPLUS-REC
           END-IF
      *
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       950-SRYACCTPLUS-READ-EXT.
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
