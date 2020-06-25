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
       PROGRAM-ID.             ORCGK06.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 算定履歴照会（Ｋ０６）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC−多々納  01.04.18 戻る時、確認画面を表示する等
      *  01.00.02    MCC−多々納  02.05.20 保険組合せ追加
      *  01.00.03    NACL-多々納  05/11/17  MONFUNC 対応
      *  03.04.00    NACL-多々納  07/01/30  排他中処理追加
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *  04.05.00    NACL-多々納  09/06/XX  数量小数５桁対応
      *  04.05.00    NACL-多々納  09/11/XX  移行履歴、リハビリコメント登録対応、登録変更対応
      *  04.07.00    NACL-多々納  13/02/XX  労災レセ電コード対応(H25.7)
      *  04.07.00    NACL-多々納  13/07/XX  第三者行為対応
      *  04.08.00    NACL-多々納  16/02/XX  初診料新規追加対応
      *  04.08.00    NACL-多々納  16/03/XX  Ｈ２８年４月改正対応
      *  05.00.00    NACL-多々納  19/10/XX  初診料（移行）削除対応対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    算定履歴画面パラメタ
           COPY    "K06SCR-SPA".
      *
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-TENSUPLUS       PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-SANTEIPLUS      PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-SRYKARRK        PIC 9(01).
           03  FLG-SYORI           PIC 9(01).
           03  FLG-HEN             PIC 9(01).
           03  FLG-TOUROKU         PIC 9(01).
           03  FLG-SANTEIPLUS-CHK  PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDW                 PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX2                PIC 9(04).
      *----(01.00.01) LINE ADD START ----------------------------------
           03  IDY-CHK             PIC 9(04).
      *----(01.00.01) LINE ADD END   ----------------------------------
           03  IDXS                PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-GMN-CUR         PIC 9(02).
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-WIDGET.
               05  WRK-WIDGET1     PIC X(03).
               05  WRK-WIDGET2     PIC 9(02).
           03  WRK-YMD             PIC X(09).
           03  WRK-YMDMAX          PIC 9(01). 
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
      *    保険適用期間
           03  WRK-ST-SRYYMD.
               05  WRK-STYY            PIC 9(04).
               05  WRK-STMM            PIC 9(02).
               05  WRK-STDD            PIC 9(02).
           03  WRK-ED-SRYYMD.
               05  WRK-EDYY            PIC 9(04).
               05  WRK-EDMM            PIC 9(02).
               05  WRK-EDDD            PIC 9(02).
      *
      *----(01.00.01) LINE ADD START ----------------------------------
           03  WRK-KIDMSG          PIC X(80).
      *----(01.00.01) LINE ADD END   ----------------------------------
           03  WRK-DAY             PIC 9(02).
      *----(01.00.01) LINE ADD END   ----------------------------------
      *入力タイプ
           03  WRK-INTYPE             PIC X(01).
      *
           03  WRK-ZZ                 PIC ZZ9.
           03  WRK-Z9                 PIC ZZZZZZZZ.
      *
           03  WRK-SRYYM.
               05  WRK-SRY-YY         PIC 9(04).
               05  WRK-SRY-MM         PIC 9(02).
      *
           03  WRK-SRYYMD.
               05  WRK-SRYYM-X        PIC X(06).
               05  WRK-SRYDD          PIC X(02).
      *
           03  WRK-RENNUM             PIC 9(03).
           03  WRK-RENNUM2            PIC 9(03).
      *    保険組合せ名称
           03  WRK-SRYCD              PIC X(09).
      *    保険組合せ名称
           03  WRK-HKNCOMBI           PIC X(04).
           03  WRK-HKNCOMBIMEI        PIC X(100).
           03  WRK-HKNKBN-MEI         PIC X(02).
           03  WRK-COMB-TEKSTYMD      PIC X(08).
           03  WRK-COMB-TEKEDYMD      PIC X(08).
      *
           03  WRK-LSTYMD             PIC X(08).
           03  WRK-SANTEIYMD.
               05  WRK-SANYM-X        PIC X(06).
               05  WRK-SANDD          PIC X(02).
      *
           03  WRK-YUKOSTYMD           PIC X(08).
      *
      *R01.10
           03  WRK-LST-SRYKA           PIC X(02).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-ACCT-SRYKA          PIC X(02).
           03  WRK-UPD-SRYKA           PIC X(02).
           03  WRK-SRYKA-IN            PIC X(02).
           03  WRK-SRYKA-IN2           PIC X(02).
           03  FLG-DELETE              PIC 9(01).
           03  FLG-KARRK-ARI           PIC 9(01).
           03  WRK-SYO-YMD.
               05  WRK-SYO-YM          PIC X(06).
               05  WRK-SYO-DD          PIC X(02).
      *R02.4
           03  WRK-DD              PIC X(02).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
      *    保険別算定履歴テーブル
           COPY   "CMSANTEITBL.INC".
      *H25.7
      *    労災コード置換えテーブル 
           COPY   "CMRSICNGTBL.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK1010.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    診療科履歴
       01  SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    算定履歴
       01  SANTEIPLUS-REC.
           COPY    "CPSANTEIPLUS.INC".
      *
      *R01.10
      *    算定履歴
       01  HZN-SANTEI-REC.
           COPY    "CPSANTEI.INC"      REPLACING
                                     //SANTEI-// BY //HZN-SANTEI-//.
      *
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
      *
           COPY  "CPORCSLNK.INC".
      *   日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *
      *   診療科履歴更新サブ
           COPY    "CPORCSKARRK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
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
           COPY    "ORCA21SCRAREA.INC".
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
           INITIALIZE                  IDX-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-K06
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-K01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-K06         TO  SPA-FREE
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
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "KERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               IF      FLG-END         =   ZERO
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END         =   ZERO
      ***      MOVE    SPACE               TO  LINKAREA
               MOVE    SPACE               TO  LNK-KYOUTU
      *
               MOVE    SPACE               TO  MCP-PUTTYPE
               MOVE    "K06    "           TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
      *
               MOVE    1                   TO  FLG-END
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *
      **** IF      LINKAREA        NOT =   SPACE
           IF      LNK-KYOUTU      NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
               MOVE    SPACE               TO  SPA-KIDCD
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "KID1"
      *            確認画面より
                   PERFORM 30010-KID1-SET-SEC
               WHEN    "K061"
               WHEN    "K062"
      *            履歴作成画面
                   PERFORM 3101-SANTEI-SPA-SEC
                   MOVE    1               TO  SPA-GMN-PAGE
                   MOVE    1               TO  SPA-GMN-CUR
      *
               WHEN    OTHER
                   MOVE    SPACE           TO  SPA-KIDCD
                   PERFORM 310-SPASET-SEC
                   MOVE    1               TO  SPA-GMN-PAGE
                   MOVE    1               TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＫＩＤ１）ＯＫ処理
      *****************************************************************
       30010-KID1-SET-SEC          SECTION.
      *
           EVALUATE    SPA-KIDCD
      *        削除確認
               WHEN    "0101"
                   IF      SPA-KID1-FLG        =   "OK"
      *                削除処理
                       PERFORM 4401-DELETE-SYORI-SEC
      *
                       IF      SPA-ERRCD       NOT =   SPACE
                           PERFORM 510-ERRSET-SEC
                           PERFORM 500-GMNHEN-SEC
                       END-IF
      *
                   ELSE
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
      *        登録確認
               WHEN    "0102"
               WHEN    "0103"
                   IF      SPA-KID1-FLG        =   "OK"
      *                登録処理
                       PERFORM 4501-TOUROKU-SYORI-SEC
      *
                       IF      SPA-ERRCD       NOT =   SPACE
                           PERFORM 510-ERRSET-SEC
                           PERFORM 500-GMNHEN-SEC
                       END-IF
                   ELSE
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
      *        初回算定日確認
               WHEN    "0201"
                   IF      SPA-KID1-FLG        =   "OK"
      *                登録処理
                       MOVE    SPA-NAI-SRYYM       TO  WRK-SRYYM-X
                       MOVE    SPA-GMN-MSANTEID    TO  WRK-SRYDD
                       MOVE    WRK-SRYYMD          TO
                                                   SPA-NAI-FSANTEIYMD
                       MOVE    SPA-NAI-FSANTEIYMD  TO  WRK-SYMD
                       PERFORM 5002-HIZUKE-HEN-SEC
                       MOVE    WRK-HENYMDG     TO  SPA-GMN-FSANTEIYMD
                       MOVE    99              TO  SPA-GMN-CUR
                   ELSE
                       MOVE    6               TO  SPA-GMN-CUR
                   END-IF
           END-EVALUATE
           MOVE    SPACE               TO  SPA-KIDCD
           .
       30010-KID1-SET-EXT.
           EXIT. 
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
      *
           INITIALIZE                  SPA-K06-AREA
      *
           MOVE    SPA-PTNUM           TO  SPA-GMN-PTNUM
           MOVE    SPA-KANANAME        TO  SPA-GMN-KANANAME
           MOVE    SPA-SEX             TO  SPA-GMN-SEX
           MOVE    SPA-NAME            TO  SPA-GMN-NAME
           MOVE    SPA-BIRTHDAYW       TO  SPA-GMN-BIRTHDAY
           MOVE    SPA-SRYYMDW (1:6)   TO  SPA-GMN-SRYYM
           MOVE    SPA-SRYYMD  (1:6)   TO  SPA-NAI-SRYYM
      *
           MOVE    SPA-MOTOPG          TO  SPA-K06-MOTOPG
      *
           PERFORM 3101-SANTEI-SPA-SEC
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴画面編集処理
      *****************************************************************
       3101-SANTEI-SPA-SEC         SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-MAX
                                           SPA-DEL-MAX
           INITIALIZE                      SPA-GMN-TBL
                                           SPA-NAI-TBL
                                           SPA-DEL-TBL
           INITIALIZE                      SPA-GMN-KOUMOKU
           INITIALIZE                      SPA-NAI-KOUMOKU
      *  
      *    算定履歴検索
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    SPA-NAI-SRYYM       TO  SANTEI-SRYYM
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       (IDX            >   100  )  OR
                               (FLG-SANTEI     =   1    )
      *
               MOVE    IDX             TO  SPA-GMN-MAX
      *
               MOVE    IDX               TO  SPA-GMN-TRENNUM (IDX)
               MOVE    SANTEI-SRYCD      TO  SPA-GMN-TSRYCD  (IDX)
               INITIALIZE                    TNS-TENSU-REC
               MOVE    SPA-GMN-TSRYCD (IDX)
                                         TO  TNS-SRYCD
      *R02.4
               IF      SANTEI-MSANTEID   >   ZERO
                   MOVE    SANTEI-MSANTEID   TO  WRK-DD
               ELSE
                   MOVE    "01"              TO  WRK-DD
                   IF     (SANTEI-SRYCD      =   "111013850")
                     AND  (SANTEI-SRYYM      =   "202004"   )
                       MOVE    "10"                 TO  WRK-DD
                   END-IF
               END-IF
      *
               PERFORM 900-TENSU-READ-SEC
               IF      FLG-TENSU         =   ZERO
                   MOVE    TNS-NAME          TO  SPA-GMN-TMEISYO (IDX)
               END-IF
               MOVE    SANTEI-NYUGAIKBN      TO  SPA-NAI-TNYUGAIKBN
                                                                 (IDX)
               EVALUATE    SANTEI-NYUGAIKBN
                   WHEN    "1"
                       MOVE    "入"        TO  SPA-GMN-TNYUGAIKBN(IDX)
                   WHEN    "2"
                       MOVE    "外"        TO  SPA-GMN-TNYUGAIKBN(IDX)
               END-EVALUATE 
               MOVE    SANTEI-SRYKA     TO  SPA-NAI-TSRYKA  (IDX)
               INITIALIZE                   SYS-1005-REC
               MOVE    "1005"           TO  SYS-1005-KANRICD
               MOVE    SANTEI-SRYKA     TO  SYS-1005-KBNCD
               MOVE    SPA-NAI-SRYYM    TO  SYS-1005-STYUKYMD (1:6)
               MOVE    "01"             TO  SYS-1005-STYUKYMD (7:2)
               MOVE    SYS-1005-STYUKYMD
                                        TO  SYS-1005-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
               MOVE    SYS-1005-REC     TO  MCPDATA-REC
               PERFORM 900-SYSKANRI-INV-SEC
               IF      FLG-SYSKANRI         =   ZERO
                   MOVE    MCPDATA-REC          TO  SYS-1005-REC
                   MOVE    SYS-1005-SRYKANAME1  TO  SPA-GMN-TSRYKA(IDX)
               END-IF
               MOVE    SANTEI-MSANTEICNT   TO  SPA-GMN-TMSANTEICNT(IDX)
               MOVE    SANTEI-MSANTEID     TO  SPA-GMN-TMSANTEID  (IDX)
               MOVE    SANTEI-FSANTEIYMD   TO  SPA-NAI-TFSANTEIYMD(IDX)
               IF      SANTEI-FSANTEIYMD   NOT =   SPACE
                   MOVE    SANTEI-FSANTEIYMD   TO  WRK-SYMD
                   IF      SANTEI-FSANTEIYMD(7:2)  =   ZERO
                       MOVE    "01"                TO  WRK-SYMD(7:2)
                   END-IF
                   PERFORM 5002-HIZUKE-HEN-SEC
                   IF      SANTEI-FSANTEIYMD(7:2)  =   ZERO
                       MOVE    ZERO                TO  WRK-HENYMDG(8:2)
                   END-IF
                   MOVE    WRK-HENYMDG         TO  SPA-GMN-TFSANTEIYMD
                                                                  (IDX)
               END-IF
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       31
                   MOVE    SANTEI-DAY (IDY)        TO  SPA-NAI-TDAY
                                                             (IDX IDY)
               END-PERFORM
      *        保険組合せ
               MOVE    SANTEI-HKNCOMBINUM      TO  SPA-NAI-THKNCOMBI
                                                                 (IDX)
               IF      SANTEI-HKNCOMBINUM  NOT =   ZERO
                   MOVE    SANTEI-HKNCOMBINUM  TO  WRK-HKNCOMBI
                   PERFORM 31011-HKNCOMBI-SET-SEC
                   MOVE    WRK-HKNCOMBIMEI     TO  SPA-GMN-THKNCOMBI
                                                                 (IDX)
               END-IF
      *
               MOVE    SANTEI-MSANTEITEN   TO  SPA-GMN-TMSANTEITEN (IDX)
               MOVE    SANTEI-MSANTEIFLG   TO  SPA-NAI-TMSANTEIFLG (IDX)
      *
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           END-PERFORM
      *
      *    カーソルクロース
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    1                   TO  SPA-GMN-CUR 
           .
       3101-SANTEI-SPA-EXT.
           EXIT.
      *****************************************************************
      *    保険組合編集処理
      *****************************************************************
       31011-HKNCOMBI-SET-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNCOMBIMEI
      *
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-PTID            TO  COMB-PTID
           MOVE    WRK-HKNCOMBI        TO  COMB-HKNCOMBINUM
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 940-HKNCOMBI-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    SPACE               TO  WRK-HKNCOMBIMEI
           STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                         INTO              WRK-HKNCOMBIMEI
           END-STRING
      *    主保険がない時
           IF      COMB-SYU-TANSEIDONAME   =   SPACE
               MOVE    SPACE               TO  WRK-HKNCOMBIMEI
               STRING  COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                         INTO              WRK-HKNCOMBIMEI
               END-STRING
           END-IF
      *
           MOVE    COMB-TEKSTYMD       TO  WRK-COMB-TEKSTYMD
           MOVE    COMB-TEKEDYMD       TO  WRK-COMB-TEKEDYMD
      *
      *    労災の時、コメントの追加
           IF      COMB-HKNNUM         =   "971"  OR  "973"
      *        患者保険情報検索
               INITIALIZE                      PTRSIINF-REC
               MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
               MOVE    SPA-PTID            TO  PTRSI-PTID
               MOVE    COMB-HKNID          TO  PTRSI-HKNID
               MOVE    PTRSIINF-REC        TO  MCPDATA-REC
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 993-PTRSIINF-NEXT-SEC
               ELSE
                   INITIALIZE                      PTRSIINF-REC
                   MOVE    1                   TO  FLG-PTRSIINF
               END-IF
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
               MOVE    SPACE               TO  WRK-HKNKBN-MEI
               EVALUATE    PTRSI-HKNKBN
                   WHEN    "1"
                       MOVE    "短"        TO  WRK-HKNKBN-MEI
                   WHEN    "2"
                       MOVE    "傷"        TO  WRK-HKNKBN-MEI
                   WHEN    "3"
                       MOVE    "ア"        TO  WRK-HKNKBN-MEI
                   WHEN    "5"
                       MOVE    "公"        TO  WRK-HKNKBN-MEI
               END-EVALUATE
               IF      PTRSI-COMMENT       =   SPACE
                   MOVE    SPACE               TO  WRK-HKNCOMBIMEI
                   STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                           " "                     DELIMITED  BY  SIZE
                           WRK-HKNKBN-MEI          DELIMITED  BY  SPACE
                           INTO              WRK-HKNCOMBIMEI
                   END-STRING
               ELSE
                   MOVE    SPACE               TO  WRK-HKNCOMBIMEI
                   STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                           " "                     DELIMITED  BY  SIZE
                           WRK-HKNKBN-MEI          DELIMITED  BY  SPACE
                           "（"                    DELIMITED  BY  SIZE
                           PTRSI-COMMENT           DELIMITED  BY  SPACE
                           "）"                    DELIMITED  BY  SIZE
                           INTO              WRK-HKNCOMBIMEI
                   END-STRING
               END-IF
      *        傷病年月日と療養開始日判定
               IF     (PTRSI-SHOBYOYMD     NOT =   SPACE  )  AND
                      (PTRSI-SHOBYOYMD         <   COMB-TEKSTYMD)
                   MOVE    PTRSI-SHOBYOYMD     TO  WRK-COMB-TEKSTYMD
               END-IF
           END-IF
      *ver.4.7
      *    第三者行為
           IF      COMB-KOH1HKNNUM     =   "970"
      *        患者保険情報検索
               INITIALIZE                      PTRSIINF-REC
               MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
               MOVE    SPA-PTID            TO  PTRSI-PTID
               MOVE    ZERO                TO  PTRSI-HKNID
               MOVE    COMB-KOH1ID         TO  PTRSI-KOHID
               MOVE    PTRSIINF-REC        TO  MCPDATA-REC
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 993-PTRSIINF-NEXT-SEC
               ELSE
                   INITIALIZE                      PTRSIINF-REC
                   MOVE    1                   TO  FLG-PTRSIINF
               END-IF
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *        名称編集
               IF      PTRSI-COMMENT   NOT =   SPACE
      *            保険＋公費１のみとなる
                   MOVE    SPACE               TO  WRK-HKNCOMBIMEI
                   STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                           " "                     DELIMITED  BY  SIZE
                           COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                           "（"                    DELIMITED  BY  SIZE
                           PTRSI-COMMENT(1:24)     DELIMITED  BY  SPACE
                           "）"                    DELIMITED  BY  SIZE
                           INTO              WRK-HKNCOMBIMEI
                   END-STRING
      *            保険組合せ名は２０バイト
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "1"                 TO  KANACHK-SYORI
                   MOVE    WRK-HKNCOMBIMEI     TO  KANACHK-MAE-INPUT
                   MOVE    20                  TO  KANACHK-MAX-CNT
                   CALL    "ORCSKANACHK"       USING
                                               ORCSKANACHKAREA
                   MOVE    KANACHK-MAE-INPUT   TO  WRK-HKNCOMBIMEI
               END-IF
      *        傷病年月日と療養開始日判定
               IF     (PTRSI-SHOBYOYMD     NOT =   SPACE  )  AND
                      (PTRSI-SHOBYOYMD         <   COMB-TEKSTYMD)
                   MOVE    PTRSI-SHOBYOYMD     TO  WRK-COMB-TEKSTYMD
               END-IF
          END-IF
      *   保険組合せ名は２０バイト
          INITIALIZE                  ORCSKANACHKAREA
          MOVE    "1"                 TO  KANACHK-SYORI
          MOVE    WRK-HKNCOMBIMEI     TO  KANACHK-MAE-INPUT
          MOVE    20                  TO  KANACHK-MAX-CNT
          CALL    "ORCSKANACHK"       USING
                                          ORCSKANACHKAREA
          MOVE    KANACHK-MAE-INPUT   TO  WRK-HKNCOMBIMEI
           .
       31011-HKNCOMBI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *     戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE    ZERO                TO  FLG-SYORI
                   PERFORM 210-BACK
      *     追加
      *        WHEN    "CLICKED"       ALSO    "B02"
      *            PERFORM 420-TUIKA-SEC
      *    削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 440-SAKUJYO-SEC
      *    変更
      *        WHEN    "CLICKED"       ALSO    "B04"
      *            PERFORM 430-HENKOU-SEC
      *    前月
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 450-MAETUKI-SEC
      *    次月
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 480-ATOTUKI-SEC
      *    登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 450-TOUROKU-SEC
      *!!
      *    クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 450-CLERA-SEC
      *    リハビリコメント履歴登録
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 480-RIHACOMMNT-SEC
      *    履歴登録
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 490-RRKTOUROKU-SEC
      *    移行時登録
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 4100-IKOUTOROKU-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
      *    入力個所のセット
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 41011-RENNUM-HEN-SEC
               WHEN    "ACTIVATE"      ALSO    "SRYYM"
                   PERFORM 41012-SRYYM-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "SRYCD"
                   PERFORM 4102-SRYCD-CHK-SEC
      *        行選択
               WHEN    ANY             ALSO    "SANTEILST"
                   PERFORM 41012-LISTSEL-SEC
               WHEN    OTHER
      *            入力チェック処理
                   MOVE    ZERO                TO  FLG-TOUROKU
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    番号選択　入力処理
      *****************************************************************
       41011-RENNUM-HEN-SEC         SECTION.
      *
           MOVE    1                  TO  SPA-GMN-CUR
      *
           INITIALIZE                 SPA-GMN-KOUMOKU
                                      SPA-NAI-KOUMOKU
      *
           MOVE    K06-SELNUM         TO  SPA-GMN-RENNUM
      *
           IF      SPA-GMN-RENNUM      =   ZERO
               IF      SPA-GMN-MAX         =   100
                   MOVE    "0002"              TO  SPA-ERRCD
               ELSE
                   MOVE    2                   TO  SPA-GMN-CUR 
               END-IF
               GO  TO  41011-RENNUM-HEN-EXT
           END-IF
      *
           IF      SPA-GMN-RENNUM      >   SPA-GMN-MAX
               MOVE    "0001"          TO  SPA-ERRCD
               GO      TO      41011-RENNUM-HEN-EXT
           END-IF
      *
           MOVE    SPA-GMN-TSRYCD (SPA-GMN-RENNUM)
                                       TO  SPA-GMN-SRYCD
           MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-RENNUM)
                                       TO  SPA-GMN-NYUGAIKBN
           MOVE    SPA-NAI-TSRYKA (SPA-GMN-RENNUM)
                                       TO  SPA-GMN-SRYKA
           MOVE    SPA-GMN-TSRYKA (SPA-GMN-RENNUM)
                                       TO  SPA-GMN-SRYKAH
           MOVE    SPA-NAI-THKNCOMBI (SPA-GMN-RENNUM)
                                       TO  SPA-GMN-HKNCOMBI
           MOVE    SPA-GMN-THKNCOMBI (SPA-GMN-RENNUM)
                                       TO  SPA-GMN-HKNCOMBIMEI
           MOVE    SPA-GMN-TMEISYO (SPA-GMN-RENNUM)
                                       TO  SPA-GMN-MEISYO
           MOVE    SPA-GMN-TMSANTEICNT (SPA-GMN-RENNUM)
                                       TO  SPA-GMN-MSANTEICNT
           MOVE    SPA-GMN-TMSANTEID (SPA-GMN-RENNUM)
                                       TO  SPA-GMN-MSANTEID
           MOVE    SPA-GMN-TFSANTEIYMD (SPA-GMN-RENNUM)
                                       TO  SPA-GMN-FSANTEIYMD
           MOVE    SPA-NAI-TFSANTEIYMD (SPA-GMN-RENNUM)
                                       TO  SPA-NAI-FSANTEIYMD
           PERFORM VARYING IDZ     FROM    1   BY  1
                   UNTIL   IDZ     >       31
               MOVE    SPA-NAI-TDAY (SPA-GMN-RENNUM IDZ)
                                           TO  SPA-GMN-DAY(IDZ)
           END-PERFORM
      *
           MOVE    SPA-NAI-TMSANTEIFLG(SPA-GMN-RENNUM)
                                       TO  SPA-NAI-MSANTEIFLG
      *
           PERFORM 41021-SRYCD-HEN-SEC
           MOVE    SPACE               TO  SPA-ERRCD
      *
           MOVE    2                   TO  SPA-GMN-CUR
      *
           .
       41011-RENNUM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       41012-LISTSEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  WRK-RENNUM
           MOVE    ZERO                TO  WRK-RENNUM2
      *
           PERFORM VARYING     IDX   FROM   1   BY  1
                   UNTIL       IDX   >   SPA-GMN-MAX
              IF      K06-SELECT (IDX)        =  "T"
                  IF      IDX                 =   SPA-GMN-RENNUM
                      CONTINUE
                  ELSE
                      MOVE    SPA-GMN-TRENNUM (IDX)    TO  WRK-RENNUM2
                  END-IF
                  MOVE    SPA-GMN-TRENNUM (IDX)    TO  WRK-RENNUM
              END-IF
           END-PERFORM
      *
           IF      WRK-RENNUM2         >   ZERO
               MOVE    WRK-RENNUM2         TO  SPA-GMN-RENNUM
           ELSE
               MOVE    WRK-RENNUM          TO  SPA-GMN-RENNUM
           END-IF
      *
           MOVE    SPA-GMN-RENNUM      TO  K06-SELNUM
           PERFORM 41011-RENNUM-HEN-SEC
      *
           .
       41012-LISTSEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療年月チェック処理
      *****************************************************************
       41012-SRYYM-CHK-SEC           SECTION.
      *
           MOVE    50                TO  SPA-GMN-CUR
      *
           IF      K06-SRYYM            =   SPA-GMN-SRYYM
               CONTINUE
           ELSE
               MOVE   K06-SRYYM          TO  SPA-GMN-SRYYM
               MOVE    SPACE             TO  SPA-NAI-SRYYM
      *
               INITIALIZE                    SPA-GMN-KOUMOKU
                                             SPA-NAI-KOUMOKU
      *
               IF     SPA-GMN-SRYYM       =   SPACE
                   MOVE    "0001"             TO  SPA-ERRCD
               ELSE
                   MOVE    SPA-GMN-SRYYM      TO  WRK-YMD
                   MOVE    "1"                TO  WRK-INTYPE
                   PERFORM 5002-HIZUKE-CHK-SEC
                   IF      SPA-ERRCD            =   SPACE
                       MOVE    WRK-SYMD   (1:6)     TO  SPA-NAI-SRYYM 
                       MOVE    WRK-HENYMDG(1:6)     TO  SPA-GMN-SRYYM
                   END-IF
               END-IF 
      *
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 3101-SANTEI-SPA-SEC
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       41012-SRYYM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療コード  入力処理
      *****************************************************************
       4102-SRYCD-CHK-SEC               SECTION.
      *
           MOVE    2                    TO  SPA-GMN-CUR
      *
           MOVE    K06-SRYCD            TO  SPA-GMN-SRYCD
           MOVE    SPACE                TO  SPA-GMN-MEISYO
           MOVE    ZERO                 TO  SPA-NAI-SANTEIRRKKBN
      *
           IF      SPA-GMN-RENNUM       >   ZERO
               IF      SPA-GMN-SRYCD    NOT =   SPA-GMN-TSRYCD
                                                       (SPA-GMN-RENNUM)
                   MOVE    "0006"               TO  SPA-ERRCD
               END-IF
           END-IF
           IF      SPA-GMN-SRYCD        =   SPACE
               MOVE    "0001"               TO  SPA-ERRCD
           END-IF
           IF      SPA-ERRCD             =   SPACE
               PERFORM 41021-SRYCD-HEN-SEC
               IF      FLG-TENSU            =   ZERO
                   CONTINUE
               ELSE
                   MOVE    "0012"               TO  SPA-ERRCD
               END-IF
           END-IF 
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    3                     TO  SPA-GMN-CUR
           ELSE
               MOVE    2                     TO  SPA-GMN-CUR
           END-IF 
           .
       4102-SRYCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタチェック処理
      *****************************************************************
       41021-SRYCD-HEN-SEC          SECTION.
      *
      *R02.4
           MOVE    "01"                TO  WRK-DD
           IF     (SPA-GMN-SRYCD       =   "111013850")
              AND (SPA-NAI-SRYYM       =   "202004"   )
               MOVE    "10"                 TO  WRK-DD
           END-IF
           INITIALIZE                  TNS-TENSU-REC
           MOVE    SPA-GMN-SRYCD       TO  TNS-SRYCD
           PERFORM 900-TENSU-READ-SEC
           IF      FLG-TENSU           =   ZERO
               MOVE    TNS-NAME            TO  SPA-GMN-MEISYO
               IF      TNS-SANTEIRRKKBN    =   ZERO
                   MOVE    "0011"              TO  SPA-ERRCD
               ELSE
                   MOVE    TNS-SANTEIRRKKBN    TO  SPA-NAI-SANTEIRRKKBN
                   MOVE    TNS-JGNCNT          TO  SPA-NAI-JGNCNT
                   MOVE    TNS-JGNCNT1D        TO  SPA-NAI-JGNCNT1D
               END-IF
           END-IF 
      *    算定履歴付加情報削除
           IF      SPA-GMN-SRYCD       =   "099800111"
                                       OR  "099800121"
                                       OR  "099800131"
                                       OR  "099800141"
                                       OR  "099800151"
                                       OR  "099800161"
                                       OR  "099800171"
                                       OR  "099800181"
                                       OR  "099800191"
                                       OR  "099800211"
                                       OR  "099800221"
                                       OR  "099800231"
                                       OR  "099800241"
                                       OR  "099800291"
               MOVE    1               TO  SPA-NAI-PLUSKBN
           ELSE
               MOVE    ZERO            TO  SPA-NAI-PLUSKBN
           END-IF
      *
      *R01.10
      *    初診料
           IF      SPA-GMN-SRYCD       =   "111000110"
                                       OR  "111700210"
                                       OR  "111003610"
                                       OR  "111700110"
                                       OR  "113003510"
                                       OR  "113003710"
                                       OR  "101110010"
                                       OR  "099110001"
                                       OR  "111012510"
                                       OR  "111012710"
                                       OR  "113019710"
                                       OR  "113019910"
      *R02.4.10 から
      *          初診料（新型コロナウイルス感染症・臨時）
                                       OR  "111013850"
               MOVE    1               TO  SPA-NAI-SYOSIN
           ELSE
               MOVE    ZERO            TO  SPA-NAI-SYOSIN
           END-IF
           .
       41021-SRYCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           MOVE    K06-NYUGAIKBN       TO  SPA-GMN-NYUGAIKBN
           MOVE    K06-SRYKA           TO  SPA-GMN-SRYKA
           MOVE    K06-HKNCOMBI        TO  SPA-GMN-HKNCOMBI
           MOVE    K06-FSANTEIYMD      TO  SPA-GMN-FSANTEIYMD
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       31
               MOVE    K06-DAY (IDX)       TO  SPA-GMN-DAY (IDX)
           END-PERFORM
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェックと別画面処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
      *    入外区分
           PERFORM 4107-NYUGAIKBN-CHK-SEC
      *    診療科
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4108-SRYKA-CHK-SEC
           END-IF
      *    初回算定日
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-FSANTEIYMD-CHK-SEC
           END-IF
      *    保険組合せ
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4109-HKNCOMBI-CHK-SEC
           END-IF
      *    回数チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41013-DAY-CHK-SEC
           END-IF
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分  入力処理
      *****************************************************************
       4107-NYUGAIKBN-CHK-SEC                SECTION.
      *
           EVALUATE    SPA-NAI-SANTEIRRKKBN
               WHEN    1
               WHEN    3
                   IF      SPA-GMN-NYUGAIKBN    =   ZERO
                       CONTINUE
                   ELSE
                       MOVE    "0013"               TO  SPA-ERRCD
                   END-IF
               WHEN    2
               WHEN    4
                   IF      SPA-GMN-NYUGAIKBN    =   1   OR  2
                       CONTINUE
                   ELSE
                       MOVE    "0014"               TO  SPA-ERRCD
                   END-IF
           END-EVALUATE
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    3               TO  SPA-GMN-CUR
           END-IF
      *
           .
       4107-NYUGAIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科  入力処理
      *****************************************************************
       4108-SRYKA-CHK-SEC              SECTION.
      *
           EVALUATE    SPA-NAI-SANTEIRRKKBN
               WHEN    1
               WHEN    2
                   IF      SPA-GMN-SRYKA        =   ZERO
                       CONTINUE
                   ELSE
                       MOVE    "0015"               TO  SPA-ERRCD
                   END-IF
               WHEN    3
               WHEN    4
                   IF      SPA-GMN-SRYKA        =   ZERO
                       MOVE    "0016"               TO  SPA-ERRCD
                   ELSE
                       INITIALIZE                   SYS-1005-REC
                       MOVE    "1005"           TO  SYS-1005-KANRICD
                       MOVE    SPA-GMN-SRYKA    TO  SYS-1005-KBNCD
                       MOVE    SPA-NAI-SRYYM    TO  SYS-1005-STYUKYMD
                                                              (1:6)
                       MOVE    "01"             TO  SYS-1005-STYUKYMD
                                                              (7:2)
                       MOVE    SYS-1005-STYUKYMD
                                                TO  SYS-1005-EDYUKYMD
                       MOVE    SPA-HOSPNUM      TO  SYS-1005-HOSPNUM
                       MOVE    SYS-1005-REC     TO  MCPDATA-REC
                       PERFORM 900-SYSKANRI-INV-SEC
                       IF      FLG-SYSKANRI     NOT =   ZERO
                           MOVE    "0017"               TO  SPA-ERRCD
                       ELSE
                           MOVE    MCPDATA-REC      TO  SYS-1005-REC
                           MOVE    SYS-1005-SRYKANAME1
                                                    TO  SPA-GMN-SRYKAH
                       END-IF
                   END-IF
           END-EVALUATE
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    4               TO  SPA-GMN-CUR
           END-IF
           .
       4108-SRYKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ  入力処理
      *****************************************************************
       4109-HKNCOMBI-CHK-SEC              SECTION.
      *
      *    保険組合せの変更はエラー
           IF     (SPA-ERRCD           =   SPACE )
             AND  (SPA-GMN-RENNUM      >   ZERO  )
               IF      SPA-GMN-HKNCOMBI NOT =   SPA-NAI-THKNCOMBI
                                                       (SPA-GMN-RENNUM)
                   MOVE    "0033"              TO  SPA-ERRCD
                   MOVE    5                   TO  SPA-GMN-CUR
                   GO      TO      4109-HKNCOMBI-CHK-EXT
               END-IF
           END-IF
      *
           IF     (SPA-GMN-HKNCOMBI    =   ZERO  )
           OR     (SPA-GMN-SRYCD       =   SPACE )
               MOVE    SPACE               TO  SPA-GMN-HKNCOMBIMEI
               GO      TO      4109-HKNCOMBI-CHK-EXT
           END-IF
      *
           MOVE    SPACE               TO  SPA-GMN-HKNCOMBIMEI
      *    保険毎のコードの判定
           MOVE    SPA-GMN-SRYCD       TO  WRK-SRYCD
           PERFORM 1000-SANTEI-CHK-SEC
      *    保険組合せがキーにない時、エラー
           IF      FLG-HKNSAN          =   ZERO
               MOVE    "0021"              TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPA-GMN-HKNCOMBI    TO  WRK-HKNCOMBI
               PERFORM 31011-HKNCOMBI-SET-SEC
               IF      FLG-HKNCOMBI        =   ZERO
      *            労災の時、コメントの追加
                   IF     (COMB-HKNNUM         =   "971"
                                               OR  "973"
                                               OR  "975" )
      *                第三者行為
                      OR  (COMB-KOH1HKNNUM     =   "970" )
                       MOVE    WRK-HKNCOMBIMEI TO  SPA-GMN-HKNCOMBIMEI
                   ELSE
                       MOVE    "0023"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    5                   TO  SPA-GMN-CUR
           END-IF
           .
       4109-HKNCOMBI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険毎のコードの判定処理
      *****************************************************************
       1000-SANTEI-CHK-SEC      SECTION.
      *
           SET     TBL-SAN             TO  1
           SEARCH  TBL-SANTEI-OCC      VARYING   TBL-SAN
               AT  END
                   MOVE    ZERO            TO  FLG-HKNSAN
               WHEN    WRK-SRYCD       =   TBL-SANTEI-SRYCD
                                                          (TBL-SAN)
                   MOVE    1               TO  FLG-HKNSAN
      *            Ｈ１８／４月から対応
                   IF      TBL-RIGAKU-RYO (TBL-SAN)    =   "H"
                       IF      SPA-NAI-SRYYM   <   "200604"
                           MOVE    ZERO            TO  FLG-HKNSAN
                       END-IF
                   END-IF
           END-SEARCH
      *    Ｈ２２年４月から追加のコードは固定で判定
      *     (COPY に追加するのは、パッケージ対応で）
           IF      WRK-SRYCD           =   "099800181"
                                       OR  "099800182"
      *H28.4
             OR  (WRK-SRYCD(1:7)       =   "0998002"  )
      *        がん患者リハ開始日・終了日
               MOVE    1               TO  FLG-HKNSAN
           END-IF
      *H25.7
      *    労災保険コード変更
           IF     (WRK-SRYCD(1:3)      =   "101"      )  AND
                  (SPA-NAI-SRYYM       >=  "201307"   )
               PERFORM 10001-RSICD-SANTEI-CHK-SEC
           END-IF
      *
           .
       1000-SANTEI-CHK-EXT.
           EXIT.
      *H25.7
      *****************************************************************
      *    労災コード保険毎のコードの判定処理
      *****************************************************************
       10001-RSICD-SANTEI-CHK-SEC      SECTION.
      *
      *    労災のコードの判定
           SET     TBL-RCN             TO  1
           SEARCH  TBL-RSICHG-OCC      VARYING   TBL-RCN
               AT  END
                   MOVE    SPACE           TO  WRK-RSI-KBN
               WHEN    WRK-SRYCD           =   TBL-RSICHG-ATO-SRYCD
                                                          (TBL-RCN)
                   MOVE    TBL-RSICHG-KBN (TBL-RCN)
                                           TO  WRK-RSI-KBN
           END-SEARCH
      *
           IF      WRK-RSI-KBN         =   "R"
               MOVE    1               TO  FLG-HKNSAN
           ELSE
               IF      WRK-RSI-KBN         =   "A"
                   MOVE    1               TO  FLG-HKNSAN
               ELSE
                   MOVE    ZERO            TO  FLG-HKNSAN
               END-IF
           END-IF
           .
       10001-RSICD-SANTEI-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    初回算定日  入力処理
      *****************************************************************
       41012-FSANTEIYMD-CHK-SEC            SECTION.
      *
           IF     SPA-GMN-FSANTEIYMD       =   SPACE
               MOVE    SPACE                   TO  SPA-NAI-FSANTEIYMD
      *        MOVE    "0018"                  TO  SPA-ERRCD
           ELSE
               MOVE    SPA-GMN-FSANTEIYMD      TO  WRK-YMD
               MOVE    SPACE                   TO  WRK-INTYPE
               PERFORM 5002-HIZUKE-CHK-SEC
               IF      SPA-ERRCD           =   SPACE
                   MOVE    WRK-SYMD            TO  SPA-NAI-FSANTEIYMD
                   MOVE    WRK-HENYMDG         TO  SPA-GMN-FSANTEIYMD
               ELSE
                   MOVE    6                   TO  SPA-GMN-CUR
               END-IF
           END-IF 
      *
           .
       41012-FSANTEIYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療回数テーブル  入力処理
      *****************************************************************
       41013-DAY-CHK-SEC                   SECTION.
      *
      *    内分泌負荷試験は変更できない
           IF     (SPA-NAI-MSANTEIFLG  =   1   )  AND
                  (SPA-GMN-RENNUM      >   ZERO)
               MOVE    SPA-GMN-RENNUM      TO  IDY
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL  (IDX         >       31  )
                           OR (SPA-ERRCD   NOT =   SPACE )
                   IF      SPA-GMN-DAY (IDX)   NOT =   SPA-NAI-TDAY 
                                                            (IDY IDX)
                       MOVE    "0030"              TO  SPA-ERRCD
                       COMPUTE SPA-GMN-CUR         =   IDX
                                                   +   10
                   END-IF
               END-PERFORM
           END-IF
      *
      *    日上限チェック
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL  (IDX         >       31  )
                       OR (SPA-ERRCD   NOT =   SPACE )
               IF     (SPA-NAI-JGNCNT1D        >   ZERO )  AND
                      (SPA-GMN-DAY (IDX)       >   SPA-NAI-JGNCNT1D )
                   IF      SPA-KEIFLG1         =   ZERO
                       MOVE    "K019"                  TO  SPA-ERRCD
                       MOVE    1                       TO  SPA-KEIFLG1
                       COMPUTE SPA-GMN-CUR         =   IDX
                                                   +   10
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    ZERO                    TO  SPA-GMN-MSANTEICNT
               MOVE    ZERO                    TO  SPA-GMN-MSANTEID
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       31
      *            回数
                   ADD     SPA-GMN-DAY (IDX)   TO  SPA-GMN-MSANTEICNT
                   IF     (SPA-GMN-DAY (IDX)       >   ZERO ) AND
                          (SPA-GMN-MSANTEID        =   ZERO )
      *                初回
                       MOVE    IDX                 TO  SPA-GMN-MSANTEID
                       COMPUTE SPA-GMN-CUR         =   IDX
                                                   +   10
                   END-IF
               END-PERFORM
           END-IF
      *    月上限
           IF     (SPA-NAI-JGNCNT      >   ZERO   )
              AND (SPA-ERRCD           =   SPACE  )
               IF      SPA-GMN-MSANTEICNT      >   SPA-NAI-JGNCNT
                   IF      SPA-KEIFLG2         =   ZERO
                       MOVE    "K020"                  TO  SPA-ERRCD
                       MOVE    1                       TO  SPA-KEIFLG2
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    ZERO                TO  SPA-GMN-CUR
           END-IF
           .
       41013-DAY-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    前月分表示　処理
      *****************************************************************
       450-MAETUKI-SEC                    SECTION.
      *
           MOVE    SPA-NAI-SRYYM       TO  WRK-SRYYM
           COMPUTE WRK-SRY-MM          =   WRK-SRY-MM    -   1
           IF      WRK-SRY-MM          =   ZERO
               COMPUTE WRK-SRY-YY      =   WRK-SRY-YY    -   1
               MOVE    12              TO  WRK-SRY-MM
           END-IF
      *    表示用編集
           MOVE    WRK-SRYYM           TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-SRYYM           TO  SPA-NAI-SRYYM
           MOVE    WRK-HENYMDG(1:6)    TO  SPA-GMN-SRYYM
      *
           PERFORM 3101-SANTEI-SPA-SEC
      *
           MOVE    1                   TO  SPA-GMN-CUR
           .
       450-MAETUKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    次月分表示　処理
      *****************************************************************
       480-ATOTUKI-SEC                    SECTION.
      *
           MOVE    SPA-NAI-SRYYM       TO  WRK-SRYYM
           COMPUTE WRK-SRY-MM          =   WRK-SRY-MM    +   1
           IF      WRK-SRY-MM          >   12
               COMPUTE WRK-SRY-YY      =   WRK-SRY-YY    +   1
               MOVE    1               TO  WRK-SRY-MM
           END-IF
      *    表示用編集
           MOVE    WRK-SRYYM           TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-SRYYM           TO  SPA-NAI-SRYYM
           MOVE    WRK-HENYMDG(1:6)    TO  SPA-GMN-SRYYM
      *
           PERFORM 3101-SANTEI-SPA-SEC
      *
           MOVE    1                   TO  SPA-GMN-CUR
           .
       480-ATOTUKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *----(01.00.01) LINE ADD START ----------------------------------
      *    IF      SPA-UPDFLG          =   "1"
      *        MOVE    "0101"          TO  SPA-KIDCD
      *        GO      TO      210-BACK-EXT
      *    END-IF
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
           IF      SPA-K06-MOTOPG      =   "J02"
      *        会計照会へ
               MOVE    LNK-COMMON          TO  SPA-KYOUTU
               MOVE    SPA-K06-MOTOPG      TO  SPA-SAKIPG
               MOVE    "K06"               TO  SPA-MOTOPG
               MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
           ELSE
      *
           IF      SPA-MOTOPG2         =   "KT01"
      *        外来まとめへ
               MOVE    SPA-MOTOPG2         TO  SPA-SAKIPG
               MOVE    SPACE               TO  LINKAREA
               MOVE    "K06"               TO  SPA-MOTOPG
               MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
               MOVE    SPACE               TO  SPA-MOTOPG2
           ELSE
               MOVE    SPA-K06-MOTOPG      TO  SPA-SAKIPG
           END-IF
      *
           END-IF
           MOVE    "K06"               TO  SPA-MOTOPG
      *
           IF      FLG-SYORI           =   ZERO
               MOVE    "CHANGE"            TO  MCP-PUTTYPE
           ELSE
               MOVE    "JOIN"              TO  MCP-PUTTYPE
           END-IF
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    追加　処理
      *****************************************************************
      *420-TUIKA-SEC                SECTION.
      *
      *    IF      SPA-GMN-RENNUM  >   ZERO
      *        MOVE    "0005"              TO  SPA-ERRCD
      *        GO  TO  420-TUIKA-EXT
      *    END-IF
      *
      *    IF      SPA-GMN-MAX      =   100
      *        MOVE    "0002"               TO  SPA-ERRCD
      *        GO      TO      420-TUIKA-EXT
      *    END-IF
      *
      *    IF     (SPA-GMN-KOUMOKU  =   SPACE )  OR
      *           (SPA-GMN-SRYCD    =   SPACE )
      *        MOVE    "0008"               TO  SPA-ERRCD
      *        GO      TO      420-TUIKA-EXT
      *    END-IF
      *
      *    確認
      *    PERFORM 4102-SRYCD-CHK-SEC
      *    IF      SPA-ERRCD        NOT =   SPACE
      *        GO      TO      420-TUIKA-EXT
      *    END-IF
      *
      *----(01.00.01) LINE UPD START ----------------------------------
      *****COMPUTE IDY     =   SPA-GMN-MAX  +   1
      *    COMPUTE IDY-CHK     =   SPA-GMN-MAX  +   1
      *----(01.00.01) LINE UPD END   ----------------------------------
      *    PERFORM 4202-INTBL-CHK-SEC
      *    IF      SPA-ERRCD        NOT =   SPACE
      *       GO  TO  420-TUIKA-EXT
      *    END-IF
      *
      *    ADD     1                TO  SPA-GMN-MAX
      *    MOVE    SPA-GMN-MAX      TO
      *                         SPA-GMN-TRENNUM   (SPA-GMN-MAX)
      *
      *    MOVE    SPA-GMN-MAX          TO  IDX
      *    PERFORM 4201-INTBL-SET-SEC
      *
      *----(01.00.01) LINE ADD START ----------------------------------
      *    MOVE    1                   TO  SPA-NAI-TUPDFLG (IDX)
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
      *    MOVE    1                TO  SPA-GMN-CUR  
      *
      *----(01.00.01) LINE ADD START ----------------------------------
      *    MOVE    "1"              TO  SPA-UPDFLG
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
      *    .
      *420-TUIKA-EXT.
      ***  EXIT.
      *
      *****************************************************************
      *    入力項目重複チェック処理
      *****************************************************************
      *4202-INTBL-CHK-SEC                SECTION.
      *
      *    MOVE    SPACE                   TO  SPA-ERRCD
      *
      *    MOVE    SPA-GMN-CUR             TO  WRK-GMN-CUR
      *    MOVE    "ACTIVATE"              TO  MCP-EVENT
      *    MOVE    2                       TO  SPA-GMN-CUR 
      *    PERFORM UNTIL   SPA-GMN-CUR     =   38
      *            OR      SPA-ERRCD   NOT =   SPACE    
      *        IF      IDX         =   6   OR  7 
      *            CONTINUE
      *        ELSE
      *            PERFORM 5001-MAPCUR-SEC
      *            PERFORM 200-ENTRY
      *        END-IF
      *    END-PERFORM
      *    MOVE    "CLICKED"               TO  MCP-EVENT
      *    IF      SPA-ERRCD           NOT =   SPACE 
      *        GO TO  4202-INTBL-CHK-EXT
      *    END-IF
      * 
      *    MOVE    WRK-GMN-CUR             TO  SPA-GMN-CUR
      *
      *    PERFORM VARYING IDX     FROM    1   BY  1
      *            UNTIL   IDX     >       100
      *            OR      IDX     >       SPA-GMN-MAX
      *----(01.00.01) LINE UPD START ----------------------------------
      *D       IF      IDX         =   IDY 
      *        IF      IDX         =   IDY-CHK
      *----(01.00.01) LINE UPD END   ----------------------------------
      *            CONTINUE
      *        ELSE
      *            IF      SPA-GMN-TSRYCD    (IDX) =  SPA-GMN-SRYCD
      *            AND     SPA-NAI-TNYUGAIKBN(IDX) =  SPA-GMN-NYUGAIKBN
      *            AND     SPA-NAI-TSRYKA    (IDX) =  SPA-GMN-SRYKA
      *            AND     SPA-NAI-THKNCOMBI (IDX) =  SPA-GMN-HKNCOMBI
      *----(01.00.01) LINE UPD START ----------------------------------
      **************** MOVE    "0002"              TO  SPA-ERRCD
      *                MOVE    "0010"              TO  SPA-ERRCD
      *----(01.00.01) LINE UPD END   ----------------------------------
      *                MOVE    100                 TO  IDX
      *            END-IF
      *        END-IF
      *    END-PERFORM
      *
      *    保険の適用期間のチェック
      *    IF      SPA-GMN-HKNCOMBI    NOT =   ZERO
      *        MOVE    SPA-GMN-HKNCOMBI    TO  WRK-HKNCOMBI
      *        PERFORM 31011-HKNCOMBI-SET-SEC
      *        IF      FLG-HKNCOMBI        =   ZERO
      *            MOVE    ZERO                TO  WRK-ST-SRYYMD
      *            MOVE    ZERO                TO  WRK-ED-SRYYMD
      *            MOVE    SPA-NAI-SRYYM       TO  WRK-ST-SRYYMD(1:6)
      *            MOVE    SPA-NAI-SRYYM       TO  WRK-ED-SRYYMD(1:6)
      *            PERFORM VARYING IDXS    FROM    1   BY  1
      *                    UNTIL   IDXS    >       31
      *                IF      SPA-GMN-DAY (IDXS)  >   ZERO
      *                    IF      WRK-STDD        =   ZERO
      *                        MOVE    IDXS        TO  WRK-STDD
      *                    END-IF
      *                    MOVE    IDXS            TO  WRK-EDDD
      *                END-IF
      *            END-PERFORM
      *            IF      WRK-STDD        NOT =   ZERO
      *               IF  ((WRK-ST-SRYYMD  >=  WRK-COMB-TEKSTYMD )  AND
      *                    (WRK-ST-SRYYMD  <=  WRK-COMB-TEKEDYMD )) AND
      *                   ((WRK-ED-SRYYMD  >=  WRK-COMB-TEKSTYMD )  AND
      *                    (WRK-ED-SRYYMD  <=  WRK-COMB-TEKEDYMD ))
      *                  CONTINUE
      *              ELSE
      *                  急性発生日はチェックなしとする
      *                  IF      SPA-GMN-SRYCD       =   "099800101"
      *                     CONTINUE
      *                  ELSE
      *                      MOVE    "0024"              TO  SPA-ERRCD
      *                  END-IF
      *              END-IF
      *            END-IF
      *        END-IF
      *    END-IF
      *    .
      *4202-INTBL-CHK-EXT.
      *    EXIT.
      *
      *****************************************************************
      *    入力項目編集処理
      *****************************************************************
      *4201-INTBL-SET-SEC                  SECTION.
      *
      *    MOVE    SPA-GMN-SRYCD           TO  SPA-GMN-TSRYCD    (IDX)
      *    MOVE    SPA-GMN-NYUGAIKBN       TO  SPA-NAI-TNYUGAIKBN(IDX)
      *    EVALUATE    SPA-GMN-NYUGAIKBN
      *        WHEN    ZERO
      *            MOVE    SPACE           TO  SPA-GMN-TNYUGAIKBN(IDX)
      *        WHEN    "1"
      *                MOVE    "入"        TO  SPA-GMN-TNYUGAIKBN(IDX)
      *        WHEN    "2"
      *                MOVE    "外"        TO  SPA-GMN-TNYUGAIKBN (IDX)
      *    END-EVALUATE
      *    MOVE    SPA-GMN-SRYKA           TO  SPA-NAI-TSRYKA    (IDX)
      *    MOVE    SPA-GMN-SRYKAH          TO  SPA-GMN-TSRYKA    (IDX)
      *    MOVE    SPA-GMN-HKNCOMBI        TO  SPA-NAI-THKNCOMBI (IDX)
      *    MOVE    SPA-GMN-HKNCOMBIMEI     TO  SPA-GMN-THKNCOMBI (IDX)
      *    MOVE    SPA-GMN-MEISYO          TO  SPA-GMN-TMEISYO   (IDX)
      *    MOVE    SPA-GMN-FSANTEIYMD      TO  SPA-GMN-TFSANTEIYMD(IDX)
      *:   MOVE    SPA-NAI-FSANTEIYMD      TO  SPA-NAI-TFSANTEIYMD(IDX)
      *    MOVE    SPA-GMN-MSANTEID        TO  SPA-GMN-TMSANTEID  (IDX)
      *    MOVE    SPA-GMN-MSANTEICNT      TO  SPA-GMN-TMSANTEICNT(IDX)
      *
      *    MOVE    ZERO                TO  WRK-DAY
      *    PERFORM VARYING IDY     FROM    1   BY  1
      *            UNTIL   IDY >   31
      *        MOVE    SPA-GMN-DAY (IDY)   TO  SPA-NAI-TDAY(IDX IDY)
      *        IF      SPA-GMN-DAY (IDY)   >   ZERO
      *            IF      WRK-DAY             =   ZERO
      *                MOVE    IDY             TO  WRK-DAY
      *            END-IF
      *        END-IF
      *    END-PERFORM
      *
      *    MOVE    WRK-DAY             TO  SPA-GMN-TMSANTEID   (IDX)
      *
      *****IF       WRK-DAY            =   ZERO
      *        MOVE    SPACE               TO  SPA-NAI-TFSANTEIYMD(IDX)
      *        MOVE    SPACE               TO  SPA-GMN-TFSANTEIYMD(IDX)
      *    ELSE
      *        MOVE    SPA-NAI-TFSANTEIYMD(IDX)    TO  WRK-SYMD
      ******** IF      SPA-NAI-TFSANTEIYMD (IDX)(7:2)  >  WRK-DAY
      *        IF      WRK-SDD             >  WRK-DAY
      *            MOVE    WRK-DAY         TO  SPA-NAI-TFSANTEIYMD(IDX)
      *                                                           (7:2)
      *            MOVE    SPA-NAI-TFSANTEIYMD(IDX)    TO  WRK-SYMD
      *            PERFORM 5002-HIZUKE-HEN-SEC
      *            MOVE    WRK-HENYMDG         TO  SPA-GMN-TFSANTEIYMD
      *                                                          (IDX)
      *        END-IF
      *****END-IF
      *
      *    EVALUATE   MCP-WIDGET
      *        WHEN    "B02"
      *            MOVE    1               TO  SPA-NAI-TUPDFLG (IDX)
      *        WHEN    "B04"
      *            MOVE    2               TO  SPA-NAI-TUPDFLG (IDX)
      *    END-EVALUATE
      *
      *    INITIALIZE               SPA-GMN-KOUMOKU
      *                             SPA-NAI-KOUMOKU
      *
      *   .
      *4201-INTBL-SET-EXT.
      *    EXIT.
      *
      *****************************************************************
      *    更新　処理
      *****************************************************************
      *430-HENKOU-SEC              SECTION.
      *
      *    IF      SPA-GMN-RENNUM  =   ZERO
      *        MOVE    "0004"              TO  SPA-ERRCD
      *        GO  TO  430-HENKOU-EXT
      *    END-IF
      *    内分泌負荷試験は変更できない
      *    IF      SPA-NAI-MSANTEIFLG  =   1
      *        MOVE    "0030"              TO  SPA-ERRCD
      *        GO  TO  430-HENKOU-EXT
      *    END-IF
      *
      *----(01.00.01) LINE UPD START ----------------------------------
      *****MOVE    SPA-GMN-RENNUM  TO  IDY
      *    MOVE    SPA-GMN-RENNUM  TO  IDY-CHK
      *----(01.00.01) LINE UPD END   ----------------------------------
      *
      *    PERFORM 4202-INTBL-CHK-SEC
      *    IF      SPA-ERRCD        NOT =   SPACE
      *       GO  TO  430-HENKOU-EXT
      *    END-IF
      *
      *    MOVE    SPA-GMN-RENNUM  TO  IDX
      *
      *    PERFORM 4201-INTBL-SET-SEC
      *
      *----(01.00.01) LINE ADD START ----------------------------------
      *    MOVE    2                   TO  SPA-NAI-TUPDFLG (IDX)
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
      *    MOVE    1                   TO  SPA-GMN-CUR 
      *
      *----(01.00.01) LINE ADD START ----------------------------------
      *    MOVE    "1"              TO  SPA-UPDFLG
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
      *   .
      *430-HENKOU-EXT.
      *    EXIT.
      *
      *****************************************************************
      *    削除　処理
      *****************************************************************
       440-SAKUJYO-SEC             SECTION.
      *
           IF      SPA-GMN-RENNUM  =   ZERO
               MOVE    "0004"              TO  SPA-ERRCD
           END-IF
      *    内分泌負荷試験は変更できない
           IF      SPA-NAI-MSANTEIFLG  =   1
               MOVE    "0030"              TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    "0101"              TO  SPA-KIDCD
           END-IF
           .
       440-SAKUJYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除　処理
      *****************************************************************
       4401-DELETE-SYORI-SEC                SECTION.
      *
           MOVE    SPA-GMN-RENNUM      TO  IDX
      *R01.10
      *    初診料の削除前保存
           IF      SPA-NAI-SYOSIN      =   1
               INITIALIZE                      HZN-SANTEI-REC
               INITIALIZE                      SANTEI-KEY
               MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
               MOVE    SPA-PTID            TO  SANTEI-PTID
               MOVE    SPA-NAI-SRYYM       TO  SANTEI-SRYYM
               MOVE    SPA-GMN-TSRYCD (IDX)
                                       TO  SANTEI-SRYCD
               MOVE    SPA-NAI-TNYUGAIKBN (IDX)
                                       TO  SANTEI-NYUGAIKBN
               MOVE    SPA-NAI-TSRYKA(IDX) TO  SANTEI-SRYKA  
               MOVE    SPA-NAI-THKNCOMBI(IDX)
                                       TO  SANTEI-HKNCOMBINUM
               PERFORM 900-SANTEI-INV-SEC
               IF      FLG-SANTEI          =   ZERO
                   MOVE    SANTEI-REC          TO  HZN-SANTEI-REC
               END-IF
           END-IF
      *
      *
           INITIALIZE                      SANTEI-KEY
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    SPA-NAI-SRYYM       TO  SANTEI-SRYYM
           MOVE    SPA-GMN-TSRYCD (IDX)
                                       TO  SANTEI-SRYCD
           MOVE    SPA-NAI-TNYUGAIKBN (IDX)
                                       TO  SANTEI-NYUGAIKBN
           MOVE    SPA-NAI-TSRYKA(IDX) TO  SANTEI-SRYKA  
           MOVE    SPA-NAI-THKNCOMBI(IDX)
                                       TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1002"                  TO  SPA-ERRCD
               DISPLAY "K06 DEL  ERR SANTEI-KEY =>" SANTEI-KEY 
                      ",MCP-RC:" MCP-RC
           END-IF
      *    算定履歴付加情報削除
           IF      SPA-NAI-PLUSKBN     =   1
               INITIALIZE                  SANTEIPLUS-REC
               MOVE    SPA-HOSPNUM         TO  SANTEIPLUS-HOSPNUM
               MOVE    SPA-PTID            TO  SANTEIPLUS-PTID
               MOVE    SPA-NAI-SRYYM       TO  SANTEIPLUS-SRYYM
               MOVE    SPA-GMN-TSRYCD (IDX)
                                           TO  SANTEIPLUS-SRYCD
               MOVE    SPA-NAI-TNYUGAIKBN (IDX)
                                           TO  SANTEIPLUS-NYUGAIKBN
               MOVE    SPA-NAI-TSRYKA(IDX) TO  SANTEIPLUS-SRYKA  
               MOVE    SPA-NAI-THKNCOMBI(IDX)
                                           TO  SANTEIPLUS-HKNCOMBINUM
      *
               MOVE    SANTEIPLUS-REC      TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_santeiplus"    TO  MCP-TABLE
               MOVE    "del2"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              NOT =   ZERO 
                   MOVE    "1002"                  TO  SPA-ERRCD
                   DISPLAY "K06 DEL SANTEIPLUS-KEY =>" SANTEIPLUS-KEY 
                           ",MCP-RC:" MCP-RC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE)
             AND ( SPA-NAI-SYOSIN      =   1    )
      *        初診料の修正の時、診療科履歴の初診算定日修正
               MOVE    1               TO  FLG-DELETE
               PERFORM 4510-KARRK-UPD-SEC
           END-IF
      *
      *    初期表示
           IF      SPA-ERRCD           =   SPACE
               PERFORM 3101-SANTEI-SPA-SEC
           END-IF
           .
       4401-DELETE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録　処理
      *****************************************************************
       450-TOUROKU-SEC                SECTION.
      *
      *    入力チェック処理
           MOVE    1                   TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD           =   SPACE
      *        関連チェック
               PERFORM 4501-KANREI-CHK-SEC
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )   AND
                  (SPA-KIDCD           =   SPACE )
               IF      SPA-GMN-RENNUM      =   ZERO
                   MOVE    "0102"              TO  SPA-KIDCD
               ELSE
                   MOVE    "0103"              TO  SPA-KIDCD
               END-IF
           END-IF
          .
       450-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録前チェック処理
      *****************************************************************
       4501-KANREI-CHK-SEC                SECTION.
      *
           IF     (SPA-GMN-SRYCD       =   SPACE )
               MOVE    "0008"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
           ELSE
      *H28.2
               IF      SPA-GMN-RENNUM       >   ZERO
                   IF      SPA-GMN-SRYCD    NOT =   SPA-GMN-TSRYCD
                                                       (SPA-GMN-RENNUM)
                       MOVE    "0006"              TO  SPA-ERRCD
                       MOVE    1                   TO  SPA-GMN-CUR
                   END-IF
               ELSE
      *            新規は、点数マスタチェック存在チェック
                   PERFORM 41021-SRYCD-HEN-SEC
                   IF      FLG-TENSU       NOT =   ZERO
                       MOVE    "0012"              TO  SPA-ERRCD
                       MOVE    2                   TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )
      *        初回算定日チェック
               IF      SPA-GMN-MSANTEID    >   ZERO
                   MOVE    SPA-NAI-SRYYM       TO  WRK-SRYYM-X
                   MOVE    SPA-GMN-MSANTEID    TO  WRK-SRYDD
                   IF     (SPA-GMN-FSANTEIYMD  =   SPACE  )
                      OR  (SPA-NAI-FSANTEIYMD  >   WRK-SRYYMD )
                       MOVE    "0201"          TO  SPA-KIDCD
                   END-IF
               END-IF
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )
             AND  (SPA-KIDCD           =   SPACE )
      *    新規の登録済み
           IF      SPA-GMN-RENNUM      =   ZERO
               INITIALIZE                      SANTEI-KEY
               MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
               MOVE    SPA-PTID            TO  SANTEI-PTID
               MOVE    SPA-NAI-SRYYM       TO  SANTEI-SRYYM
               MOVE    SPA-GMN-SRYCD       TO  SANTEI-SRYCD
               MOVE    SPA-GMN-NYUGAIKBN   TO  SANTEI-NYUGAIKBN
               MOVE    SPA-GMN-SRYKA       TO  SANTEI-SRYKA  
               MOVE    SPA-GMN-HKNCOMBI    TO  SANTEI-HKNCOMBINUM
               PERFORM 900-SANTEI-INV-SEC
               IF      FLG-SANTEI          =   1
                   CONTINUE
      *            初診は警告
                   IF     (SPA-KEIFLG3         =   ZERO )
                       PERFORM 45012-SYOSIN-CHK-SEC
                   END-IF
               ELSE
                   MOVE    "0010"              TO  SPA-ERRCD
                   MOVE    2                   TO  SPA-GMN-CUR
               END-IF
           ELSE
               IF     (SPA-NAI-PLUSKBN     =   1    )  AND
                      (SPA-KEIFLG3         =   ZERO )
                 AND  (SPA-ERRCD           =   SPACE )
                   MOVE    SPA-GMN-RENNUM      TO  IDX
                   MOVE    ZERO                TO  FLG-SANTEIPLUS-CHK
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL      (IDY     >       31  )
                                  OR  (FLG-SANTEIPLUS-CHK  NOT =   ZERO)
                       IF     (SPA-NAI-TDAY (IDX IDY)  >   ZERO )  AND
                              (SPA-GMN-DAY (IDY)       =   ZERO )
      *                    今回削除の付加情報チェック
                           PERFORM 45011-SANTEIPLUS-CHK-SEC
                       END-IF
                   END-PERFORM
                   IF     FLG-SANTEIPLUS-CHK   =   1
                       MOVE    "K032"          TO  SPA-ERRCD
                       MOVE    99              TO  SPA-GMN-CUR
                   END-IF
                   MOVE    1                   TO  SPA-KEIFLG3
               END-IF
           END-IF
      *
           END-IF
      *
           .
       4501-KANREI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    付加情報チェック処理
      *****************************************************************
       45011-SANTEIPLUS-CHK-SEC                SECTION.
      *
           INITIALIZE                      SANTEIPLUS-REC
           MOVE    SPA-HOSPNUM         TO  SANTEIPLUS-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEIPLUS-PTID
           MOVE    SPA-NAI-SRYYM       TO  SANTEIPLUS-SRYYM
           MOVE    SPA-GMN-TSRYCD(IDX) TO  SANTEIPLUS-SRYCD
           MOVE    SPA-NAI-TNYUGAIKBN (IDX)
                                       TO  SANTEIPLUS-NYUGAIKBN
           MOVE    SPA-NAI-TSRYKA(IDX) TO  SANTEIPLUS-SRYKA  
           MOVE    SPA-NAI-THKNCOMBI(IDX)
                                       TO  SANTEIPLUS-HKNCOMBINUM
           MOVE    IDY                 TO  SANTEIPLUS-DAYKEY
      *
           PERFORM 900-SANTEIPLUS-INV-SEC
           IF      FLG-SANTEIPLUS      =   ZERO
               MOVE    1               TO  FLG-SANTEIPLUS-CHK
           END-IF
           .
       45011-SANTEIPLUS-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    新規の初診料チェック処理
      *****************************************************************
       45012-SYOSIN-CHK-SEC                SECTION.
      *
      *    初診料のみ対象
           IF      SPA-NAI-SYOSIN      =   1
               CONTINUE
           ELSE
               GO      TO      45012-SYOSIN-CHK-EXT
           END-IF
      *
      *    最終来院日
           MOVE    SPACE               TO  SRYKARRK-REC
           INITIALIZE                      SRYKARRK-REC
           MOVE    SPA-HOSPNUM         TO  KARRK-HOSPNUM
           MOVE    SPA-PTID            TO  KARRK-PTID
      *
           MOVE    SRYKARRK-REC        TO  MCPDATA-REC
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_srykarrk"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 975-SRYKARRK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYKARRK
           END-IF
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SRYKARRK        =   ZERO
               MOVE    KARRK-LASTYMD       TO  WRK-LSTYMD
               MOVE    KARRK-SRYKA         TO  WRK-LST-SRYKA
           ELSE
               MOVE    SPACE               TO  WRK-LSTYMD
               MOVE    SPACE               TO  WRK-LST-SRYKA
           END-IF
      *    初診算定チェック
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    "999999"            TO  SANTEI-SRYYM
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key11"             TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
           MOVE    SPACE               TO  WRK-SANTEIYMD
           PERFORM
                   UNTIL       (FLG-SANTEI     =   1    )
      *        初回初診日
               MOVE    SANTEI-SRYYM        TO  WRK-SANYM-X
               MOVE    ZERO                TO  WRK-SANDD
               PERFORM VARYING     IDX2    FROM    31  BY  -1
                       UNTIL      (IDX2    <   1    )
                   IF      SANTEI-DAY (IDX2)   >   ZERO
                       MOVE    IDX2            TO  WRK-SANDD
                       MOVE    1               TO  IDX2
                   END-IF
               END-PERFORM
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key11"             TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           END-PERFORM
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    初診算定日登録あり
           IF     (WRK-SANTEIYMD    NOT =   SPACE   )
            AND   (WRK-SANYM-X         <=  SPA-NAI-SRYYM)
               MOVE    "K035"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-KEIFLG3
           ELSE
               IF     (WRK-LSTYMD  NOT =   SPACE  )
                 AND  (WRK-LSTYMD      <   WRK-SRYYMD )
                   MOVE    "K036"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-KEIFLG3
               END-IF
           END-IF
      *
           IF     (WRK-SANTEIYMD       =   SPACE  )
             AND  (WRK-LSTYMD      NOT =   SPACE  )
               MOVE    WRK-LST-SRYKA       TO  SPA-LST-SRYKA
           END-IF
           .
       45012-SYOSIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録　処理
      *****************************************************************
       4501-TOUROKU-SYORI-SEC                SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           INITIALIZE                      SANTEI-KEY
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    SPA-NAI-SRYYM       TO  SANTEI-SRYYM
           MOVE    SPA-GMN-SRYCD       TO  SANTEI-SRYCD
           MOVE    SPA-GMN-NYUGAIKBN   TO  SANTEI-NYUGAIKBN
           MOVE    SPA-GMN-SRYKA       TO  SANTEI-SRYKA  
           MOVE    SPA-GMN-HKNCOMBI    TO  SANTEI-HKNCOMBINUM
           PERFORM 900-SANTEI-INV-SEC
           IF      FLG-SANTEI          =   ZERO
      *R01.10
              MOVE    SANTEI-REC          TO  HZN-SANTEI-REC
      *
              IF      SPA-GMN-RENNUM       =   ZERO
                   MOVE    "0010"              TO  SPA-ERRCD
                   MOVE    2                   TO  SPA-GMN-CUR
              END-IF
           ELSE
              IF      SPA-GMN-RENNUM       >   ZERO
                   MOVE    "0034"              TO  SPA-ERRCD
                   MOVE    2                   TO  SPA-GMN-CUR
              END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-GMN-RENNUM      =   ZERO
      *            新規算定追加
                   PERFORM 45011-SANTEI-INS-SEC
               ELSE
      *            更新
                   PERFORM 45012-SANTEI-UPD-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE)
             AND ( SPA-NAI-SYOSIN      =   1    )
               IF      SPA-GMN-RENNUM      =   ZERO
                   MOVE    ZERO            TO  FLG-DELETE
               ELSE
                   MOVE    2               TO  FLG-DELETE
               END-IF
      *        初診料の修正の時、診療科履歴の初診算定日修正
               PERFORM 4510-KARRK-UPD-SEC
           END-IF
      *
      *    初期表示
           IF      SPA-ERRCD           =   SPACE
               PERFORM 3101-SANTEI-SPA-SEC
           END-IF
      *
           .
       4501-TOUROKU-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    更新　処理
      *****************************************************************
       45012-SANTEI-UPD-SEC                SECTION.
      *
           MOVE    SPA-GMN-RENNUM      TO  IDX
      *
           INITIALIZE                      SANTEI-KEY
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    SPA-NAI-SRYYM       TO  SANTEI-SRYYM
           MOVE    SPA-GMN-TSRYCD(IDX) TO  SANTEI-SRYCD
           MOVE    SPA-NAI-TNYUGAIKBN (IDX)
                                       TO  SANTEI-NYUGAIKBN
           MOVE    SPA-NAI-TSRYKA(IDX) TO  SANTEI-SRYKA  
           MOVE    SPA-NAI-THKNCOMBI(IDX)
                                       TO  SANTEI-HKNCOMBINUM
           PERFORM 900-SANTEI-INV-SEC
           IF      FLG-SANTEI          =   1
      *        新規算定追加
               PERFORM 45011-SANTEI-INS-SEC
           ELSE
               IF     (SPA-NAI-PLUSKBN     =   1  )
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL       IDY     >       31
                       IF     (SANTEI-DAY  (IDY)   >   ZERO )  AND
                              (SPA-GMN-DAY (IDY)   =   ZERO )
      *                    算定付加情報削除
                           PERFORM 45013-SANTEIPLUS-DEL-SEC
                       END-IF
                   END-PERFORM
               END-IF
      *
               PERFORM 4501-SANTEI-HENSYU-SEC 
               MOVE    SANTEI-REC          TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO 
                   MOVE    "1001"              TO  SPA-ERRCD
                   DISPLAY "UPD SANTEI-KEY =>" SANTEI-KEY 
               END-IF
           END-IF
           .
       45012-SANTEI-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録　処理
      *****************************************************************
       45013-SANTEIPLUS-DEL-SEC                SECTION.
      *
      *    算定履歴付加情報削除
           INITIALIZE                  SANTEIPLUS-REC
           MOVE    SANTEI-HOSPNUM      TO  SANTEIPLUS-HOSPNUM
           MOVE    SANTEI-PTID         TO  SANTEIPLUS-PTID
           MOVE    SANTEI-SRYYM        TO  SANTEIPLUS-SRYYM
           MOVE    SANTEI-SRYCD        TO  SANTEIPLUS-SRYCD
           MOVE    SANTEI-NYUGAIKBN    TO  SANTEIPLUS-NYUGAIKBN
           MOVE    SANTEI-SRYKA        TO  SANTEIPLUS-SRYKA  
           MOVE    SANTEI-HKNCOMBINUM  TO  SANTEIPLUS-HKNCOMBINUM
           MOVE    IDY                 TO  SANTEIPLUS-DAYKEY
      *
           MOVE    SANTEIPLUS-REC      TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       45013-SANTEIPLUS-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録　処理
      *****************************************************************
       45011-SANTEI-INS-SEC                SECTION.
      *
           INITIALIZE                      SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    SPA-NAI-SRYYM       TO  SANTEI-SRYYM
           MOVE    SPA-GMN-SRYCD       TO  SANTEI-SRYCD
           MOVE    SPA-GMN-NYUGAIKBN   TO  SANTEI-NYUGAIKBN
           MOVE    SPA-GMN-SRYKA       TO  SANTEI-SRYKA
           MOVE    SPA-GMN-HKNCOMBI    TO  SANTEI-HKNCOMBINUM
           PERFORM 4501-SANTEI-HENSYU-SEC 
      *
           MOVE    SMCNDATE-YMD        TO  SANTEI-CREYMD
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY "INS SANTEI-KEY =>" SANTEI-KEY 
            END-IF
           .
       45011-SANTEI-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録　処理
      *****************************************************************
       4501-SANTEI-HENSYU-SEC                SECTION.
      *
           MOVE    SPA-NAI-FSANTEIYMD  TO  SANTEI-FSANTEIYMD
           MOVE    SPA-GMN-MSANTEID    TO  SANTEI-MSANTEID
           MOVE    SPA-GMN-MSANTEICNT  TO  SANTEI-MSANTEICNT
           PERFORM VARYING     IDY    FROM    1   BY  1
                   UNTIL       IDY    >       31
               MOVE    SPA-GMN-DAY (IDY)   TO  SANTEI-DAY (IDY)
           END-PERFORM
      *
           MOVE    SMCNDATE-YMD        TO  SANTEI-UPYMD
           MOVE    SMCNDATE-HMS        TO  SANTEI-UPHMS
      *
           .
       4501-SANTEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科履歴の初診算定日修正処理
      *****************************************************************
       4510-KARRK-UPD-SEC             SECTION.
      *
      *R01.10
      *    削除・更新時の診療科取得
           IF      FLG-DELETE          =   1
                                       OR  2
      *        対象の診療科履歴
               PERFORM 45101-SRYKARRK-SET-SEC
           ELSE
               IF      SPA-LST-SRYKA       =   SPACE
                   MOVE    SPA-SRYKA       TO  WRK-UPD-SRYKA
               ELSE
                   MOVE    SPA-LST-SRYKA   TO  WRK-UPD-SRYKA
               END-IF
           END-IF
      *
           INITIALIZE                  ORCSKARRKAREA
           MOVE    SPA-NAI-SRYYM       TO  SKARRK-SRYYM
      *R01.10
           MOVE    WRK-UPD-SRYKA       TO  SKARRK-SRYKA
      *
           CALL    "ORCSKARRK"         USING
                                       ORCSKARRKAREA
                                       SPA-AREA.
      *
           IF      SKARRK-ERRMSG   NOT =   SPACE
               MOVE    "1004"              TO  SPA-ERRCD
           END-IF
           .
       4510-KARRK-UPD-EXT.
           EXIT.
      *
      *R01.10
      *****************************************************************
      *    診療科履歴検索　処理
      *****************************************************************
       45101-SRYKARRK-SET-SEC      SECTION.
      *
      *    更新前の算定日
           MOVE    SPACE               TO  WRK-SYO-YMD
           IF      HZN-SANTEI-SRYYM    NOT =   SPACE
               MOVE    HZN-SANTEI-SRYYM    TO  WRK-SYO-YM
               MOVE    ZERO                TO  WRK-SYO-DD
               PERFORM VARYING     IDXS    FROM    31  BY  -1
                       UNTIL      (IDXS    <   1    )
                   IF      HZN-SANTEI-DAY(IDXS)    >   ZERO
                       MOVE    IDXS            TO  WRK-SYO-DD
                       MOVE    1               TO  IDXS
                   END-IF
               END-PERFORM
           END-IF
      *    診療行為存在
           MOVE    SPACE               TO  WRK-ACCT-SRYKA
      *
           INITIALIZE                  SRYACT-REC
           MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    SPA-PTID            TO  SRY-PTID
           MOVE    SPA-NAI-SRYYM       TO  SRY-SRYYM
           MOVE    SPA-GMN-SRYCD       TO  SRY-SRYCD (1)
           MOVE    SPA-GMN-SRYCD       TO  SRY-SRYCD (2)
           MOVE    SPA-GMN-SRYCD       TO  SRY-SRYCD (3)
           MOVE    SPA-GMN-SRYCD       TO  SRY-SRYCD (4)
           MOVE    SPA-GMN-SRYCD       TO  SRY-SRYCD (5)
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key12"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key12"             TO  MCP-PATHNAME
               PERFORM 920-SRYACT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           IF      FLG-SRYACT          =   ZERO
               PERFORM 451012-SRYACCT-CHK-SEC
           END-IF
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key12"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    SPACE               TO  SRYKARRK-REC
           INITIALIZE                      SRYKARRK-REC
           MOVE    SPA-HOSPNUM         TO  KARRK-HOSPNUM
           MOVE    SPA-PTID            TO  KARRK-PTID
      *
           MOVE    SRYKARRK-REC        TO  MCPDATA-REC
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_srykarrk"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 975-SRYKARRK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYKARRK
           END-IF
           MOVE    SPACE               TO  WRK-SRYKA
           MOVE    SPACE               TO  WRK-SRYKA-IN
           MOVE    ZERO                TO  FLG-KARRK-ARI
           PERFORM
                   UNTIL   FLG-SRYKARRK    =   1
               IF      KARRK-SRYKA         =   SPA-SRYKA
                   MOVE    1                   TO  FLG-KARRK-ARI
               ELSE
                   MOVE    KARRK-SRYKA         TO  WRK-SRYKA
               END-IF
      *        算定履歴のみで診療科履歴のみ
               IF     (KARRK-SYOSINYMD2    =   WRK-SYO-YMD )
                   MOVE    KARRK-SRYKA         TO  WRK-SRYKA-IN2
                   IF     (WRK-ACCT-SRYKA      =   SPACE    )
                       MOVE    KARRK-SRYKA         TO  WRK-SRYKA-IN
                   END-IF
               END-IF
               MOVE    "tbl_srykarrk"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 975-SRYKARRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    SPA-SRYKA           TO  WRK-UPD-SRYKA
      *    初診ダミーの算定日の診療科
           IF     (WRK-ACCT-SRYKA      =   SPACE    )
              AND (WRK-SRYKA-IN    NOT =   SPACE    )
               MOVE    WRK-SRYKA-IN    TO  WRK-UPD-SRYKA
           ELSE
      *        SPA-SRYKA の診療科履歴が存在しない時
               IF     (WRK-SRYKA       NOT =   SPACE )
                 AND  (FLG-KARRK-ARI       =   ZERO  )
                   MOVE    WRK-SRYKA       TO  WRK-UPD-SRYKA
               END-IF
      *        診療会計がある
               IF      WRK-SRYKA-IN2   NOT =   SPACE
                   IF      WRK-SRYKA-IN2   =   WRK-ACCT-SRYKA
                       MOVE    WRK-ACCT-SRYKA  TO  WRK-UPD-SRYKA
                   END-IF
               END-IF
           END-IF
           .
       45101-SRYKARRK-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計検索処理
      *****************************************************************
       451012-SRYACCT-CHK-SEC          SECTION.
      *
           MOVE    SPACE               TO  SRYACCT-REC
           INITIALIZE                      SRYACCT-REC
           MOVE    SRY-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SRY-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SRY-PTID            TO  ACCT-PTID
           MOVE    SRY-SRYKA           TO  ACCT-SRYKA
           MOVE    SRY-SRYYM           TO  ACCT-SRYYM
           MOVE    SRY-SRYKBN          TO  ACCT-SRYKBN
           MOVE    SRY-ZAINUM          TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 940-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SRYACCT         =   ZERO
               MOVE    ACCT-SRYKA          TO  WRK-ACCT-SRYKA
           END-IF
           .
       451012-SRYACCT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア　処理
      *****************************************************************
       450-CLERA-SEC             SECTION.
      *
           IF     (SPA-GMN-RENNUM      >   ZERO )  OR
                  (SPA-GMN-SRYCD   NOT =   SPACE )
               INITIALIZE                 SPA-GMN-KOUMOKU
               INITIALIZE                 SPA-NAI-KOUMOKU
           ELSE
               PERFORM 3101-SANTEI-SPA-SEC
           END-IF
           MOVE    1                   TO  SPA-GMN-CUR
           .
       450-CLERA-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリコメント履歴登録　処理
      *****************************************************************
       480-RIHACOMMNT-SEC             SECTION.
      *
           IF     (SPA-GMN-RENNUM  NOT =   ZERO )
           AND    (SPA-NAI-PLUSKBN     =   1    )
      *        リハビリ開始日で登録済みの時
               PERFORM 4901-K062-SYORI-SEC
           ELSE
               MOVE    "0031"             TO  SPA-ERRCD
           END-IF
           .
       480-RIHACOMMNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリコメント履歴登録　処理
      *****************************************************************
       490-RRKTOUROKU-SEC             SECTION.
      *
      *    初期画面セット
      *****MOVE    SPACE               TO  LINKAREA
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    2                   TO  SPA-K061-GMN
           MOVE    "K06"               TO  SPA-MOTOPG
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-K06             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGK061"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-K06
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           MOVE    "K061"              TO  SPA-SAKIPG
           MOVE    "K06"               TO  SPA-MOTOPG
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
      **** MOVE    SPACE               TO  LINKAREA
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "K061"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       490-RRKTOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリコメント履歴登録　処理
      *****************************************************************
       4901-K062-SYORI-SEC             SECTION.
      *
           INITIALIZE                     SPA-K062-GMN-G
           MOVE    SPA-GMN-SRYCD       TO  SPA-K062-SRYCD
           MOVE    SPA-NAI-SRYYM       TO  SPA-K062-SRYYM
           MOVE    SPA-NAI-THKNCOMBI (SPA-GMN-RENNUM)
                                       TO  SPA-K062-HKNCOMBI
           MOVE    "K06"               TO  SPA-K062-MOTOPG
           MOVE    SPACE               TO  SPA-MOTOPG
      *    初期画面セット
      **** MOVE    SPACE               TO  LINKAREA
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-K06             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGK062"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-K06
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           MOVE    "K062"              TO  SPA-SAKIPG
           MOVE    "K06"               TO  SPA-MOTOPG
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
      **** MOVE    SPACE               TO  LINKAREA
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "K062"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       4901-K062-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    移行時登録　処理
      *****************************************************************
       4100-IKOUTOROKU-SEC             SECTION.
      *
      *    初期画面セット
      *****MOVE    SPACE               TO  LINKAREA
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    1                   TO  SPA-K061-GMN
           MOVE    "K06"               TO  SPA-MOTOPG
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-K06             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGK061"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-K06
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           MOVE    "K061"              TO  SPA-SAKIPG
           MOVE    "K06"               TO  SPA-MOTOPG
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
      *****MOVE    SPACE               TO  LINKAREA
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "K061"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       4100-IKOUTOROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-INTYPE          TO  SGDAY-INTYPE
           MOVE    WRK-YMD             TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY          TO  WRK-SYMD
           ELSE
               MOVE    "0001"          TO  SPA-ERRCD
           END-IF
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
      *----(01.00.01) LINE ADD START ----------------------------------
           IF      SPA-KIDCD       NOT =   SPACE
               PERFORM 520-JIDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
      **** MOVE    SPACE               TO  LINKAREA
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "K06    "           TO  MCP-WINDOW.
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
           MOVE    SPACE               TO  K06
      *
           INITIALIZE                      K06
           MOVE    SPA-GMN-PTNUM       TO  K06-PTNUM
           MOVE    SPA-GMN-SRYYM       TO  K06-SRYYM
           MOVE    SPA-GMN-KANANAME    TO  K06-KANANAME
           MOVE    SPA-GMN-NAME        TO  K06-NAME
           MOVE    SPA-GMN-SEX         TO  K06-SEX
           MOVE    SPA-GMN-BIRTHDAY    TO  K06-BIRTHDAY
      *
           MOVE    SPA-NENREI-HEN      TO  K06-NENREI
      *    排他中
           IF      SPA-LOCK                =   ZERO
               MOVE    "black"             TO  K06-KANANAME-STYLE
               MOVE    "black"             TO  K06-NAME-STYLE
           ELSE
               MOVE    "red"               TO  K06-KANANAME-STYLE
               MOVE    "red"               TO  K06-NAME-STYLE
           END-IF
      *
           MOVE    ZERO                TO  K06-COUNT
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX                 >   100
                   OR          SPA-GMN-TRENNUM (IDX)=   ZERO  
               MOVE    SPA-GMN-TRENNUM (IDX)   TO  WRK-ZZ
               MOVE    WRK-ZZ                  TO  K06-TRENNUM    (IDX)
               MOVE    SPA-GMN-TSRYCD (IDX)    TO  K06-TSRYCD     (IDX)
               MOVE    SPA-GMN-TNYUGAIKBN (IDX)
                                               TO  K06-TNYUGAIKBN (IDX)
               MOVE    SPA-GMN-TSRYKA  (IDX)   TO  K06-TSRYKA     (IDX)
               MOVE    SPA-GMN-THKNCOMBI(IDX)  TO  K06-THKNCOMBI  (IDX)
               MOVE    SPA-GMN-TMEISYO (IDX)   TO  K06-TMEISYO    (IDX)
               MOVE    SPA-GMN-TFSANTEIYMD (IDX)
                                               TO  K06-TFSANTEIYMD(IDX)
               MOVE    SPA-GMN-TMSANTEICNT (IDX)
                                               TO  K06-TMSANTEICNT(IDX)
               MOVE    SPA-GMN-TMSANTEID   (IDX)
                                               TO  K06-TMSANTEID  (IDX)
               MOVE    SPA-GMN-TMSANTEITEN (IDX)
                                               TO  WRK-Z9
               MOVE    WRK-Z9                  TO  K06-TMSANTEITEN(IDX)
      *
               MOVE    IDX                     TO  K06-COUNT
           END-PERFORM
      *
           MOVE    SPA-GMN-RENNUM         TO  K06-SELNUM
           MOVE    SPA-GMN-SRYCD          TO  K06-SRYCD
           MOVE    SPA-GMN-NYUGAIKBN      TO  K06-NYUGAIKBN
           MOVE    SPA-GMN-SRYKA          TO  K06-SRYKA
           MOVE    SPA-GMN-MEISYO         TO  K06-MEISYO
           MOVE    SPA-GMN-MSANTEICNT     TO  K06-MSANTEICNT
           MOVE    SPA-GMN-MSANTEID       TO  K06-MSANTEID
           MOVE    SPA-GMN-FSANTEIYMD     TO  K06-FSANTEIYMD
           MOVE    SPA-GMN-HKNCOMBI       TO  K06-HKNCOMBI
           MOVE    SPA-GMN-HKNCOMBIMEI    TO  K06-HKNCOMBIMEI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       31
               MOVE    SPA-GMN-DAY(IDX)   TO  K06-DAY(IDX)
           END-PERFORM
      *
           MOVE    SPA-GMN-SRYKAH         TO  K06-SRYKAMEI
      *
      *    排他中
           IF      SPA-LOCK            =   ZERO
               MOVE    WIDGET-NORMAL       TO  K06-B03-STATE
               MOVE    WIDGET-NORMAL       TO  K06-B08-STATE
               MOVE    WIDGET-NORMAL       TO  K06-B09-STATE
               MOVE    WIDGET-NORMAL       TO  K06-B10-STATE
               MOVE    WIDGET-NORMAL       TO  K06-B12-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  K06-B03-STATE
               MOVE    WIDGET-INSENSITIVE  TO  K06-B08-STATE
               MOVE    WIDGET-INSENSITIVE  TO  K06-B09-STATE
               MOVE    WIDGET-INSENSITIVE  TO  K06-B10-STATE
               MOVE    WIDGET-INSENSITIVE  TO  K06-B12-STATE
           END-IF
      *
           PERFORM 5001-MAPCUR-SEC
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
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
             WHEN    01
                    MOVE  "SELNUM"     TO  MCP-WIDGET
             WHEN    50
                    MOVE  "SRYYM"      TO  MCP-WIDGET
             WHEN    02
                    MOVE  "SRYCD"      TO  MCP-WIDGET
             WHEN    03
                    MOVE  "NYUGAIKBN"  TO  MCP-WIDGET
             WHEN    04
                    MOVE  "SRYKA"      TO  MCP-WIDGET
             WHEN    05
                    MOVE  "HKNCOMBI"   TO  MCP-WIDGET
             WHEN    06
                    MOVE  "FSANTEIYMD" TO  MCP-WIDGET
             WHEN    11  THRU  41
                    MOVE  "DAY"        TO  WRK-WIDGET1
                    COMPUTE IDZ =   SPA-GMN-CUR   -   10
                    MOVE    IDZ        TO  WRK-WIDGET2
                    MOVE    WRK-WIDGET TO  MCP-WIDGET
             WHEN   90
                    MOVE  "B01"        TO  MCP-WIDGET   
             WHEN    91
                    MOVE  "B02"        TO  MCP-WIDGET   
             WHEN    92
                    MOVE  "B03"        TO  MCP-WIDGET   
             WHEN    93
                    MOVE  "B04"        TO  MCP-WIDGET   
             WHEN    94
                    MOVE  "B10"        TO  MCP-WIDGET   
             WHEN    99
                    MOVE  "B12"        TO  MCP-WIDGET   
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       50011-INPUT-CUR-SEC          SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "SRYCD"
                   MOVE    03          TO  SPA-GMN-CUR
               WHEN    "NYUGAIKBN"
                   MOVE    04          TO  SPA-GMN-CUR
               WHEN    "SRYKA"
                   MOVE    05          TO  SPA-GMN-CUR
               WHEN    "HKNCOMBI"
                   MOVE    06          TO  SPA-GMN-CUR
               WHEN    "FSANTEIYMD"
                   MOVE    11          TO  SPA-GMN-CUR
               WHEN    OTHER
                   IF      WRK-MCP-WIDGET(1:3)     =   "DAY"
                       MOVE    WRK-MCP-WIDGET      TO  WRK-WIDGET
                       COMPUTE SPA-GMN-CUR         =   WRK-WIDGET2
                                                   +   11
                       IF      SPA-GMN-CUR         >   41
                           MOVE    99          TO  SPA-GMN-CUR
                       END-IF
                   END-IF
           END-EVALUATE
           .
       50011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "入力エラーです"            TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "追加できません"            TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "警告メッセージ"            TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "対象のデータを選択してありません"
                                                       TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "対象のデータが選択してあります"
                                                       TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "診療コードは変更できません。"
                                                       TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "項目に入力がありません"    TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "入力の途中です"            TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "同じキーが存在します。登録できません"
                                                       TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "算定履歴の算定できない診療です。"
                                                   TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "点数マスタに存在しません。"
                                                   TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "入外区分の入力できない診療です。"
                                                   TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "入外区分を入力して下さい。"
                                                   TO  SPA-ERRMSG
               WHEN    "0015"
                   MOVE    "診療科の入力できない診療です。"
                                                   TO  SPA-ERRMSG
               WHEN    "0016"
                   MOVE    "診療科を入力して下さい。"
                                                   TO  SPA-ERRMSG
               WHEN    "0017"
                   MOVE    "診療科が存在しません。"
                                                   TO  SPA-ERRMSG
               WHEN    "0018"
                   MOVE    "算定日を入力して下さい。"
                                                   TO  SPA-ERRMSG
               WHEN    "K019"
                   MOVE    "警告！１日の算定回数を超えています。"
                                                   TO  SPA-ERRMSG
               WHEN    "K020"
                   MOVE    "警告！１月の算定回数を超えています。"
                                                   TO  SPA-ERRMSG
               WHEN    "0021"
                   MOVE    "保険組合せは入力できません。"
                                                   TO  SPA-ERRMSG
               WHEN    "0022"
                   MOVE    "保険組合せがありません。"
                                                   TO  SPA-ERRMSG
               WHEN    "0023"
                   MOVE    "保険組合せが健保です。入力できません。"
                                                   TO  SPA-ERRMSG
               WHEN    "0024"
                   STRING  "保険組合せの適用期間外です。"
                           "算定日を変更して下さい。"
                           DELIMITED   BY  SIZE        INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0030"
                   MOVE    "内分泌負荷試験検査は変更できません。"
                                                       TO  SPA-ERRMSG
               WHEN    "0031"
                   STRING  "リハビリテーション開始日を選択してから"
                           "コメント入力を行って下さい。"
                           DELIMITED   BY  SIZE        INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "K032"
                   STRING  "警告！"
                           "リハビリコメント登録日が削除されました。"
                           "リハビリコメントを同時に削除します。"
                           DELIMITED   BY  SIZE        INTO  SPA-ERRMSG
                   END-STRING
      *H25.9
               WHEN    "0033"
                   STRING  "保険組合せの変更はできません。"
                           DELIMITED   BY  SIZE        INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0034"
                   MOVE    "算定履歴が存在しません。更新できません。"
                                                       TO  SPA-ERRMSG
      *H28.2
               WHEN    "K035"
                   STRING  "警告！"
                           "既に初診料の登録があります。"
                           "診療行為で登録して下さい。"
                           DELIMITED   BY  SIZE        INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "K036"
                   STRING  "警告！"
                           "初診料算定日≦最終来院日で登録して下さい。"
                           DELIMITED   BY  SIZE        INTO  SPA-ERRMSG
                   END-STRING
      *H25.9
               WHEN    "1001"
                   MOVE    "更新処理でエラーになりました"
                                                   TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "削除処理でエラーになりました"
                                                   TO  SPA-ERRMSG
               WHEN    "1003"
                   MOVE    "マスタが存在しません" 
                                                   TO  SPA-ERRMSG
               WHEN    "1004"
                   MOVE    "診療科履歴更新エラー" 
                                                   TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  KERR
           MOVE    SPA-ERRCD            TO  KERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  KERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "K06"                TO  SPA-MOTOPG
           MOVE    "KERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "KERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END 
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *----(01.00.01) LINE ADD START ----------------------------------
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-JIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-KIDMSG
           EVALUATE    SPA-KIDCD
               WHEN    "0101"
                   STRING  "算定履歴を削除します。"
                           "元に戻すことはできません。"
                           "よろしいですか？"
                           DELIMITED   BY  SIZE        INTO  WRK-KIDMSG
                   END-STRING
               WHEN    "0102"
                   STRING  "算定履歴を登録します。"
                           "よろしいですか？"
                           DELIMITED   BY  SIZE        INTO  WRK-KIDMSG
                   END-STRING
               WHEN    "0103"
                   STRING  "算定履歴を変更します。"
                           "元に戻すことはできません。"
                           "よろしいですか？"
                           DELIMITED   BY  SIZE        INTO  WRK-KIDMSG
                   END-STRING
               WHEN    "0201"
                   STRING   "初回算定日を初回日に変更します。"
                           "よろしいですか？"
                           DELIMITED   BY  SIZE        INTO  WRK-KIDMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPA-KIDCD       TO  WRK-KIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-KID1-FLG
      *
           MOVE    SPACE               TO  KID1
           INITIALIZE                      KID1
           MOVE    SPA-KIDCD           TO  KID1-ID1CODE
           MOVE    WRK-KIDMSG          TO  KID1-ID1MSG
      *
           MOVE    "戻る"              TO  KID1-B01
           MOVE    "ＯＫ"              TO  KID1-B12
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "K06"               TO  SPA-MOTOPG
           MOVE    "KID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "KID1"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-JIDSET-EXT.
           EXIT.
      *----(01.00.01) LINE ADD END   ----------------------------------
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
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       900-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-NAI-SRYYM       TO  TNS-YUKOSTYMD (1:6)
           MOVE    WRK-DD              TO  TNS-YUKOSTYMD (7:2)
      **** MOVE    "01"                TO  TNS-YUKOSTYMD (7:2)
           MOVE    SPA-NAI-SRYYM       TO  TNS-YUKOEDYMD (1:6)
      **** MOVE    "01"                TO  TNS-YUKOEDYMD (7:2)
           MOVE    WRK-DD              TO  TNS-YUKOEDYMD (7:2)
           MOVE    TNS-YUKOSTYMD       TO  WRK-YUKOSTYMD
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
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *!
      *    検査正式名称
           IF      SPA-FORMALFLG       =   "1"
               IF     (TNS-SRYCD(1:1)      =   "1") AND
                      (TNS-SRYKBN          =   "60")  AND
                      (TNS-FORMALNAME  NOT =   SPACE )  AND
                      (TNS-FORMALNAME  NOT =   TNS-NAME )
                   MOVE    TNS-FORMALNAME  TO  TNS-NAME
               END-IF
           END-IF
      *    附加情報の上限回数設定
           IF      FLG-TENSU           =   ZERO
               PERFORM 9101-TENSUPLUS-SYORI-SEC
           END-IF
      *
           .
       900-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ付加情報より編集処理
      *****************************************************************
       9101-TENSUPLUS-SYORI-SEC          SECTION.
      *
           MOVE    SPACE               TO  TENSUPLUS-REC
           INITIALIZE                      TENSUPLUS-REC
           MOVE    TNS-SRYCD           TO  TNSP-SRYCD
           MOVE    WRK-YUKOSTYMD       TO  TNSP-YUKOSTYMD
           MOVE    WRK-YUKOSTYMD       TO  TNSP-YUKOEDYMD
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
      *
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
      *
               MOVE    TNSP-JGNCNTETCM     TO  TNS-JGNCNTETCM
               MOVE    TNSP-JGNCNTETC      TO  TNS-JGNCNTETC
           END-IF
      *
           .
       9101-TENSUPLUS-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴マスター読込
      *****************************************************************
       900-SANTEI-INV-SEC              SECTION.
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
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SANTEI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定歴マスタ読み込み
      *****************************************************************
       920-SANTEI-READ-SEC         SECTION.
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
      *
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
      *    保険組合せマスター読込
      *****************************************************************
       940-HKNCOMBI-NEXT-SEC         SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
      *
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
      *
      *****************************************************************
      *    患者労災保険情報読み込み
      *****************************************************************
       993-PTRSIINF-NEXT-SEC        SECTION.
      *
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTRSIINF-REC
               MOVE    ZERO            TO  FLG-PTRSIINF
           ELSE
               INITIALIZE                  PTRSIINF-REC
               MOVE    1               TO  FLG-PTRSIINF
           END-IF
      *
           .
       993-PTRSIINF-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    算定履歴マスター読込
      *****************************************************************
       900-SANTEIPLUS-INV-SEC              SECTION.
      *
           MOVE    SANTEIPLUS-REC      TO  MCPDATA-REC
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santeiplus"    TO  MCP-TABLE
               MOVE    "key2"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SANTEIPLUS-REC
                   MOVE    ZERO            TO  FLG-SANTEIPLUS
               ELSE
                   INITIALIZE                  SANTEIPLUS-REC
                   MOVE    1               TO  FLG-SANTEIPLUS
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SANTEIPLUS
           END-IF
      *
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SANTEIPLUS-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科歴データ主キー検索
      *****************************************************************
       975-SRYKARRK-READ-SEC           SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYKARRK-REC
               MOVE    ZERO                TO  FLG-SRYKARRK
           ELSE
               MOVE    1                   TO  FLG-SRYKARRK
               INITIALIZE                  SRYKARRK-REC
           END-IF
      *
           .
       975-SRYKARRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計読み込み処理
      *****************************************************************
       940-SRYACCT-READ-SEC         SECTION.
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
       940-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計読み込み処理
      *****************************************************************
       920-SRYACT-READ-SEC         SECTION.
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
       920-SRYACT-READ-EXT.
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
      *
