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
       PROGRAM-ID.                 ORCGJ02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 会計照会
      *  コンポーネント名  : 会計カード入力（Ｊ０２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-多々納   01.04.17  戻る時、確認画面を表示する等
      *  01.00.02    MCC-多々納   01/05/17  年齢計算、老人判定追加
      *  01.00.03    MCC-多々納   02/04/25  労災追加
      *  01.00.04    MCC-多々納   02/06/05  患者氏名での検索を追加
      *  01.00.05    NACL-多々納  02/07/29  パラメタファイルをＤＢへ
      *  01.00.06    NACL-多々納  02/09/11  自賠責追加
      *  01.00.07    NACL-多々納  02/10/25  入院追加
      *  01.00.08    NACL-多々納  02/12/09  剤削除修正
      *  01.00.09    NACL-多々納  03/03/05  排他制御処理追加
      *  01.00.10    NACL-多々納  03/04/04  中心静脈自動算定追加
      *  01.00.11    NACL-多々納  03/05/12  前次頁対応、診療日変更修正
      *  01.00.12    NACL-多々納  03/08/28  入院の受診履歴追加修正
      *  01.00.13    NACL-多々納  04/01/09  保険毎表示追加修正
      *  01.00.14    NACL-多々納  04/05/09  収納の会計変更区分追加
      *                                     受診履歴更新修正
      *  01.00.15    NACL-多々納  04/07/XX  外来まとめ追加
      *                                     一括入力項目追加
      *  01.00.16    NACL-多々納  05/04/14  レセプレビュー追加
      *  01.00.17    NACL-多々納  05/04/20  診療会計附加情報追加
      *  01.00.18    NACL-多々納  05/08/19  診区順追加
      *  01.00.19    NACL-多々納  05/11/24  MONFUNC 対応
      *  01.00.20    NACL-多々納  06/01/20  診療科初期表示追加
      *  01.00.21    NACL-多々納  06/01/23  用量割合・包括検査数追加
      *  01.00.22    NACL-多々納  06/05/14  コメント入力画面変更
      *  03.04.00    NACL-多々納  06/11/13  検査正式名称表示追加
      *  03.04.01    NACL-多々納  07/02/28  包括診療追加対応
      *  03.05.00    NACL-多々納  07/05/XX  グループ診療対応
      *  03.05.00    NACL-多々納  07/06/13  レセプレビューに労災・自賠追加
      *                                     入院の受診履歴削除追加
      *                                     一括入力内容修正
      *  04.01.00    NACL-多々納  07/10/22  公務災害追加
      *  04.01.00    NACL-多々納  07/11/27  公害追加
      *  04.02.00    NACL-多々納  08/03/XX  Ｈ２０年４月改正
      *  04.03.00    NACL-多々納  08/08/01  保険組合せ期間チェック処理追加
      *  04.04.00    NACL-多々納  08/12/24  入院料包括対応
      *  04.05.00    NACL-多々納  09/06/XX  数量小数５桁対応
      *  04.06.00    NACL-多々納  10/09/06  自費保険入院中入力対応
      *  04.07.00    NACL-多々納  11/08/XX  算定履歴付加情報削除対応
      *  04.07.00    NACL-多々納  11/09/XX  院外処方点数表示対応
      *  04.07.00    NACL-多々納  11/10/XX  同日再入院対応
      *  04.08.00    NACL-多々納  13/01/15  診療回数テーブル１０対応
      *  04.07.00    NACL-多々納  13/02/06  レセ電データ移行対応
      *  04.07.00    NACL-多々納  13/04/XX  労災レセ電コード変換対応
      *  04.07.00    NACL-多々納  13/10/XX  第三者行為対応
      *  04.08.00    NACL-多々納  14/05/XX  一時データ変更対応
      *  04.08.00    NACL-多々納  16/03/XX  Ｈ２８年４月改正対応
      *  04.08.00    NACL-多々納  16/05/XX  一般名表示対応
      *  04.08.00    NACL-多々納  16/06/XX  中心静脈（植入型カテーテル）対応
      *  04.08.00    NACL-多々納  16/08/XX  持参薬対応
      *  04.08.00    NACL-多々納  16/10/XX  一括変更項目桁数対応
      *  04.08.00    NACL-多々納  18/03/XX  Ｈ３０年０４月改正対応
      *  05.00.00    NACL-多々納  18/12/27  HAORI入院調剤料対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    パラメタデータ
      *****SELECT  PARA-FILE       ASSIGN  "/var/tmp/J01PARA"
      *    SELECT  PARA-FILE       ASSIGN  FILE-NAME2
      *                            ORGANIZATION    IS  INDEXED
      *                            ACCESS  MODE    IS  DYNAMIC
      *                            RECORD  KEY     IS  PARA-KEY
      *******                      FILE    STATUS  IS  STS-PARA.
      *
      *    ＳＰＡデータ
      *    SELECT  SPASCR-FILE     ASSIGN  FILE-NAME1
      *                            ORGANIZATION    IS  SEQUENTIAL
      *                            FILE    STATUS  IS  STS-SPASCR.
      *
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    ＳＰＡパラメタデータ
      *FD  SPASCR-FILE.
      *01  SPA-DATA-REC            PIC X(30000).
      *
      *    パラメタデータ
      *FD  PARA-FILE.
      *01  PARA-REC.
      *    03  PARA-KEY.
      *********05  PARA-TERMID          PIC X(64).
      *        05  PARA-FILEMEI         PIC X(08).
      *        05  PARA-EDANUM          PIC 9(03).
      *    03  PARA-DATA-REC            PIC X(3500).
      *
      *
       WORKING-STORAGE             SECTION.
      * ＳＰＡパラメタデータ
      *01  FILE-NAME1.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(09)   VALUE   "J02SPASCR".
      *    03  FILE-HOSPNUM        PIC 9(02)   VALUE   ZERO.
      *    03  FILE-TERMID         PIC X(64)   VALUE   SPACE.
      *    03  FILLER              PIC X(06)   VALUE   ".TXT".
      *
      *01  FILE-NAME2.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(07)   VALUE   "J01PARA".
      *    03  FILE-HOSPNUM2       PIC 9(02)   VALUE   ZERO.
      *    03  FILE-TERMID2        PIC X(64)   VALUE   SPACE.
      *
      *01  WRK-DATA-REC.
      *    03  WRK-DATA            PIC X(30000)
      ***                          OCCURS  20.
       01  SPA-SCR-REC.
           COPY    "CPJ02SPASCR.INC".
      *
      *    画面コントロール
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    会計照会共通パラメタ
           COPY    "J01COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "J02SCR-SPA".
      *
      *    フラグ領域
      *01  STS-AREA.
      *    03  STS-PARA            PIC X(02).
      *    03  STS-SPASCR          PIC X(02).
      *    03  STS-SRYKBN          PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-END2            PIC 9(01).
           03  FLG-SPASCR          PIC 9(01).
           03  FLG-SRYKBN          PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-TENSUPLUS       PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-INPUTSET        PIC 9(01).
           03  FLG-INPUTCD         PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-SYUMEI          PIC 9(01).
           03  FLG-PTNYUINRRK      PIC 9(01).
           03  FLG-SYUTTL          PIC 9(01).
           03  FLG-SANTEIPLUS      PIC 9(01).
           03  FLG-SPATMP          PIC 9(01).
      *
           03  FLG-NAIYAK          PIC X(01).
           03  FLG-TONPUK          PIC X(01).
           03  FLG-GAIYAK          PIC X(01).
           03  FLG-SP              PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-OK2             PIC 9(01).
           03  FLG-SYORI           PIC 9(01).
           03  FLG-HENSYU          PIC 9(01).
           03  FLG-DOUJI           PIC 9(01).
      *
           03  FLG-YAKUSOKU        PIC 9(01).
      *
           03  FLG-UPD             PIC 9(01).
      *    老人フラグ
           03  FLG-ROUJIN          PIC 9(01).
      *    編集フラグ
           03  FLG-HEN-OK          PIC 9(01).
      *    受診履歴変更フラグ
           03  FLG-JYURRK-UPD      PIC 9(01).
      *    受診履歴エラー
           03  FLG-JYURRK-ERR      PIC 9(01).
      *
           03  FLG-ACCT-ERR        PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-JYURRK-ARI      PIC 9(01).
      *
           03  FLG-K1              PIC 9(01).
           03  FLG-K2              PIC 9(01).
           03  FLG-IKKATUFLG       PIC 9(01).
      *
           03  FLG-SYUNOU-KOUSIN   PIC 9(01).
           03  FLG-HEN             PIC 9(01).
           03  FLG-HEN2            PIC 9(01).
      *
           03  FLG-NAIBUN          PIC 9(01).
      *
           03  FLG-HKN-OK          PIC 9(01).
           03  FLG-KOGHKN-OK       PIC 9(01).
           03  FLG-RSIHKN-OK       PIC 9(01).
           03  FLG-RSIHKN-OK2      PIC 9(01).
           03  FLG-RRKNUM-ARI      PIC 9(01).
           03  FLG-DAISAN-OK       PIC 9(01).
      *!!
      *
           03  FLG-HKN-CHK         PIC 9(01).
           03  FLG-DEKIDAKA        PIC 9(01).
      *
           03  FLG-RSIGAIRAI       PIC 9(01).
           03  FLG-NYUGAICHG       PIC 9(01).
      *H24.5
           03  FLG-JYURRK-OK       PIC 9(01).
           03  FLG-SANTEIDATA      PIC 9(01).
      *
           03  FLG-HAORI           PIC 9(01).
      *H30.11
           03  FLG-INSARI          PIC 9(01).
      *
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
           03  CNT-ENT             PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDG                 PIC 9(04).
           03  IDG3                PIC 9(04).
           03  IDH                 PIC 9(04).
           03  IDK                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDS                 PIC 9(04).
           03  IDX-L               PIC 9(04).
           03  IDK1                PIC 9(04).
           03  IDK2                PIC 9(04).
           03  IDM                 PIC 9(04).
           03  IDXG                PIC 9(04).
           03  IDX-H               PIC 9(04).
           03  IDX-D               PIC 9(02).
      *    漢字変換用
           03  IDJ                 PIC 9(04).
           03  IDJ2                PIC 9(04).
           03  IDJ3                PIC 9(04).
      *
           03  IDX-S               PIC 9(04).
      *
           03  IDX-R               PIC 9(04).
           03  IDR                 PIC 9(04).
           03  IDR2                PIC 9(02).
           03  IDR3                PIC 9(04).
           03  IDD3                PIC 9(02).
      *
           03  IDR4                PIC 9(04).
           03  IDR5                PIC 9(04).
           03  IDR6                PIC 9(04).
           03  IDX-RRK2            PIC 9(04).
           03  IDX-RRK3            PIC 9(04).
      *    受診履歴更新用
           03  IDX-JYU             PIC 9(04).
           03  IDX-JYU2            PIC 9(04).
      *
      *    入院点滴用
           03  IDX-TEN             PIC 9(04).
           03  IDX-TEN1            PIC 9(04).
           03  IDX-TEN2            PIC 9(04).
           03  IDX-TEN3            PIC 9(04).
           03  IDX-TENT1           PIC 9(04).
           03  IDX-TENT3           PIC 9(04).
           03  IDX-REN             PIC 9(04).
           03  IDX-REN2            PIC 9(04).
           03  IDX-DEL             PIC 9(04).
      *
           03  IDX-J               PIC 9(04).
      *
           03  IDX-Z               PIC 9(04).
           03  IDX-IN              PIC 9(02).
           03  IDX-IN2             PIC 9(04).
      *
           03  IDX3                PIC 9(04).
           03  IDX-T               PIC 9(04).
           03  IDT                 PIC 9(04).
           03  IDT2                PIC 9(04).
           03  IDX-SYU             PIC 9(04).
      *
           03  IDX-J025            PIC 9(04).
      *
           03  IDX-C               PIC 9(04).
      *
           03  IDXD                PIC 9(04).
      *!!!!!
           03  IDX-GEN             PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-PARA-EDANUM         PIC 9(03).
      *
           03  WRK-SYSYMD.
               05  WRK-SYSYY       PIC 9(04).
               05  WRK-SYSMM       PIC 9(02).
               05  WRK-SYSDD       PIC 9(02).
           03  WRK-SRYYMD.
               05  WRK-SRYYY       PIC 9(04).
               05  WRK-SRYMM       PIC 9(02).
               05  WRK-SRYDD       PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-GMN-SRYYM       PIC X(06).
           03  WRK-NAI-SRYYM       PIC X(06).
           03  WRK-ENDDD           PIC 9(02).
     ****  03  WRK-DD              PIC 9(02).
           03  WRK-YMD.
               05  WRK-YM          PIC X(06).
               05  WRK-DD          PIC 9(02).
      *
           03  WRK-SYM-1.
               05  WRK-SYY-1       PIC 9(04).
               05  WRK-SMM-1       PIC 9(02).
           03  WRK-SYM-2.
               05  WRK-SYY-2       PIC 9(04).
               05  WRK-SMM-2       PIC 9(02).
      *    連番
           03  WRK-BANGO           PIC 9(04).
      *    年齢計算用
           03  WRK-YMDS1.
               05  WRK-YYS1        PIC 9(04).
               05  WRK-MMS1        PIC 9(02).
               05  WRK-DDS1        PIC 9(02).
           03  WRK-YMDS2.
               05  WRK-YYS2        PIC 9(04).
               05  WRK-MMS2        PIC 9(02).
               05  WRK-DDS2        PIC 9(02).
      *
           03  WRK-TAIINYMD        PIC X(08).
      *
           03  WRK-TENSU-X.
               05  WRK-TENSU       PIC Z(07).
               05  WRK-TENSU-2     REDEFINES   WRK-TENSU
                                   PIC -(07).
           03  WRK-KAISU-X.
               05  WRK-KAISU       PIC Z(08).
           03  WRK-KEI-X.
               05  WRK-KEI         PIC Z(07).
      *ver.4.7
           03  WRK-TENSU-GX.
               05  WRK-TENSU-G1    PIC X(01).
               05  WRK-TENSU-G     PIC Z(05).
               05  WRK-TENSU-G2    PIC X(01).
           03  WRK-TENSU-GX-R      REDEFINES    WRK-TENSU-GX.
               05  WRK-TENSU-GR    PIC Z(06).
               05  WRK-TENSU-GR2   PIC X(01).
      *
           03  WRK-MEISYO          PIC X(80).
      *    数量表示用
           03  WRK-SURYO           PIC 9(05)V9(05).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(05).
               05  WRK-SURYO-S2    PIC 9(05).
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1    PIC ZZZZZ.
               05  WRK-SURYO-Z19   REDEFINES   WRK-SURYO-Z1
                                   PIC ZZZZ9.
               05  WRK-SURYO-Z2    PIC X(01).
               05  WRK-SURYO-Z3    PIC ZZZZZ.
               05  WRK-SURYO-Z3-X  REDEFINES   WRK-SURYO-Z3
                                   PIC X(05).
           03  WRK-SURYO-J         PIC X(20).
      *    分画数画面編集用
           03  WRK-BUNGSU-X.
               05  WRK-BUNGSU-Z1   PIC ZZZ.
           03  WRK-BUNGSU-X2.
               05  WRK-BUNGSU-Z2   PIC ZZZ.
           03  WRK-BUNGSU-J        PIC X(06).
      *    剤回数等画面編集用
           03  WRK-KAISU-H         PIC X(08).
           03  WRK-KEI-H           PIC X(08).
           03  WRK-ZAIKEI          PIC X(40).
           03  WRK-ZAIKEIJ         PIC X(80).
           03  WRK-IDG             PIC 9(04).
      * 
           03  WRK-ERRCD            PIC X(04).
           03  WRK-HZN-ERRCD            PIC X(04).
           03  WRK-ERRCD02          PIC X(04).
      *    保険組合せ名称
           03  WRK-HKNCOMBI         PIC X(04).
           03  WRK-HKNCOMBIMEI      PIC X(100).
           03  WRK-HKNKBN-MEI       PIC X(02).
      *
           03  WRK-ZAIKAISU        PIC 9(07).
      *
           03  WRK-HOUKNS-CNT          PIC 9(02).
           03  WRK-HOUKNS-IDX          PIC 9(04).
           03  WRK-HOUKNS-NAME         PIC X(100).
      *
      *    診療行為名称
           03  WRK-SRYSYUMEI       PIC X(40).
           03  WRK-SRYSYUMEI2      PIC X(40).
           03  WRK-SRYKAMEI        PIC X(12).
           03  WRK-TEKEDYMD        PIC X(08).
      *
           03  WRK-ZZZZ-X.
               05  WRK-ZZZZ            PIC ZZZ.
      *
           03  WRK-ZZ-X.
               05  WRK-ZZ              PIC ZZ.
           03  WRK-ZZZ-X.
               05  WRK-ZZZ             PIC ZZZ.
      *
           03  WRK-Z9-X.
               05  WRK-Z9              PIC Z9.
           03  WRK-ZZ9-X.
               05  WRK-ZZ9             PIC ZZ9.
      *
           03  WRK-HENNYMD             PIC X(22).
      *
           03  WRK-IDX                 PIC 9(04).
      *
      *    算定履歴更新用
           03  WRK-MSANTEICNT          PIC  9(03).
           03  WRK-MSANTEID            PIC  9(02).
           03  WRK-SANCNT              PIC S9(03).
           03  WRK-SANTEI              PIC S9(03).
           03  IDX-SAN                 PIC  9(04).
      *    行選択用
           03  WRK-SELNUM              PIC 9(03).
           03  WRK-SELNUM2             PIC 9(03).
      *
      *    受診履歴変更用
           03  WRK-RRKYMD              PIC X(08).
           03  WRK-RENNUM              PIC 9(02).
           03  WRK-MAE-RENNUM          PIC 9(02).
           03  WRK-DOUJI-RENNUM        PIC 9(01).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-RRK-SRYKA           PIC X(02).
           03  WRK-MAE-HKNCOMBINUM     PIC X(04).
           03  WRK-DAY-S               PIC 9(03).
           03  WRK-DAY-MAE             PIC 9(03).
           03  IDX-D1                  PIC 9(02).
           03  IDX-D2                  PIC 9(02).
           03  IDX-D3                  PIC 9(04).
           03  IDX-D4                  PIC 9(04).
      *    履歴剤変更用
           03  WRK-RRKZAI-HEN.
               05  WRK-IDR             PIC 9(04).
               05  WRK-IDR-MAX         PIC 9(04).
           03  IDX-RRK                 PIC 9(04).
           03  IDX-N                   PIC 9(04).
      *
           03  WRK-STDD                PIC 9(02).
           03  WRK-EDDD                PIC 9(02).
      *
           03  WRK-HKNNUM              PIC X(03).
           03  WRK-RSI-HKNKBN          PIC X(01).
      *
           03  WRK-TBANGO              PIC  9(04).
      *
           03  WRK-MOTOPG              PIC X(08).
      *    老人年齢
           03  WRK-ROU-NEN             PIC 9(02).
      *
           03  WRK-ERRMSG              PIC X(80).
           03  WRK-JIDMSG.
               05  WRK-JIDMSG1              PIC X(40).
               05  WRK-JIDMSG2              PIC X(60).
      *
      *    診療コード
           03  WRK-SRYCD              PIC X(09).
      *    発生年月日
           03  WRK-HASSYOYMD.
               05  WRK-HAS-YY         PIC 9(04).
               05  WRK-HAS-MM         PIC 9(02).
               05  WRK-HAS-DD         PIC 9(02).
      *
           03  WRK-ERRFLG             PIC X(01).
           03  WRK-ERRFLG2            PIC X(01).
      *
      *
           03  WRK-ORC-DBPATH          PIC X(64).
      *
           03  WRK-MCP-PATH.
               04  WRK-MCP-BLOCKS  PIC S9(9)   BINARY.
               04  WRK-MCP-RNAME   PIC S9(9)   BINARY.
               04  WRK-MCP-PNAME   PIC S9(9)   BINARY.
      *
      *    クリア用保存
           03  WRK-NYUGAIKBN-G         PIC X(10).
           03  WRK-SRYKBN-G            PIC X(19).
           03  WRK-HKNCOMBI-G          PIC X(55).
      *    入院エラー
           03  WRK-NYUIN-ERR           PIC X(200).
           03  IDX-N2                  PIC 9(02).
      *    入院エラー２
           03  WRK-NYUIN-ERR2           PIC X(200).
           03  IDX-N22                  PIC 9(02).
      *    受診履歴追加用
           03  WRK-RRK-HKNKBN          PIC X(01).
           03  WRK-RRK-RENNUM          PIC 9(01).
           03  WRK-RRK-DOUJI-RENNUM    PIC 9(01).
           03  WRK-RRK-DRCD            PIC X(05).
           03  WRK-GRP-DENPNUM         PIC 9(07).
      *
           03  WRK-DENPNUM             PIC 9(07).
           03  WRK-KA-DENPNUM          PIC 9(07).
      *
           03  WRK-ACCT-UPDKBN         PIC X(01).
      *    点滴算定エラー
           03  WRK-ERR-TBL.
               05  WRK-ERR-DAY         PIC 9(01)   OCCURS  31.
      *
      *    カルテ３号紙プログラム名
           03  WRK-PRINT-PG            PIC X(20).
      *
      *    コラムリスト位置
           03  WRK-J02-DATALIST-ROW    PIC S9(09).
           03  WRK-J02-RRKLIST-ROW     PIC S9(09).
           03  WRK-J02-DATALIST-ROWATTR    PIC S9(09).
           03  WRK-J02-RRKLIST-ROWATTR     PIC S9(09).
           03  FLG-GMN-INIT            PIC 9(01).
           03  MAE-J02-DATALIST-ROW    PIC S9(09).
           03  WRK-MAE-ZAINUM          PIC 9(08).
      *
      *    剤変更チェック用
           03  TBL-MAE-ZAINUM-AREA.
               05  TBL-MAE-ZAINUM      PIC 9(08)
                                       OCCURS   135.
      *
      *    一括回数エラー
           03  WRK-SPA-GMN-DAY-G.
               05  WRK-SPA-GMN-DAY     PIC 9(03)    OCCURS   31.
      *    回数保存
           03  WRK-REN                 PIC 9(02).
      *    履歴回数
           03  WRK-CNT-DAY             PIC 9(02).
      *    回数
           03  WRK-CNT-TUKI            PIC 9(03).
           03  WRK-DAYCNT              PIC 9(03).
      *    労災読み替え判定
           03  WRK-RSAI-GAIRAITEN      PIC 9(08).
      *    プレビュー用
           03  WRK-RSI-CNT             PIC 9(03).
           03  WRK-JIB-CNT             PIC 9(03).
           03  WRK-NCNT-R              PIC 9(03).
           03  WRK-NCNT-J              PIC 9(03).
           03  WRK-NCNT                PIC 9(03).
           03  WRK-KOG-CNT             PIC 9(03).
           03  WRK-NCNT-G              PIC 9(03).
      *入院料包括
           03  WRK-HKTSRY-AREA.
               05  WRK-HKTCHKMSG           PIC X(100).
               05  WRK-HKTCHKMSG-H         PIC X(100).
               05  WRK-ERRDAY-G.
                   07  WRK-ERRDAY          PIC 9(02)   OCCURS   31.
               05  IDD                     PIC 9(02).
      *
      *    週上限
           03  WRK-WEEK-CHEK-G.
               05  WRK-WEEKCNT             PIC 9(03).
               05  WRK-YOBI                PIC 9(01).
               05  WRK-CHK-SRYYMD.
                   07  WRK-CHK-SRYYM       PIC X(06).
                   07  WRK-CHK-SRYDD       PIC 9(02).
               05  WRK-STR-YMD.
                   07  WRK-STR-YM          PIC X(06).
                   07  WRK-STR-DD          PIC 9(02).
               05  WRK-END-YMD.
                   07  WRK-END-YM          PIC X(06).
                   07  WRK-END-DD          PIC 9(02).
               05  IDX-ST                  PIC 9(02).
               05  IDX-ED                  PIC 9(02).
      *ver.4.7
      *    同日再入院
           03  WRK-D-RENNUM            PIC 9(01).
           03  WRK-D-IDX               PIC 9(03).
           03  WRK-IDX-P               PIC 9(03).
      *    レセプトプレビュー業務区分
           03  WRK-GYOUMUKBN           PIC X(01).
      *
      *
           03  WRK-OLD-NYUINYMD        PIC X(08).
           03  WRK-OLD-TAIINYMD        PIC X(08).
      *H27.1
           03  WRK-CCNT                PIC 9(03).
      *
      *
       01  TBL-GRP-AREA.
           03  TBL-GRP-OCC             OCCURS   10.
               05  TBL-SRYKA           PIC X(02).
               05  TBL-HKNCOMBI        PIC X(04)    OCCURS   10.
               05  TBL-DENPNUM         PIC 9(07).
      *
      *    回数一括変更用
       01  WRK-DAY-AREA.
           03  WRK-DAY-TBL.
               05  WRK-DAY-OCC         OCCURS   31.
                   07  WRK-DAY         PIC 9(03).
                   07  WRK-DAYDEL      PIC 9(01).
           03  WRK-IKKATU-TBL.
               05  WRK-IKKATU-OCC          OCCURS   99.
                   07  WRK-KAISUT          PIC X(99).
                   07  WRK-DAYG            OCCURS   99.
                       09  WRK-DAY1            PIC X(99).
                       09  WRK-DAY2            PIC X(99).
           03  WRK-KAISU-9              PIC 9(03).
           03  WRK-DAYG-9               OCCURS   99.
               05  WRK-DAY1-9               PIC 9(02).
               05  WRK-DAY2-9               PIC 9(02).
           03  WRK-IKKATUDAY            PIC 9(02).
           03  WRK-IKKATUKAI            PIC 9(03).
           03  IDW1                     PIC 9(04).
           03  IDW2                     PIC 9(04).
           03  IDW3                     PIC 9(04).
           03  IDW4                     PIC 9(04).
           03  IDW5                     PIC 9(04).
           03  IDW9                     PIC 9(04).
           03  WRK-HZNSU              PIC X(99).
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
      *    診療種別名テーブル
           COPY    "CPORCSSRYSYU.INC".
      *
      *    保険別算定履歴テーブル
           COPY    "CMSANTEITBL.INC".
      *!!
      *H25.7
      *    労災コード置換えテーブル 
           COPY   "CMRSICNGTBL.INC".
      *H28.4
      * 湿布薬（減）
       01  CONS-SPCM-SRYCD         PIC X(09)   VALUE    "820000047".
      *
      *
      *
      *    受診履歴
       01  WRK-JYURRK-REC.
           COPY    "CPJYURRK.INC"     REPLACING
                                     //JYURRK-// BY //WRK-JYURRK-//.
      *    受診履歴
       01  HZN-JYURRK-REC.
           COPY    "CPJYURRK.INC"     REPLACING
                                     //JYURRK-// BY //HZN-JYURRK-//.
      *    受診履歴
       01  TBL-JYURRK-AREA.
           02  TBL-JYURRK-REC         OCCURS  100.
           COPY    "CPJYURRK.INC"     REPLACING
                                     //JYURRK-// BY //TBL-JYURRK-//.
      *
      *    スパ領域 ワーク
           COPY    "COMMON-SPA"      REPLACING
                                     //SPA-// BY //WRK-SPA-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1001.INC".
      *
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK0041.INC".
      *    会計照会機能情報
           COPY    "CPSK1043.INC".
      * 
           COPY    "CPSK1041.INC".
      *
           COPY    "CPSK1030.INC".
      *
           COPY    "CPSK1038.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *    患者番号変換
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    算定履歴付加情報
       01  SANTEIPLUS-REC.
           COPY    "CPSANTEIPLUS.INC".
      *
      *    入力セットコード
       01  INPUTSET-REC.
           COPY    "CPINPUTSET.INC".
      *
      *    入力コードマスタ−
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    収納マスタ
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *    収納マスタ
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    収納合計マスタ−
       01  SYUTTL-REC.
           COPY    "CPSYUTOTAL.INC".
      *
      *    診療会計附加情報マスタ−
       01  SRYACCTPLUS-REC.
           COPY    "CPSRYACCTPLUS.INC".
      *
      *     パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
      *    ＳＰＡ一時テーブル
       01  SPATMP-REC.
           COPY    "CPSPA-TMP.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *    保存用収納ワーク
       01  TBL-SYUNOU-AREA.
         02  TBL-SYUNOU-REC        OCCURS   10.
           COPY    "CPSYUNOU.INC"  REPLACING
                                     //SYU-// BY //TBL-SYU-//.
      *    保存用収納ワーク
       01  WRK-SYUNOU-REC.
           COPY    "CPSYUNOU.INC"  REPLACING
                                     //SYU-// BY //WRK-SYU-//.
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
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *   診療科算定履歴更新サブ
           COPY    "CPORCSKARRK.INC".
      *
      *   算定履歴更新サブ
           COPY    "CPORCSSANTEI.INC".
      *
      *    保険算定チェックサブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    入院点滴一括更新サブ
           COPY    "CPORCSTENTEK.INC".
      *
      *    入院調剤料更新サブ
           COPY    "CPORCSCHOZAI.INC".
      *
      *    入院料包括診療チェクパラメタ
           COPY    "CPORCSCHKTALL.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    カルテ３号様式出力パラメタ
           COPY    "CPORCHC33.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    日報キー取得サブ
           COPY    "CPORCSDAILYKEY.INC".
      *
      *    保険組合せセット領域
           COPY    "CPORCSHKNSET.INC".
      *
      *    収納一括作成領域
           COPY    "CPORCSSAIKEISAN.INC".
      *    表示名称編集パラメタ領域
           COPY    "CPORCSMEIHEN.INC".
      *
      *    内分泌負荷試験更新パラメタ
           COPY    "CPORCSSANFUKA.INC".
      *
      *    主情報アクセス用パラメタ
           01  ORCSSYUACCAREA.
               COPY    "CPORCSSYUACC.INC".
      *    収納更新差額取得パラメタ
           COPY    "CPORCSS002.INC".
      *
      *    ＳＰＡパラメタ（ORCSC97.CBL 用のDAMMY)
       01  LNK-K02SCR-REC.
           COPY    "CPK02SPASCR.INC"    REPLACING
                                        //SPA-// BY //LNK-SPA-//.
      *    プレビュー用情報
           COPY    "CPXC01LNK.INC".
      *
       01  WRK-PREV-AREA.
           03  WRK-CONS-PRTKANRI-TBL-KEY
                                   PIC X(08)   VALUE   "RECEPTX".
      *
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *
      *TEST
           COPY    "MCPDATA2.INC".
      *****COPY    "CPORCMCP.INC".
      *
      *****COPY    "ORCA-DBPATH".
      *
      *    院外処方一般名編集パラメタ
           COPY    "CPORCSGETGENERIC.INC".
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
           COPY    "ORCA24SCRAREA.INC".
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
      **** INITIALIZE                  STS-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  TBL-GRP-AREA
           INITIALIZE                  WRK-DAY-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-J02
           MOVE    SPA-WORK        TO  SPA-J01KYOUTU
      *
      *    診療種別テーブルセット
           PERFORM 1001-SRYKBN-SET-SEC
      *
      *    画面ＳＰＡ読み込み
           PERFORM 1002-J02SPA-SET-SEC
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
      *HAORI対応
               WHEN    "API"           ALSO    ANY
                   PERFORM 300-HAORI-SYORI-SEC
      *            画面表示なし
                   MOVE    1                   TO  FLG-END
      *
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
      *    画面ＳＰＡ書込み
      *    IF      FLG-END2            =   ZERO
      *        PERFORM 1003-J02SPA-OUT-SEC
      *    END-IF
      *
           MOVE    SPA-J01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-J02         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    診療種別テーブルセット処理
      *****************************************************************
       1001-SRYKBN-SET-SEC             SECTION.
      *
      *    診療種別テーブルセット
           CALL    "ORCSSRYSYU"        USING
                                       MSYU-DATA-REC
           .
       1001-SRYKBN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＳＰＡデータセット処理
      *****************************************************************
      *
       1002-J02SPA-SET-SEC             SECTION.
      *
           INITIALIZE                      SPA-SCR-REC
           MOVE    ZERO                TO  FLG-SPASCR
           MOVE    ZERO                TO  IDX
      *
      *    tbl_spa_tmp を使用する
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "J02"               TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPAGMN"            TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
      *
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-SPATMP-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-SPATMP-READ-SEC
               IF      FLG-SPATMP          =   ZERO
                   MOVE    SPATMP-DATA-REC     TO   SPA-SCR-REC
               END-IF
           ELSE
               INITIALIZE                  SPATMP-REC
               MOVE    1               TO  FLG-SPATMP
           END-IF
           MOVE    "key"              TO  MCP-PATHNAME
           PERFORM 990-SPATMP-DBCLOSE-SEC
      *
      **** MOVE    SPA-TERMID          TO  FILE-TERMID
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM
      *
      *    OPEN    INPUT               SPASCR-FILE
      *    IF      STS-SPASCR     NOT =   ZERO
      *        GO      TO         1002-J02SPA-SET-EXT
      *    END-IF
      *
      *    INITIALIZE          SPA-SCR-REC
      *    INITIALIZE          WRK-DATA-REC
      *    MOVE    ZERO                TO  FLG-SPASCR
      *    MOVE    ZERO                TO  IDX
      *    PERFORM UNTIL       FLG-SPASCR  =   1
      *        READ    SPASCR-FILE
      *            AT  END
      *                MOVE   1            TO  FLG-SPASCR
      *            NOT AT  END
      *                ADD    1            TO  IDX
      *                MOVE   SPA-DATA-REC     TO  WRK-DATA (IDX)
      *        END-READ
      *    END-PERFORM
      *
      *    MOVE    WRK-DATA-REC        TO  SPA-SCR-REC
      *
      *****CLOSE                       SPASCR-FILE
           .
       1002-J02SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    ＳＰＡデータＷＲＩＴＥ処理
      *****************************************************************
       1003-J02SPA-OUT-SEC             SECTION.
      *
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "J02"               TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPAGMN"            TO  SPATMP-FILEMEI
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del1"              TO  MCP-PATHNAME
           PERFORM 910-SPATMP-CALL-SEC
      *
      *    SPA-SCR-REC保存
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "J02"               TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPAGMN"            TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
           MOVE    SPA-SCR-REC         TO  SPATMP-DATA-REC
           PERFORM 10031-SPATMP-INSERT-SEC
      *
      *
      **   MOVE    SPA-TERMID          TO  FILE-TERMID
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM
      *
      *    OPEN    OUTPUT              SPASCR-FILE
      *    IF      STS-SPASCR     NOT =   ZERO
      *        DISPLAY "J02SPASCR OUT OPEN ERR:"   STS-SPASCR "."
      *    END-IF
      *    MOVE    SPA-SCR-REC         TO  WRK-DATA-REC
      *    PERFORM VARYING     IDX     FROM   1   BY  1
      *            UNTIL      (IDX     >   20  )  OR
      *                       (WRK-DATA (IDX)     =   SPACE)
      *
      *        MOVE    WRK-DATA (IDX)      TO  SPA-DATA-REC
      *        WRITE                       SPA-DATA-REC
      *        IF      STS-SPASCR     NOT =   ZERO
      *            DISPLAY "J02SPASCR OUT WRITE ERR:"   STS-SPASCR "."
      *         END-IF
      *    END-PERFORM
      *
      **** CLOSE                       SPASCR-FILE
           .
       1003-J02SPA-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＳＰＡ一時保存テーブル追加
      *****************************************************************
       10031-SPATMP-INSERT-SEC                SECTION.
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-SPATMP-CALL-SEC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "J02 SPATMP INSERT ERR:"  MCP-RC
                           ",KEY:" SPATMP-KEY
               MOVE    "9999"          TO  SPA-ERRCD
           END-IF
           .
       10031-SPATMP-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *
           INITIALIZE                  FLG-AREA
      *****INITIALIZE                  STS-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "JERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
      *        画面編集
               IF      FLG-END         =   ZERO
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
               MOVE   SPACE                TO  MCP-PUTTYPE
               MOVE   "J02"                TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
               MOVE    1                   TO  FLG-END
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他画面より
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
      *
      *        画面ＳＰＡ読み込み
               PERFORM 1002-J02SPA-SET-SEC
      *
               MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
      *        元画面表示
               IF      WRK-MOTOPG          =   "Y01"  OR  "U01"
                                                      OR  "P97"
                                                      OR  "XC01"
                                                      OR  "C50"
                                                      OR  "K06"
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-J02
                   MOVE    SPA-WORK        TO  SPA-J01KYOUTU
                   MOVE    ZERO            TO  SPA-J025-SYORI
                   MOVE    SPACE           TO  SPA-JIDCD
               END-IF
      *        予約の時、そのまま画面表示へ
               IF      WRK-MOTOPG          =   "Y01"
                   GO      TO      300-SCREEN-EXT
               END-IF
      *        プレビューから
               IF      WRK-MOTOPG          =   "XC01"
                   GO      TO      300-SCREEN-EXT
               END-IF
      *        コメント入力から
               IF      WRK-MOTOPG          =   "C50"
                                           OR  "K06"
                   GO      TO      300-SCREEN-EXT
               END-IF
      *        外部より選択がある時
               IF      WRK-MOTOPG(1:3)     =   "U01"  OR
                                               "P97"
                   PERFORM 3101-P97SPASET-SEC
                   GO      TO      300-SCREEN-EXT
               END-IF
      *
               MOVE    SPA-MOTOPG          TO  SPA-MOTOPG2
           END-IF
      *
           EVALUATE    SPA-MOTOPG(1:4)
      *        回数入力より
               WHEN    "J022"
                   PERFORM 3002-J022-HEN-SEC
      *        カルテ印刷より
               WHEN    "J026"
                   CONTINUE
      *            確認画面より
               WHEN    "JID1"
                   PERFORM 3003-JID1-SET-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       PERFORM 500-GMNHEN-SEC
                       PERFORM 510-ERRSET-SEC
                       MOVE    1               TO  FLG-END
                   END-IF
                   IF     (SPA-ERRCD           =   SPACE )  AND
                          (SPA-JIDCD       NOT =   SPACE )
                       PERFORM 500-GMNHEN-SEC
                       PERFORM 520-JIDSET-SEC
                       MOVE    1               TO  FLG-END
                   END-IF
                   IF     (SPA-ERRCD           =   SPACE )  AND
                          (SPA-JIDCD           =   SPACE )  AND
                          (SPA-JIDCD2      NOT =   SPACE )
                       PERFORM 500-GMNHEN-SEC
                       PERFORM 520-JID2SET-SEC
                       MOVE    1               TO  FLG-END
                   END-IF
      *            確認画面より
               WHEN    "JID2"
                   PERFORM 3004-JID2-SET-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       PERFORM 500-GMNHEN-SEC
                       PERFORM 510-ERRSET-SEC
                       MOVE    1               TO  FLG-END
                   END-IF
                   IF     (SPA-ERRCD           =   SPACE )  AND
                          (SPA-JIDCD       NOT =   SPACE )
                       PERFORM 500-GMNHEN-SEC
                       PERFORM 520-JIDSET-SEC
                       MOVE    1               TO  FLG-END
                   END-IF
      *        収納変更確認画面
               WHEN    "J025"
                   PERFORM 3002-J025-BACK-SEC
                   IF     (SPA-ERRCD           =   SPACE )  AND
                          (SPA-JIDCD       NOT =   SPACE )
                       PERFORM 500-GMNHEN-SEC
                       PERFORM 520-JIDSET-SEC
                       MOVE    1               TO  FLG-END
                   END-IF
      *        その他
               WHEN    OTHER
                   PERFORM 3001-SCREEN-INIT-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面編集処理
      *****************************************************************
       3001-SCREEN-INIT-SEC                  SECTION.
      *
           MOVE    SPACE               TO  SPA-JIDCD
           INITIALIZE                  SPA-J02-AREA
           INITIALIZE                  SPA-J02-SYOKI
           INITIALIZE                  SPA-SCR-REC
           EVALUATE    SPA-MOTOPG
      *         業務選択より
               WHEN    "M01"
                   INITIALIZE                  SPA-J01KYOUTU
                   MOVE    SPA-MOTOPG          TO  SPA-J02-MOTOPG
                   MOVE    "0"                 TO  SPA-J02-MOTOFLG
                   MOVE    SPA-SYSYMDWH(1:6)   TO  SPA-GMN-SRYYM
                   MOVE    SPA-SYSYMD  (1:6)   TO  SPA-NAI-SRYYM
                   MOVE    SPA-SYSYMDWH        TO  SPA-J02-SRYYMDW
                   MOVE    SPA-SYSYMD          TO  SPA-J02-SRYYMD
                   MOVE    "2"                 TO  SPA-NYUGAIKBN
                   PERFORM 3005-SRYKA-HEN-SEC
                   MOVE    ZERO                TO  SPA-GMN-CUR
               WHEN    "K02N"
               WHEN    "K02"
      *        外来まとめより
               WHEN    "KT01"
      *            診療行為より
                   INITIALIZE                  SPA-J01KYOUTU
      *
                   MOVE    SPA-MOTOPG          TO  SPA-J02-MOTOPG
                   IF     (SPA-PTID            =   ZERO  )  OR
                          (SPA-PTNUM           =   SPACE )
                       MOVE    "0"                 TO  SPA-J02-MOTOFLG
                   ELSE
                       MOVE    "1"                 TO  SPA-J02-MOTOFLG
                   END-IF
                   MOVE    SPA-SRYYMDW(1:6)    TO  SPA-GMN-SRYYM
                   MOVE    SPA-SRYYMD (1:6)    TO  SPA-NAI-SRYYM
                   MOVE    SPA-SRYYMDW         TO  SPA-J02-SRYYMDW
                   MOVE    SPA-SRYYMD          TO  SPA-J02-SRYYMD
                   MOVE    SPA-PTNUM           TO  SPA-GMN-PTNUM
                   MOVE    SPA-PTID            TO  SPA-GMN-PTID
                   MOVE    SPA-KANANAME        TO  SPA-GMN-KANANAME
                   MOVE    SPA-NAME            TO  SPA-GMN-NAME
                   MOVE    SPA-SEX             TO  SPA-GMN-SEX
                   MOVE    SPA-BIRTHDAYW       TO  SPA-GMN-BIRTHDAYW
                   MOVE    SPA-BIRTHDAY        TO  SPA-GMN-BIRTHDAY
      *
                   IF     (SPA-PTID            =   ZERO  )  OR
                          (SPA-PTNUM           =   SPACE )
                       MOVE    SPACE               TO  SPA-GMN-SRYKA
                       MOVE    SPACE               TO  SPA-J01-SRYKA
                       MOVE    SPACE               TO  SPA-J01-SRYKAMEI
                       MOVE    ZERO                TO  SPA-GMN-CUR
                       PERFORM 3005-SRYKA-HEN-SEC
                   ELSE
      *                患者マスター初期編集・チェック
                       PERFORM 410100-PTINF-HEN-SEC
                       MOVE    SPA-SRYKA           TO  SPA-J01-SRYKA
                       MOVE    SPA-SRYKA           TO  SPA-GMN-SRYKA
                       MOVE    SPA-SRYKAMEI        TO  SPA-J01-SRYKAMEI
                       MOVE    1                   TO  FLG-HENSYU
                       PERFORM 310-SPA-SET-SEC
                       MOVE    1                   TO  SPA-GMN-CUR
      *                排他制御処理
                       PERFORM 990-LOCK-IN-SEC
                   END-IF
               WHEN    "U02"
                   INITIALIZE                  SPA-J01KYOUTU
      *            受付より
                   MOVE    SPA-MOTOPG          TO  SPA-J02-MOTOPG
                   IF     (SPA-PTID            =   ZERO  )  OR
                          (SPA-PTNUM           =   SPACE )
                       MOVE    "0"                 TO  SPA-J02-MOTOFLG
                   ELSE
                       MOVE    "1"                 TO  SPA-J02-MOTOFLG
                   END-IF
                   MOVE    SPA-SYSYMDWH(1:6)   TO  SPA-GMN-SRYYM
                   MOVE    SPA-SYSYMD  (1:6)   TO  SPA-NAI-SRYYM
                   MOVE    SPA-SYSYMDWH        TO  SPA-J02-SRYYMDW
                   MOVE    SPA-SYSYMD          TO  SPA-J02-SRYYMD
                   MOVE    "2"                 TO  SPA-NYUGAIKBN
                   IF     (SPA-PTID            =   ZERO  )  OR
                          (SPA-PTNUM           =   SPACE )
                       MOVE    ZERO                TO  SPA-GMN-CUR
                       PERFORM 3005-SRYKA-HEN-SEC
                   ELSE
                       MOVE    SPA-SRYKA           TO  SPA-J01-SRYKA
                       MOVE    SPA-SRYKA           TO  SPA-GMN-SRYKA
                       MOVE    SPA-SRYKAMEI        TO  SPA-J01-SRYKAMEI
                       MOVE    1                   TO  FLG-HENSYU
                       MOVE    SPA-PTNUM           TO  J02-PTNUM
                       PERFORM 41010-PTNUM-SYORI-SEC
                       MOVE    1                   TO  FLG-HENSYU
                       PERFORM 310-SPA-SET-SEC
                       MOVE    1                   TO  SPA-GMN-CUR
                   END-IF
      *
               WHEN     OTHER
                   MOVE    SPA-J02-SRYYMDW(1:6)    TO  SPA-GMN-SRYYM
                   MOVE    SPA-J02-SRYYMD (1:6)    TO  SPA-NAI-SRYYM
                   IF      SPA-MOTOPG(1:3)     =   "J04"  OR  "J03"
                       MOVE    SPA-MAE-SRYYMD      TO  SPA-SRYYMD
                       MOVE    SPA-MAE-PTID        TO  SPA-PTID
                   END-IF
      *
                   IF      SPA-MOTOPG          =   "J04"
      *?               MOVE    J02-DATALIST-ROW
      *?                                       TO  MAE-J02-DATALIST-ROW
                       MOVE    SPA-MAE-ZAINUM  TO  WRK-MAE-ZAINUM
                   END-IF
                   MOVE    1                   TO  FLG-HENSYU
                   PERFORM 310-SPA-SET-SEC
                   MOVE    1                   TO  SPA-GMN-CUR
           END-EVALUATE
      *
           MOVE    1                   TO  SPA-GMN-PAGE
           MOVE    1                   TO  SPA-IDXZ
      *
           .
       3001-SCREEN-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    回数入力画面より編集処理
      *****************************************************************
       3002-J022-HEN-SEC                  SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   31
               IF      SPA-J022NAI-DAYCHK (IDX)    =   1
                   MOVE    ZERO                TO  SPA-GNAI-DAY (1 IDX )
                   PERFORM VARYING     IDX-J   FROM    1   BY  1
                           UNTIL       IDX-J   >   9
                       COMPUTE IDR4            =   IDX-J   +   1
                       MOVE    SPA-J022NAI-DAY (IDX IDX-J)
                                           TO  SPA-GNAI-DAY (IDR4 IDX)
                       COMPUTE SPA-GNAI-DAY (1 IDX )
                                           =   SPA-GNAI-DAY (1 IDX )
                                           +   SPA-J022NAI-DAY
                                                           (IDX IDX-J)
                   END-PERFORM
      **           MOVE    SPA-NAI-DAY2(IDX)   TO  SPA-GNAI-DAY (3 IDX)
      **           MOVE    SPA-NAI-DAY3(IDX)   TO  SPA-GNAI-DAY (4 IDX)
      **           COMPUTE SPA-GNAI-DAY (1 IDX )
      *                                        =   SPA-NAI-DAY1 (IDX)
      *                                        +   SPA-NAI-DAY2 (IDX)
      *****                                    +   SPA-NAI-DAY3 (IDX)
                   MOVE    SPA-GNAI-DAY (1 IDX )   TO  SPA-GMN-DAY(IDX)
               END-IF
           END-PERFORM
      *    同日複数回数の判定
           MOVE    ZERO                TO  IDX-IN
           PERFORM 410129-DAY-NAICHK-SEC
           IF      FLG-END             =   1
               GO      TO      3002-J022-HEN-EXT
           END-IF
      *
           IF      SPA-GMN-CUR         =   2
               IF      SPA-DAY-IDX             >=  SPA-SRYMATU
                   MOVE    5                   TO  SPA-GMN-CUR
               ELSE
                   MOVE    2                   TO  SPA-GMN-CUR
                   ADD     1                   TO  SPA-DAY-IDX
               END-IF
           ELSE
               IF     (SPA-ERRCD           =   SPACE )  AND
                      (SPA-GMN-IKKATU  NOT =   SPACE )
                   MOVE    SPACE               TO  SPA-GMN-IKKATU
               END-IF
           END-IF
      *
           .
       3002-J022-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     確認画面よりの処理
      *****************************************************************
       3003-JID1-SET-SEC                  SECTION.
      *
           EVALUATE    SPA-JIDCD
               WHEN    "0101"
               WHEN    "0102"
                   MOVE    SPACE           TO  SPA-JIDCD
      *            戻るの確認
                   IF      SPA-JID1-FLG        =   "OK"
                       MOVE    1                   TO  SPA-GMN-CUR
                       MOVE    SPACE               TO  SPA-UPDFLG
                       PERFORM 210-BACK-SYORI-SEC
                   ELSE
                       MOVE    1                   TO  SPA-GMN-CUR
                   END-IF
      *
               WHEN    "0103"
                   MOVE    SPACE           TO  SPA-JIDCD
      *            明細選択の確認
                   IF      SPA-JID1-FLG        =   "OK"
                       MOVE    1                   TO  SPA-GMN-CUR
                       MOVE    SPA-GMN-BANGO       TO  J02-NUM
                       PERFORM 41011-BANGO-CHK-SEC
                   ELSE
                       MOVE    2                   TO  SPA-GMN-CUR
                       MOVE    SPA-MAE-BANGO       TO  SPA-GMN-BANGO
                       MOVE    SPA-MAEN-BANGO      TO  SPA-NAI-BANGO
                   END-IF
      *
               WHEN    "0104"
               WHEN    "0134"
                   MOVE    SPACE           TO  SPA-JIDCD
      *            剤変更の確認
                   IF      SPA-JID1-FLG        =   "OK"
                       MOVE     1             TO  SPA-J02-SYORI
                       PERFORM 420-ZAIHENKOU-SYORI-SEC
                   ELSE
                       MOVE     1             TO  SPA-GMN-CUR
                   END-IF
      *
               WHEN    "0106"
                   MOVE    SPACE           TO  SPA-JIDCD
      *            診療年月変更の確認
                   IF      SPA-JID1-FLG        =   "OK"
                       IF      SPA-J02-SEL         =   1
      *                    前月処理へ
                           PERFORM 41018-ZENGETU-SYORI-SEC
                       ELSE
      *                    次月処理へ
                           PERFORM 41019-JIGETU-SYORI-SEC
                       END-IF
                   ELSE
                       MOVE    1                  TO  SPA-GMN-CUR
                   END-IF
      *
               WHEN    "0107"
                   MOVE    SPACE           TO  SPA-JIDCD
      *            診療科変更の確認
                   IF      SPA-JID1-FLG        =   "OK"
                       PERFORM 41013-SRYKA-CHANGE-SEC
                       MOVE    1                  TO  SPA-GMN-CUR
                   ELSE
                       MOVE    SPA-MAE-SRYKA-G    TO  SPA-GMN-SRYKA-G
                       MOVE    1                  TO  SPA-GMN-CUR
                   END-IF
      *
               WHEN    "0108"
                   MOVE    SPACE           TO  SPA-JIDCD
      *            診療年月変更の確認
                   IF      SPA-JID1-FLG        =   "OK"
                       PERFORM 410181-SRYYM-CHANGE-SEC
                       MOVE    1                  TO  SPA-GMN-CUR
                   ELSE
                       MOVE    SPA-J02-SRYYMDW(1:6)  TO  SPA-GMN-SRYYM
                       MOVE    SPA-J02-SRYYMD(1:6)   TO  SPA-NAI-SRYYM
                       MOVE    1                  TO  SPA-GMN-CUR
                   END-IF
               WHEN    "0109"
                   MOVE    SPACE           TO  SPA-JIDCD
      *            保険一括変換の確認
                   IF      SPA-JID1-FLG        =   "OK"
                       PERFORM 41001-HKNHEN-SYORI-SEC
                   ELSE
                       MOVE    1                  TO  SPA-GMN-CUR
                   END-IF
      *
               WHEN    "0110"
                   MOVE    SPACE           TO  SPA-JIDCD
      *            診療内容変更の確認
                   IF      SPA-JID1-FLG        =   "OK"
                       PERFORM 410171-NYUGAIKBN-CHG-SEC
                       MOVE    1                  TO  SPA-GMN-CUR
                   ELSE
                       MOVE    SPA-J02-SRYYMDW(1:6)  TO  SPA-GMN-SRYYM
                       MOVE    SPA-J02-SRYYMD(1:6)   TO  SPA-NAI-SRYYM
                       MOVE    1                  TO  SPA-GMN-CUR
                   END-IF
               WHEN    "0111"
                   MOVE    SPACE           TO  SPA-JIDCD
      *            保険組合せ確認
                   IF      SPA-JID1-FLG        =   "OK"
                       PERFORM 410171-NYUGAIKBN-CHG-SEC
                       IF      SPA-GMN-MAX         =   ZERO
                           MOVE    10              TO  SPA-GMN-CUR
                       END-IF
                   ELSE
                       MOVE    SPA-MAE-HHKNCOMBI-G TO
                                                   SPA-GMN-HHKNCOMBI-G
                       MOVE    1                   TO  SPA-GMN-CUR
                   END-IF
               WHEN    "0112"
                   MOVE    SPACE           TO  SPA-JIDCD
      *            入外区分
                   IF      SPA-JID1-FLG        =   "OK"
                       PERFORM 410171-NYUGAIKBN-CHG-SEC
                       IF      SPA-GMN-MAX         =   ZERO
                           MOVE    10              TO  SPA-GMN-CUR
                       END-IF
                   ELSE
                       MOVE    SPA-MAE-SRYKBN-G    TO
                                                   SPA-GMN-SRYKBN-G
                       MOVE    1                   TO  SPA-GMN-CUR
                   END-IF
      *
               WHEN    "0113"
                   MOVE    SPACE           TO  SPA-JIDCD
      *            前頁表示の確認
                   IF      SPA-JID1-FLG        =   "OK"
                       PERFORM 4401-MAEGMN-SYORI-SEC
                   ELSE
                       MOVE    1                  TO  SPA-GMN-CUR
                   END-IF
      *
               WHEN    "0114"
                   MOVE    SPACE           TO  SPA-JIDCD
      *            前頁表示の確認
                   IF      SPA-JID1-FLG        =   "OK"
                       PERFORM 4501-ATOGMN-SYORI-SEC
                   ELSE
                       MOVE    1                  TO  SPA-GMN-CUR
                   END-IF
      *
      *********WHEN    "0115"
               WHEN    "0118"
                   MOVE    SPACE           TO  SPA-JIDCD
      *            受診日の変更確認
                   IF      SPA-JID1-FLG        =   "OK"
                       PERFORM 410161-HENSRYYMD-UPDSYORI-SEC
                   ELSE
                       MOVE    SPACE           TO  SPA-NAI-HENSRYYMD
                       MOVE    SPACE           TO  SPA-GMN-HENSRYYMD
                       MOVE    SPACE           TO  SPA-JIDCD
                       MOVE    SPACE           TO  SPA-JID1-FLG
                   END-IF
      *
               WHEN    "0116"
               WHEN    "0117"
                   MOVE    SPACE           TO  SPA-JIDCD
      *            印刷確認
                   IF      SPA-JID1-FLG        =   "OK"
                       PERFORM 41201-PRINT-SYORI-SEC
                   ELSE
                       MOVE    1                  TO  SPA-GMN-CUR
                   END-IF
               WHEN    "0119"
               WHEN    "0131"
                   MOVE    SPACE           TO  SPA-JIDCD
      *            診療日追加
                   IF      SPA-JID1-FLG        =   "OK"
      *                受診日追加処理
                       PERFORM 41016211-HENYMD-INSSYORI-SEC
                   ELSE
                       MOVE    1                  TO  SPA-GMN-CUR
                   END-IF
               WHEN    "0120"
                   MOVE    SPACE           TO  SPA-JIDCD
      *            内分泌負荷試験警告
      *            収納変更時、再計算
                   IF      SPA-SYUNOU-KOUSIN   =   1
                       PERFORM 49012-SYUNOU-SAIKEISAN-SEC
                   END-IF
               WHEN    "0123"
      *            同一日に受診あり
                   MOVE    SPACE           TO  SPA-JIDCD
                   IF      SPA-JID1-FLG        =   "OK"
                       PERFORM 410162-GRP-DENPNUM-CHK-SEC
                       IF      SPA-NAI-KEIFLG2     =   1
                           MOVE    "0124"          TO  SPA-JIDCD
                       ELSE
      *                    更新の確認メッセージ
                           PERFORM 41016-HENSRYYMD-KMSG-SEC
                       END-IF
                   ELSE
                       MOVE    4                   TO  SPA-GMN-CUR
                   END-IF
               WHEN    "0124"
      *            受診連番が３以上
                   MOVE    SPACE           TO  SPA-JIDCD
                   IF      SPA-JID1-FLG        =   "OK"
      *                更新の確認メッセージ
                       PERFORM 41016-HENSRYYMD-KMSG-SEC
                   ELSE
                       MOVE    4                   TO  SPA-GMN-CUR
                   END-IF
      *
               WHEN    "0130"
                   MOVE    SPACE           TO  SPA-JIDCD
      *            回数ゼロ剤削除
                   IF      SPA-JID1-FLG        =   "OK"
                       PERFORM 42001-ZAIDELETE-SYORI-SEC
                   ELSE
                       MOVE    1                  TO  SPA-GMN-CUR
                   END-IF
      *
               WHEN    "0140"
                   MOVE    SPACE           TO  SPA-JIDCD
      *            剤のない受診履歴削除
                   IF      SPA-JID1-FLG        =   "OK"
                       PERFORM 43000-JYURRKDELETE-SYORI-SEC
                   ELSE
                       MOVE    1                  TO  SPA-GMN-CUR
                   END-IF
      *
      *H25.4
               WHEN    "0149"
                   MOVE    SPACE           TO  SPA-JIDCD
      *            レセ電移行データ削除
                   PERFORM 43990-RECEDEN-DELETE-SYORI-SEC
      *
               WHEN    "0121"
                   MOVE    SPACE           TO  SPA-JIDCD
      *            プレビュー確認
                   IF      SPA-JID1-FLG        =   "OK"
                       PERFORM 4130-PREVIEW-CHK-SEC
                   ELSE
                       MOVE    SPACE           TO  SPA-JIDCD
                       MOVE    1                  TO  SPA-GMN-CUR
                   END-IF
               WHEN    "0125"
      *            プレビュー確認
                   MOVE    SPACE           TO  SPA-JIDCD
                   MOVE    SPACE           TO  SPA-XC01-CHK
                   IF      SPA-JID1-FLG        =   "OK"
                       PERFORM 4130-PREVIEW-SEC
                   ELSE
                       MOVE    1                  TO  SPA-GMN-CUR
                   END-IF
           END-EVALUATE
           .
       3003-JID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *     確認画面２よりの処理
      *****************************************************************
       3004-JID2-SET-SEC                  SECTION.
      *
           MOVE    SPACE               TO  SPA-JIDCD
           EVALUATE    SPA-JIDCD2
               WHEN    "0115"
               WHEN    "0118"
                   EVALUATE    SPA-JID2-FLG
                       WHEN    1
      *                受診日の変更
                           PERFORM 410161-HENSRYYMD-UPDSYORI-SEC
                       WHEN    2
      *                受診日の追加
                           PERFORM 410162-HENSRYYMD-INSSYORI-SEC
                       WHEN    OTHER
                           MOVE    SPACE           TO  SPA-NAI-HENSRYYMD
                           MOVE    SPACE           TO  SPA-GMN-HENSRYYMD
                   END-EVALUATE
                   MOVE    ZERO            TO  SPA-NAI-KEIFLG2
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-JIDCD2
           MOVE    ZERO                TO  SPA-JID2-FLG
           .
       3004-JID2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ一括変換より処理
      *****************************************************************
       3004-HKNCOMBI-HENKAN-SEC              SECTION.
      *
      *    保存用パラメタ作成
           PERFORM 4201-SRYACCT-PARA-SEC
           MOVE    1                   TO  SPA-GMN-PAGE
           MOVE    1                   TO  SPA-STRNUM
      *
      *    診療会計マスターより編集をする
           PERFORM 410181-SRYACCT-HEN-SEC
      *
           .
       3004-HKNCOMBI-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＬＤ外の項目入力編集処理
      *****************************************************************
       3101-P97SPASET-SEC              SECTION.
      *
           MOVE    LNK-KYOUTU          TO  WRK-SPA-KYOUTU
      *    受付、患者検索より
           IF      WRK-SPA-PTNUM       =   SPA-GMN-PTNUM
               CONTINUE
           ELSE
               IF      WRK-SPA-PTNUM       NOT =   SPACE
      *            初期表示へ
                   MOVE    WRK-SPA-PTNUM   TO  J02-PTNUM
                   PERFORM 41010-PTNUM-SYORI-SEC
               END-IF
           END-IF
      *
           IF      SPA-GMN-PTID        =   ZERO
               MOVE    ZERO                TO  SPA-GMN-CUR
           ELSE
               MOVE    6                   TO  SPA-GMN-CUR
           END-IF
           .
       3101-P97SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科初期編集
      *****************************************************************
       3005-SRYKA-HEN-SEC                  SECTION.
      *
           INITIALIZE                  SPA-GMN-SRYKA-AREA
      *    診療科
           MOVE    SPACE               TO  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SPA-NAI-SRYYM       TO  SYS-1005-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1005-STYUKYMD(7:2)
           MOVE    SYS-1005-STYUKYMD   TO  SYS-1005-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  SPA-SRYKA-MAX
           PERFORM VARYING     IDX     FROM    2   BY  1
                   UNTIL       (IDX            >   100  )  OR
                               (FLG-SYSKANRI   =   1    )
               MOVE    MCPDATA-REC         TO  SYS-1005-REC
      *
               MOVE    SYS-1005-KBNCD(1:2) TO
                                         SPA-GMN-SRYKAL (IDX)
               MOVE    SYS-1005-SRYKANAME1 TO
                                         SPA-GMN-SRYKAMEIL (IDX)
      *
               ADD     1                   TO  SPA-SRYKA-MAX
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           END-PERFORM
      *
           IF      SPA-SRYKA-MAX       =   1
               MOVE    SPA-GMN-SRYKA-LIST (2)
                                       TO  SPA-GMN-SRYKA-LIST (1)
               MOVE    SPACE           TO  SPA-GMN-SRYKA-LIST (2)
           ELSE
               ADD     1                   TO  SPA-SRYKA-MAX
               MOVE    "00 全科"       TO  SPA-GMN-SRYKA-LIST (1)
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *H25.4
      *    レセ電移行対応
           IF     (SPA-SRYKA-MAX       =   1    )
             AND  (SPA-PTID        NOT =   ZERO )
      *        診療科１件の時、レセ電移行チェック
               PERFORM 310319-RESE-JYURRK-SEC
           END-IF
      *
           .
       3005-SRYKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                  SECTION.
      *
      *    入外区分
           MOVE    SPA-NYUGAIKBN       TO  SPA-GMN-NYUGAIKBN
      *
      *    入外区分・病床数
           PERFORM 3009-NYUGAIKBN-HEN-SEC
      *    診療科一覧編集
           PERFORM 3005-SRYKA-HEN-SEC
      *
      *    診療種別
           PERFORM 3006-SRYKBN-HEN-SEC
      *    診療順
           PERFORM 3007-SRYKBN-FLG-HEN-SEC
      *
      *H28.4
      *     一般名記載設定処理
           PERFORM 3008-GENERIC-HEN-SEC
      *
           IF     (SPA-PTID            =   ZERO )  AND
                  (SPA-GMN-PTID        =   ZERO )
               MOVE    00                  TO  SPA-GMN-CUR
           ELSE
               MOVE    1                   TO  SPA-GMN-CUR
               PERFORM 3000-PTID-INIT-SEC
           END-IF
           .
       310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分・病床数処理
      *****************************************************************
       3009-NYUGAIKBN-HEN-SEC                  SECTION.
      *
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SPA-NAI-SRYYM       TO  SYS-1001-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1001-STYUKYMD(7:2)
           MOVE    SYS-1001-STYUKYMD   TO  SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  SYSKANRI-REC
           PERFORM 9001-SYSKANRI-READ-SEC
      *
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1001-REC
      *        病院種別
               MOVE    SYS-1001-HOSPSBT    TO  SPA-HOSPSBT
               IF      SPA-BEDKBN          =   ZERO
                   CONTINUE
               ELSE
      *        病床数
                   MOVE    SYS-1001-BEDSU          TO  SPA-BEDSU
      *        病床数（一般）
                   IF     (SYS-1001-BEDSUIPN(1:4)  =   SPACE )  OR
                          (SYS-1001-BEDSUIPN(1:4)  NOT NUMERIC)
                       MOVE    ZERO                TO  SPA-BEDSUIPN
                   ELSE
                       MOVE    SYS-1001-BEDSUIPN   TO  SPA-BEDSUIPN
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-BEDSU           >   ZERO
               MOVE    "1 入院"            TO  SPA-GMN-NYUGAIKBN-LST (1)
               MOVE    "2 外来"            TO  SPA-GMN-NYUGAIKBN-LST (2)
               MOVE    2                   TO  SPA-NYUGAI-MAX
           ELSE
               MOVE    "2 外来"            TO  SPA-GMN-NYUGAIKBN-LST (1)
               MOVE    1                   TO  SPA-NYUGAI-MAX
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-NYUGAI-MAX
               IF      SPA-GMN-NYUGAIKBN   =   SPA-GMN-NYUGAIKBN-LST
                                                           (IDX)(1:1)
                   MOVE    SPA-GMN-NYUGAIKBN-LST (IDX)
                                           TO  SPA-GMN-NYUGAIKBN-G
               END-IF
           END-PERFORM
           .
       3009-NYUGAIKBN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       3000-PTID-INIT-SEC                  SECTION.
      *
           MOVE    SPACE               TO  WRK-HZN-ERRCD
      *
      *    保険一覧編集
           PERFORM 3008-HKNCOMBI-HEN-SEC
      *
      *    診療年月の末日算定
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    SPA-NAI-SRYYM       TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI(7:2)    TO  SPA-SRYMATU
      *
      *----(01.00.01) LINE ADD START ----------------------------------
      *    年齢計算（算定月の１日の年齢）
           MOVE    SPA-NAI-SRYYM       TO  WRK-YMDS2(1:6)
           MOVE    01                  TO  WRK-DDS2
           PERFORM 30101-NENREI-SET-SEC
      *    該当月で６歳になる時、年齢フラグを立てる
           IF     (SPA-NAI-NENREI-YY   =   05  )  AND
                  (SPA-NAI-NENREI-MM   =   11  )  AND
                  (WRK-YMDS1(5:2)      =   WRK-YMDS2(5:2))
               MOVE    1                   TO  SPA-FLG-NENREI
           END-IF
      *
      *
      *    Ｈ１４．１０時点で７０歳未満の時
           IF     (SPA-NAI-SRYYM       >=  "200210"   )  AND
                  (SPA-BIRTHDAY        >=  "19321001" )
               MOVE    75                  TO  WRK-ROU-NEN
           ELSE
               MOVE    70                  TO  WRK-ROU-NEN
           END-IF
      *    該当月１日が７５歳以上の時、老人とする
           IF      SPA-NAI-NENREI-YY   >=  WRK-ROU-NEN
               MOVE    1                   TO  SPA-J02-ROUJIN
           ELSE
               MOVE    ZERO                TO  SPA-J02-ROUJIN
           END-IF
      *
      *H25.4 レセ電移行対応
           IF      SPA-SRYKA-MAX       =   1
      *        診療科１件の時、レセ電移行チェック
               PERFORM 310319-RESE-JYURRK-SEC
               IF      SPA-SRYKA-MAX       =   2
                   MOVE    SPACE           TO  SPA-J01-SRYKA
                   MOVE    SPACE           TO  SPA-J01-SRYKAMEI
               END-IF
           END-IF
      *
      *    診療科選択のない時、以下中止
           MOVE    ZERO                TO  SPA-GMN-MAX
      *    表示診療科
      *    全科
           IF      SPA-MOTOPG          =   "J04"  OR  "J03"
                                       OR  "J021"
               CONTINUE
           ELSE
               IF      SPA-NAI-SRYKA-FLG   =   2
                   MOVE    SPA-GMN-SRYKA-LIST(01)
                                               TO  SPA-GMN-SRYKA-G
                   MOVE    SPA-GMN-SRYKA       TO  SPA-J01-SRYKA
                   MOVE    SPA-GMN-SRYKAMEI    TO  SPA-J01-SRYKAMEI
               END-IF
           END-IF
      *    診療科編集
           IF      SPA-J01-SRYKA       =   SPACE
      *        受診履歴より診療科決定
               PERFORM 310310-JYURRK-CHK-SEC
               IF      SPA-J01-SRYKA       =   SPACE
                   MOVE    "0020"          TO  SPA-ERRCD
                   MOVE    07              TO  SPA-GMN-CUR
                   GO      TO      3000-PTID-INIT-EXT
               END-IF
           END-IF
           PERFORM 3101-SRYKA-HEN-SEC
      *
      *    入院期間編集
           IF     (SPA-BEDSU           >   ZERO )
      *D     AND  (SPA-NYUGAIKBN       =  "1"   )
      *D     AND  (SPA-NAI-NYUINYMD    =  SPACE )
               PERFORM 410101-NYUINRRK-CHK-SEC
               PERFORM 410102-NYUINRRK-LST-SEC
           END-IF
      *
      *    診療科未入力の時、終了へ
           IF      FLG-HENSYU          =   ZERO
                MOVE    06              TO  SPA-GMN-CUR
                GO      TO      3000-PTID-INIT-EXT
           END-IF
      *
      *    入院料包括診療チェック処理
           PERFORM 30091-HKTCD-CHK-SEC
      *
      *    受診履歴編集
           PERFORM 3103-JYURRK-HEN-SEC
           IF     (SPA-NYUGAIKBN       =  "1"   ) AND
                  (SPA-J01-SRYKA   NOT =   SPACE)
               CONTINUE
           ELSE
               IF      SPA-RRK-MAX         =   ZERO
                   IF      SPA-SRYKA-MAX       =   1
                       MOVE    07              TO  SPA-GMN-CUR
                   ELSE
                       MOVE    06              TO  SPA-GMN-CUR
                   END-IF
                   GO      TO      3000-PTID-INIT-EXT
               END-IF
           END-IF
      *
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
      *    ＰＡＲＡ削除
           IF      SPA-MOTOPG(1:1)     =   "J"
               CONTINUE
           ELSE
               PERFORM 3106-PARA-DEL-SEC
           END-IF
      *
      *
           IF     (SPA-MOTOPG2(1:3)    =   "K02" OR  "U02"  ) OR
                  (SPA-MOTOPG          =   "KT01"           )  OR
                  (SPA-MOTOPG          =   "J04" OR  "J03"  OR  "J021")
      *        保存用パラメタ作成
               PERFORM 4201-SRYACCT-PARA-SEC
               MOVE    1                   TO  SPA-GMN-PAGE
               MOVE    1                   TO  SPA-STRNUM
      *        診療会計マスターより編集をする
               PERFORM 410181-SRYACCT-HEN-SEC
           ELSE
      *        診療会計マスターより編集をする
               PERFORM 410181-SRYACCT-HEN-SEC
           END-IF
      *    収納更新チェック
           PERFORM 3104-SYUNOU-CHECK-SEC
      *
           .
       3000-PTID-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    年齢計算処理
      *****************************************************************
       30101-NENREI-SET-SEC                  SECTION.
      *
           MOVE    SPA-GMN-BIRTHDAY    TO  WRK-YMDS1
           COMPUTE SPA-NAI-NENREI-YY   =   WRK-YYS2
                                       -   WRK-YYS1
           IF      WRK-YMDS1(5:4)      >   WRK-YMDS2(5:4)
               COMPUTE SPA-NAI-NENREI-YY   =   SPA-NAI-NENREI-YY
                                           -   1
               COMPUTE SPA-NAI-NENREI-MM   =  (WRK-MMS2  +  12 )
                                           -   WRK-MMS1
           ELSE
               COMPUTE SPA-NAI-NENREI-MM   =   WRK-MMS2
                                           -   WRK-MMS1
           END-IF
      *
      *    乳幼児チェック
           IF     (SPA-NAI-NENREI-YY       =   ZERO  )  AND
                  (SPA-NAI-NENREI-MM       =   ZERO  OR  1 )
      *        年齢期間算定
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY5-AREA
               MOVE    "51"                TO  LNK-DAY5-IRAI
               MOVE    SPA-GMN-BIRTHDAY    TO  LNK-DAY5-START
               MOVE    SPA-NAI-SRYYM       TO  LNK-DAY5-END(1:6)
               MOVE    SPA-SRYMATU         TO  LNK-DAY5-END(7:2)
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY5-AREA
               IF      LNK-DAY5-NISSU2     >   28
                   MOVE    1                   TO  SPA-FLG-NENREI
                   IF      LNK-DAY5-NISSU2     <=  31
                       MOVE    LNK-DAY5-NISSU2 TO  SPA-NAI-NENREI-DD
                       MOVE    ZERO            TO  SPA-NAI-NENREI-MM
                   END-IF
               END-IF
           END-IF
      *
      *
           .
       30101-NENREI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分編集処理
      *****************************************************************
       410102-NYUINRRK-LST-SEC                  SECTION.
      *
           MOVE    SPA-NYUGAIKBN       TO  SPA-GMN-NYUGAIKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-NYUGAI-MAX
               IF      SPA-GMN-NYUGAIKBN   =   SPA-GMN-NYUGAIKBN-LST
                                                           (IDX)(1:1)
                   MOVE    SPA-GMN-NYUGAIKBN-LST (IDX)
                                           TO  SPA-GMN-NYUGAIKBN-G
               END-IF
           END-PERFORM
           .
       410102-NYUINRRK-LST-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療種別初期編集処理
      *****************************************************************
       3006-SRYKBN-HEN-SEC                  SECTION.
      *
           INITIALIZE                  SPA-GMN-SRYKBN-COMB
           MOVE    "00 全体"           TO  SPA-GMN-SRYKBN-LST (01)
           MOVE    "01 全体（診区順）" TO  SPA-GMN-SRYKBN-LST (02)
           MOVE    "10 診察"           TO  SPA-GMN-SRYKBN-LST (03)
           MOVE    "20 投薬"           TO  SPA-GMN-SRYKBN-LST (04)
           MOVE    "30 注射"           TO  SPA-GMN-SRYKBN-LST (05)
           MOVE    "40 処置"           TO  SPA-GMN-SRYKBN-LST (06)
           MOVE    "50 手術"           TO  SPA-GMN-SRYKBN-LST (07)
           MOVE    "60 検査"           TO  SPA-GMN-SRYKBN-LST (08)
           MOVE    "70 画像診断"       TO  SPA-GMN-SRYKBN-LST (09)
           MOVE    "80 リハ・その他"   TO  SPA-GMN-SRYKBN-LST (10)
           MOVE    "90 自費・その他"   TO  SPA-GMN-SRYKBN-LST (11)
      *
           MOVE    11                  TO  SPA-SRYKBN-MAX
      *
           IF      SPA-GMN-SRYKBN-MAE  =   SPACE
               MOVE    SPA-GMN-SRYKBN-LST (01)
                                       TO  SPA-GMN-SRYKBN-G
           ELSE
               MOVE    SPA-GMN-SRYKBN-MAE  TO  SPA-GMN-SRYKBN
               PERFORM VARYING     IDX     FROM   1   BY  1
                       UNTIL       IDX     >   SPA-SRYKBN-MAX
                  IF      SPA-GMN-SRYKBN-LST (IDX)(1:2)
                                           =   SPA-GMN-SRYKBN
                       MOVE    SPA-GMN-SRYKBN-LST (IDX)
                                               TO  SPA-GMN-SRYKBN-G
                   END-IF
               END-PERFORM
           END-IF
           .
       3006-SRYKBN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診区初期表示編集
      *****************************************************************
       3007-SRYKBN-FLG-HEN-SEC                  SECTION.
      *
           INITIALIZE                  SPA-NAI-SRYKBN-FLG
           INITIALIZE                  SPA-NAI-SRYKA-FLG
      *    レセプトプレビュー
           MOVE    "1"                 TO  SPA-NAI-RESPRVFLG
      *    診療科
           MOVE    SPACE               TO  SYS-1043-REC
           MOVE    "1043"              TO  SYS-1043-KANRICD
           MOVE    "*"                 TO  SYS-1043-KBNCD
           MOVE    SPA-NAI-SRYYM       TO  SYS-1043-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1043-STYUKYMD(7:2)
           MOVE    SYS-1043-STYUKYMD   TO  SYS-1043-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1043-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1043-REC        TO  SYSKANRI-REC
           PERFORM 9001-SYSKANRI-READ-SEC
      *
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1043-REC
               MOVE    SYS-1043-SRYKBN-FLG TO  SPA-NAI-SRYKBN-FLG
               IF      SYS-1043-SRYKA-FLG  =   SPACE
                   MOVE    ZERO                TO  SPA-NAI-SRYKA-FLG
               ELSE
                   MOVE    SYS-1043-SRYKA-FLG  TO  SPA-NAI-SRYKA-FLG
               END-IF
      *        レセプトプレビュー
               IF      SYS-1043-RESPRVFLG  =   "1"  OR  "2"
                   MOVE    SYS-1043-RESPRVFLG  TO  SPA-NAI-RESPRVFLG
               ELSE
                   MOVE    "1"                 TO  SPA-NAI-RESPRVFLG
               END-IF
           END-IF
      *
           .
       3005-SRYKA-HEN-EXT.
           EXIT.
      *****************************************************************
      *   一般名記載設定処理
      *****************************************************************
       3008-GENERIC-HEN-SEC             SECTION.
      *
      *    後発医薬品への変更可署名
           MOVE    SPACE               TO  SYS-1030-REC
           INITIALIZE                  SYS-1030-REC
           MOVE    "1030"              TO  SYS-1030-KANRICD
           MOVE    "*"                 TO  SYS-1030-KBNCD
           MOVE    SPA-NAI-SRYYM       TO  SYS-1030-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1030-STYUKYMD(7:2)
           MOVE    SYS-1030-STYUKYMD   TO  SYS-1030-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1030-HOSPNUM
           MOVE    SYS-1030-REC        TO  SYSKANRI-REC
           PERFORM 9001-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1030-REC
               MOVE    SYS-1030-KOUHATUKA  TO  SPA-KOUHATUKA
           ELSE
               MOVE    "0"                 TO  SPA-KOUHATUKA
           END-IF
      *H28.4
      *    一般名表示
           MOVE    "0"                 TO  SPA-GENERICFLG
           IF      SPA-NAI-SRYYM       >=  "201604"
               MOVE    SPACE               TO  SYS-1038-REC
               INITIALIZE                  SYS-1038-REC
               MOVE    "1038"              TO  SYS-1038-KANRICD
               MOVE    "*"                 TO  SYS-1038-KBNCD
               MOVE    SPA-NAI-SRYYM       TO  SYS-1038-STYUKYMD(1:6)
               MOVE    "01"                TO  SYS-1038-STYUKYMD(7:2)
               MOVE    SYS-1038-STYUKYMD   TO  SYS-1038-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYS-1038-HOSPNUM
               MOVE    SYS-1038-REC        TO  SYSKANRI-REC
               PERFORM 9001-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    SYSKANRI-REC        TO  SYS-1038-REC
                   IF      SYS-1038-GENERICFLG     =   "2"
                       MOVE    "1"                 TO  SPA-GENERICFLG
                   END-IF
               END-IF
           END-IF
      *
           .
       3008-GENERIC-HEN-EXT.
           EXIT.
      *****************************************************************
      *   保険一覧編集処理
      *****************************************************************
       3008-HKNCOMBI-HEN-SEC             SECTION.
      *
           INITIALIZE                  SPA-GMN-HKNCOMBI-LISTG
           MOVE    SPA-GMN-BIRTHDAY    TO  SPA-BIRTHDAY
           MOVE    SPA-NAI-SRYYM       TO  SPA-SRYYMD(1:6)
           MOVE    "00"                TO  SPA-SRYYMD(7:2)
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
           MOVE    SPA-GMN-NYUGAIKBN   TO  SPA-NYUGAIKBN
      *
           INITIALIZE                  ORCSHKNSETAREA
           MOVE    "1"                 TO  ORCSHKN-KBN
           MOVE    "1"                 TO  ORCSHKN-KBN2
           CALL    "ORCSHKNSET"        USING
                                       SPA-AREA
                                       ORCSHKNSETAREA
      *
           MOVE    ORCSHKN-HKN-MAX     TO  SPA-HKN-MAX
           IF      SPA-HKN-MAX         =   ZERO
               GO      TO      3008-HKNCOMBI-HEN-EXT
           END-IF
      *
      *    全体を追加
           MOVE    "0000"              TO  SPA-GMN-THKNCOMBINUM (01)
           MOVE    "全保険"            TO  SPA-GMN-THKNCOMBIMEI (01)
           MOVE    ZERO                TO  SPA-NAI-THKNKBN      (01)
      *
           MOVE    1                   TO  IDH
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL      (IDX     >   ORCSHKN-HKN-MAX )  OR
                              (IDH     >=  40              )
               ADD     1                   TO  IDH
               MOVE    ORCSHKN-GMN-HKNCOMBINUM (IDX)
                                       TO  SPA-GMN-THKNCOMBINUM (IDH)
               MOVE    ORCSHKN-GMN-HKNCOMBIMEI (IDX)
                                       TO  SPA-GMN-THKNCOMBIMEI (IDH)
      *
               MOVE    ORCSHKN-NAI-HKNID (IDX)
                                       TO  SPA-NAI-THKNID  (IDH)
               MOVE    ORCSHKN-NAI-HKNNUM (IDX)
                                       TO  SPA-NAI-THKNNUM  (IDH)
               MOVE    ORCSHKN-NAI-HKNKBN (IDX)
                                       TO  SPA-NAI-THKNKBN  (IDH)
               MOVE    ORCSHKN-NAI-KOH1HKNNUM (IDX)
                                       TO  SPA-NAI-TKOH1HKNNUM (IDH)
               MOVE    ORCSHKN-NAI-KOH2HKNNUM (IDX)
                                       TO  SPA-NAI-TKOH2HKNNUM (IDH)
               MOVE    ORCSHKN-NAI-KOH3HKNNUM (IDX)
                                       TO  SPA-NAI-TKOH3HKNNUM (IDH)
               MOVE    ORCSHKN-NAI-KOH4HKNNUM (IDX)
                                       TO  SPA-NAI-TKOH4HKNNUM (IDH)
      *
               MOVE    ORCSHKN-NAI-FTNRATE (IDX)
                                       TO  SPA-NAI-TFTNRATE (IDH)
      *        四肢区分
               MOVE    ORCSHKN-NAI-RSI-SISIKBN (IDX)
                                       TO  SPA-NAI-TRSI-SISIKBN (IDH)
      *        傷病年月日
               MOVE    ORCSHKN-NAI-RSI-SHOBYOYMD (IDX)
                                       TO  SPA-NAI-TRSI-SHOBYOYMD(IDH)
      *        労災区分
               MOVE    ORCSHKN-NAI-RSI-HKNKBN (IDX)
                                       TO  SPA-NAI-TRSI-HKNKBN  (IDH)
      *        適用期間
               MOVE    ORCSHKN-NAI-TEKSTYMD (IDX)
                                       TO  SPA-NAI-TTEKSTYMD  (IDH)
               MOVE    ORCSHKN-NAI-TEKEDYMD (IDX)
                                       TO  SPA-NAI-TTEKEDYMD  (IDH)
               MOVE    ORCSHKN-GMN-TEKSTYMD (IDX)
                                       TO  SPA-GMN-TTEKSTYMD  (IDH)
               MOVE    ORCSHKN-GMN-TEKEDYMD (IDX)
                                       TO  SPA-GMN-TTEKEDYMD  (IDH)
      *        自賠責・労務災害保険の時、健保準拠かの判定
               IF      ORCSHKN-NAI-RSI-HKNKBN (IDX)    =   "4"
                                                       OR  "5"
                   PERFORM 30081-JIBAISEKI-HEN-SEC
               END-IF
      *        第三者行為（健保準拠）
               IF      ORCSHKN-NAI-RSI-HKNKBN (IDX)    =   "6"
                   MOVE    "1"             TO  SPA-NAI-TRSI-JUNKYO(IDH)
               END-IF
      *
               MOVE    IDH             TO  SPA-HKN-MAX
           END-PERFORM
      *
           MOVE    SPA-GMN-THKNCOMBINUM-G (01)
                                       TO  SPA-GMN-HHKNCOMBI-G
      *
           .
       3008-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   自賠責の健保準拠判定
      *****************************************************************
       30081-JIBAISEKI-HEN-SEC             SECTION.
      *
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  AGECHK-PTID
           MOVE    SPA-NAI-SRYYM       TO  AGECHK-SRYYMD(1:6)
           MOVE    "01"                TO  AGECHK-SRYYMD(7:2)
           MOVE    SPA-BIRTHDAY        TO  AGECHK-BIRTHDAY
           MOVE    ORCSHKN-GMN-HKNCOMBINUM (IDX)
                                       TO  AGECHK-HKNCOMBINUM
           MOVE    SPA-GMN-NYUGAIKBN   TO  AGECHK-NYUGAIKBN
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
      *
           EVALUATE    AGECHK-HKNKBN
      *        自賠責
               WHEN    4
                   MOVE    "1"             TO  SPA-NAI-TRSI-JUNKYO(IDH)
      *        労務災害
               WHEN    5
                   MOVE    "1"             TO  SPA-NAI-TRSI-JUNKYO(IDH)
           END-EVALUATE
           .
       30081-JIBAISEKI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   入院料包括診療チェック処理
      *****************************************************************
       30091-HKTCD-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  SPA-NAI-HKTERR
           MOVE    ZERO                TO  SPA-NAI-HKTKEI
           IF      SPA-GMN-NYUGAIKBN   =   "1"
               INITIALIZE                  ORCSCHKTALLAREA
               MOVE    "1"                 TO  ORCSCHKTALL-KBN
               MOVE    SPA-NAI-SRYYM       TO  ORCSCHKTALL-SRYYM
               MOVE    "01"                TO  ORCSCHKTALL-SRYDD
               MOVE    SPA-GMN-PTID        TO  ORCSCHKTALL-PTID
               CALL    "ORCSCHKTALL"       USING
                                           ORCSCHKTALLAREA
                                           SPA-AREA
               MOVE    ORCSCHKTALL-NYUHKTKBN   TO  SPA-NAI-NYUHKTKBN
               IF      ORCSCHKTALL-NYUERR      =   1
                   MOVE    1                   TO  SPA-NAI-HKTERR
                   MOVE    ZERO                TO  SPA-NAI-HKTKEI
               END-IF
           ELSE
               MOVE    ZERO                    TO  SPA-NAI-NYUHKTKBN
           END-IF
           .
       30091-HKTCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *   パラメタ削除処理
      *****************************************************************
       3106-PARA-DEL-SEC             SECTION.
      *
      *    パラメタ削除
           INITIALIZE                      PARA-REC
           MOVE    "J02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "SRYACCT"           TO  PARA-FILEMEI
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    パラメタ削除
           INITIALIZE                      PARA-REC
           MOVE    "J02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "JYURRK"            TO  PARA-FILEMEI
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       3106-PARA-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *   受診履歴より診療科決定処理
      *****************************************************************
       310310-JYURRK-CHK-SEC             SECTION.
      *
      *    受診歴を検索する  医療機関、患者ＩＤ
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-NAI-SRYYM       TO  JYURRK-SRYYMD(1:6)
           MOVE    "31"                TO  JYURRK-SRYYMD(7:2)
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key35"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key35"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key35"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-JYURRK          =   ZERO
               IF      SPA-NAI-SRYYM       =   JYURRK-SRYYMD(1:6)
                   MOVE    JYURRK-SRYKA        TO  SPA-J01-SRYKA
                   MOVE    JYURRK-SRYKA        TO  SPA-GMN-SRYKA
               END-IF
           END-IF
      *
      *    入院で調剤料のみの時のチェック
           IF     (SPA-NYUGAIKBN       =   "1" )  AND
                  (SPA-J01-SRYKA       =   SPACE ) 
      *        診療会計マスターを編集
               INITIALIZE                      SRYACCT-REC 
               MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
               MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
               MOVE    SPA-GMN-PTID        TO  ACCT-PTID
               MOVE    SPA-NAI-SRYYM       TO  ACCT-SRYYM
      *
               MOVE    SRYACCT-REC         TO  MCPDATA-REC
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key7"              TO  MCP-PATHNAME
                   PERFORM 900-SRYACCT-READ-SEC
               ELSE
                   INITIALIZE                  SRYACCT-REC
                   MOVE    1               TO  FLG-SRYACCT
               END-IF
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
               IF      FLG-SRYACCT         =   ZERO
                   MOVE    ACCT-SRYKA         TO  SPA-J01-SRYKA
                   MOVE    ACCT-SRYKA         TO  SPA-GMN-SRYKA
      *H25.4
                   IF      ACCT-SRYKA (1:1)   =   "A"
      *                レセ電分　全科へ
                       MOVE    "00"                TO  SPA-J01-SRYKA
                       MOVE    "00"                TO  SPA-GMN-SRYKA
                   END-IF
               END-IF
           END-IF
      *H25.4
      *    包括保険・レセ電分判定
           IF     (SPA-J01-SRYKA       =   SPACE ) 
               INITIALIZE                  JYURRK-REC
               MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
               MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
               MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
               MOVE    SPA-NAI-SRYYM       TO  JYURRK-SRYYMD(1:6)
               MOVE    "01"                TO  JYURRK-SRYYMD(7:2)
               MOVE    SPA-NAI-SRYYM       TO  JYURRK-UPSRYYMD(1:6)
               MOVE    "31"                TO  JYURRK-UPSRYYMD(7:2)
               MOVE    "9999"              TO  JYURRK-HKNCOMBINUM
      *
               MOVE    JYURRK-REC          TO  MCPDATA-REC
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                   MOVE    "key17"             TO  MCP-PATHNAME
                   PERFORM 970-JYURRK-READ-SEC
               ELSE
                   INITIALIZE                  JYURRK-REC
                   MOVE    1               TO  FLG-JYURRK
               END-IF
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *
               IF     (FLG-JYURRK          =   ZERO )
                   IF      JYURRK-SRYKA (1:1)  =   "A"
      *                レセ電分　全科へ
                       MOVE    "00"                TO  SPA-J01-SRYKA
                       MOVE    "00"                TO  SPA-GMN-SRYKA
                   ELSE
                       MOVE    JYURRK-SRYKA        TO  SPA-J01-SRYKA
                       MOVE    JYURRK-SRYKA        TO  SPA-GMN-SRYKA
                   END-IF
               END-IF
           END-IF
           .
       310310-JYURRK-CHK-EXT.
           EXIT.
      *
      *!!!!!!!!!!!!!!!!
      *****************************************************************
      *   診療科内容編集処理
      *****************************************************************
       310319-RESE-JYURRK-SEC             SECTION.
      *
      *    診療科１件の時、レセ電移行チェック
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           STRING  SPA-NAI-SRYYM
                   "%"                 DELIMITED  BY  SIZE
                                       INTO    JYURRK-SRYYMD
           END-STRING
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key54"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key54"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key54"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-JYURRK          =   ZERO
      *        全科追加
               MOVE    SPA-GMN-SRYKA-LIST (1)
                                       TO  SPA-GMN-SRYKA-LIST (2)
               MOVE    "00 全科"       TO  SPA-GMN-SRYKA-LIST (1)
               ADD     1               TO  SPA-SRYKA-MAX
           ELSE
               IF      SPA-GMN-SRYKA   =   "00"
                   MOVE    SPA-GMN-SRYKA-LIST(01)
                                           TO  SPA-GMN-SRYKA-G
               END-IF
           END-IF
           .
       310319-RESE-JYURRK-EXT.
           EXIT.
      *!!!!!!!!!!!!!!!!
      *
      *****************************************************************
      *   診療科内容編集処理
      *****************************************************************
       3101-SRYKA-HEN-SEC             SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SRYKA-MAX
               IF      SPA-J01-SRYKA       =   SPA-GMN-SRYKAL (IDX)
                   MOVE    SPA-GMN-SRYKA-LIST(IDX)
                                           TO  SPA-GMN-SRYKA-G
               END-IF
           END-PERFORM
      *
           .
       3101-SRYKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   受診履歴編集処理
      *****************************************************************
       3103-JYURRK-HEN-SEC             SECTION.
      *
      *    受診歴を検索する  医療機関、患者ＩＤ
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM          TO  JYURRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-J01-SRYKA       TO  JYURRK-SRYKA
           MOVE    SPA-NAI-SRYYM       TO  JYURRK-SRYYMD(1:6)
           MOVE    "01"                TO  JYURRK-SRYYMD(7:2)
           MOVE    SPA-NAI-SRYYM       TO  JYURRK-UPSRYYMD(1:6)
           MOVE    "31"                TO  JYURRK-UPSRYYMD(7:2)
           IF      SPA-J01-SRYKA       =   "00"
               MOVE    "key13"         TO  WRK-ORC-DBPATH
           ELSE
               MOVE    "key12"         TO  WRK-ORC-DBPATH
           END-IF
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           MOVE    ZERO                TO  SPA-RRK-MAX
           INITIALIZE                  SPA-GMN-RRKTBL
           INITIALIZE                  SPA-NAI-RRKTBL
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  FLG-JYURRK-ERR
           PERFORM
               UNTIL   (FLG-JYURRK         =   1   )  OR
                       (FLG-JYURRK-ERR NOT =   ZERO )
      *        診療日、連番で１行とする
               MOVE    ZERO            TO  IDX-S
      *        対象年月のみ
               IF      JYURRK-SRYYMD(1:6)  =   SPA-NAI-SRYYM
      *            診療日、連番で１行とする
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL       IDX2    >   SPA-RRK-MAX
                       IF     (JYURRK-RENNUM       =   SPA-RRK-RENNUM
                                                       (IDX2))  AND
                              (JYURRK-DOUJI-RENNUM =
                                                   SPA-RRK-DOUJI-RENNUM
                                                         (IDX2))  AND
                              (JYURRK-SRYYMD       =   SPA-RRK-RRKYMD
                                                       (IDX2)  )  AND
                              (JYURRK-SRYKA        =   SPA-RRK-SRYKA
                                                       (IDX2)  )
                          AND (JYURRK-HKNCOMBINUM  =
                                                   SPA-RRK-HKNCOMBI
                                                         (IDX2) )
                           MOVE    IDX2            TO  IDX-S
                           MOVE    SPA-RRK-MAX     TO  IDX2
                       END-IF
                   END-PERFORM
               ELSE
                   MOVE    99                  TO  IDX-S
               END-IF
      *
               IF      JYURRK-SRYYMD(1:6)  =   SPA-NAI-SRYYM
                   IF      IDX-S           =   ZERO
                       ADD     1               TO  SPA-RRK-MAX
                       IF      SPA-RRK-MAX     >   100
                           MOVE    1               TO  FLG-JYURRK-ERR
                           MOVE    100             TO  SPA-RRK-MAX
                           MOVE    ZERO            TO  IDX
                       ELSE
                           MOVE    SPA-RRK-MAX     TO  IDX
                       END-IF
                   ELSE
                       MOVE    IDX-S           TO  IDX
                   END-IF
                   IF      IDX             >   ZERO  AND
                                           <=  100
      *                明細項目退避
                       PERFORM 3101-JYURRK-HEN-SEC
                   END-IF
               END-IF
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-JYURRK-ERR      =   1
               IF      SPA-ERRCD           =   SPACE
                   MOVE    "0023"          TO  WRK-HZN-ERRCD
               END-IF
           END-IF
      *
      *    診察日の再編集
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-RRK-MAX
               MOVE    ZERO            TO  FLG-HEN
               PERFORM VARYING     IDX2    FROM    IDX BY  1
                       UNTIL       IDX2    >   SPA-RRK-MAX
                   IF     (IDX2            NOT =   IDX )  AND
                          (SPA-RRK-RRKYMD(IDX)
                                               =   SPA-RRK-RRKYMD
                                                       (IDX2)) AND
                          (SPA-RRK-SRYKA(IDX)  =   SPA-RRK-SRYKA
                                                       (IDX2))
                       MOVE    1                   TO  FLG-HEN
                       MOVE    SPA-RRK-MAX         TO  IDX2
                   ELSE
                       IF     (SPA-RRK-RRKYMD(IDX)
                                           NOT =   SPA-RRK-RRKYMD
                                                       (IDX2))
                           MOVE    SPA-RRK-MAX     TO  IDX2
                       END-IF
                   END-IF
               END-PERFORM
               IF     (SPA-RRK-RENNUM (IDX)    >   1 ) OR
                      (SPA-RRK-DOUJI-RENNUM (IDX)  >  ZERO )
                   MOVE    1                   TO  FLG-HEN
               END-IF
               IF      FLG-HEN             =   1
                   MOVE    "("                 TO  SPA-GMN-RRKYMD (IDX)
                                                               (10:1)
                   MOVE    SPA-RRK-RENNUM (IDX) 
                                               TO  SPA-GMN-RRKYMD (IDX)
                                                              (11:1)
                   MOVE    ")"                 TO  SPA-GMN-RRKYMD (IDX)
                                                              (12:1)
                   IF      SPA-RRK-DOUJI-RENNUM (IDX)  NOT =   ZERO
                       MOVE    SPA-RRK-DOUJI-RENNUM (IDX) 
                                               TO  SPA-GMN-RRKYMD (IDX)
                                                              (13:1)
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       3103-JYURRK-HEN-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       3101-JYURRK-HEN-SEC             SECTION.
      *
           MOVE    IDX                 TO  SPA-GMN-RRKNO (IDX)
      *    連番
           MOVE    JYURRK-RENNUM       TO  SPA-RRK-RENNUM(IDX)
      *    診療科
           MOVE    JYURRK-SRYKA        TO  SPA-RRK-SRYKA (IDX)
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   SPA-SRYKA-MAX
               IF      JYURRK-SRYKA        =   SPA-GMN-SRYKAL (IDY)
                   MOVE    SPA-GMN-SRYKAMEIL(IDY)(1:2)
                                           TO  SPA-GMN-RRKSRYKA (IDX)
               END-IF
           END-PERFORM
      *H25.2
           IF      SPA-GMN-RRKSRYKA (IDX)  =   SPACE
               MOVE    JYURRK-SRYKA        TO  SPA-GMN-RRKSRYKA (IDX)
           END-IF
      *!!!!
      *    保険組合せ
           MOVE    JYURRK-HKNCOMBINUM  TO  SPA-GMN-RRKHKNCOMBI (IDX)
           IF      JYURRK-HKNKBN       =   "1"
                                       OR  "5"
               MOVE    "労"                TO  SPA-GMN-RRKHKNCOMBI (IDX)
           END-IF
           IF      JYURRK-HKNKBN       =   "2"
                                       OR  "4"
               MOVE    "自"                TO  SPA-GMN-RRKHKNCOMBI (IDX)
           END-IF
      *
      *    同時連番
           MOVE    JYURRK-DOUJI-RENNUM TO  SPA-RRK-DOUJI-RENNUM(IDX)
      *    診察日
           MOVE    JYURRK-SRYYMD       TO  SPA-RRK-RRKYMD(IDX)
           MOVE    JYURRK-SRYYMD       TO  SPA-RRK-RRKYMD-MAE(IDX)
           MOVE    JYURRK-SRYYMD       TO  WRK-SYMD
           PERFORM 41012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-RRKYMD (IDX)
      *    伝票番号
           MOVE    JYURRK-DENPNUM      TO  SPA-RRK-DENPNUM (IDX)
      *    労災用
      *    保険区分
           MOVE    JYURRK-HKNKBN       TO  SPA-RRK-HKNKBN (IDX)
      *    保険組合せ
           MOVE    JYURRK-HKNCOMBINUM  TO  SPA-RRK-HKNCOMBI (IDX)
      *
           MOVE    JYURRK-DRCD         TO  SPA-RRK-DRCD     (IDX)
      *
      *    まとめ入力伝票番号
           MOVE    JYURRK-GRP-DENPNUM  TO  SPA-RRK-GRP-DENPNUM (IDX)
           MOVE    JYURRK-GRP-RENNUM   TO  SPA-RRK-GRP-RENNUM  (IDX)
      *
      *    内部の剤番号
           MOVE    ZERO                TO  IDX-RRK
           PERFORM VARYING     IDX-N   FROM    1   BY  1
                   UNTIL      (IDX-N       >   135  )  OR
                              (SPA-RRK-ZAINUM (IDX IDX-N)  =   ZERO )
               ADD     1               TO  IDX-RRK
           END-PERFORM
           PERFORM VARYING     IDX-N   FROM    1   BY  1
                   UNTIL      (IDX-N       >   15  )  OR
                              (IDX-RRK     >=  135 )
               IF      JYURRK-ZAINUM (IDX-N)   NOT =   ZERO
                   ADD     1               TO  IDX-RRK
                   MOVE    JYURRK-ZAINUM (IDX-N)   TO  SPA-RRK-ZAINUM
                                                         (IDX IDX-RRK)
               END-IF
           END-PERFORM
      *
           .
       3101-JYURRK-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療会計マスターより画面内容編集処理
      *****************************************************************
       3101-SRYACCT-SPA-SEC             SECTION.
      *
      *    診療会計マスタ内容編集
           MOVE    WRK-BANGO            TO  SPA-NAI-RENNUM  (IDX)
           MOVE    ACCT-SRYKA           TO  SPA-NAI-SRYKA   (IDX)
           MOVE    ACCT-SRYKBN          TO  SPA-NAI-SRYKBN  (IDX)
           MOVE    ACCT-ZAINUM          TO  SPA-NAI-ZAINUM  (IDX)
           MOVE    ACCT-HKNCOMBI        TO  SPA-NAI-HKNCOMBI (IDX)
           MOVE    ACCT-ZAITEN          TO  SPA-NAI-ZAITEN  (IDX)
           MOVE    ACCT-ZAIKAISU        TO  SPA-NAI-ZAIKAISU(IDX)
           MOVE    ACCT-DAY-AREA        TO  SPA-NAI-DAY-AREA (IDX)
      *    労災で外来管理加算の振替えありの時、手技料２がある
           MOVE    ACCT-SYUTEN2         TO  SPA-NAI-SYUTEN2 (IDX)
      *    包括診療分
           MOVE    ACCT-JIHOKBN         TO  SPA-NAI-JIHOKBN (IDX)
      *    剤区分
           MOVE    ACCT-ZAIKBN          TO  SPA-NAI-ZAIKBN  (IDX)
      *
      *    老人判定
           IF      ACCT-HKNCOMBI       =   "9999"
               INITIALIZE                  HKNCOMBI-REC
               MOVE    "00000000"          TO  COMB-TEKSTYMD
               MOVE    "99999999"          TO  COMB-TEKEDYMD
               MOVE    ZERO                TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
               MOVE    SPA-GMN-PTID        TO  COMB-PTID
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
                   IF      COMB-DELKBN     NOT  =   SPACE
      *                削除は対象外
                       MOVE    1               TO  FLG-HKNCOMBI
                       INITIALIZE                  HKNCOMBI-REC
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-HKNCOMBI
                   INITIALIZE                  HKNCOMBI-REC
               END-IF
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-IF
           IF     (COMB-KOH1HKNNUM     =   "027"  )  OR
                  (COMB-KOH2HKNNUM     =   "027"  )  OR
                  (COMB-KOH3HKNNUM     =   "027"  )  OR
                  (COMB-KOH4HKNNUM     =   "027"  )  OR
                  (SPA-J02-ROUJIN      =   1      )
               MOVE    1               TO  SPA-NAI-ROUJIN (IDX)
           ELSE
               MOVE    ZERO            TO  SPA-NAI-ROUJIN (IDX)
           END-IF
      *    有効期間判定
           MOVE    ZERO                TO  FLG-HKN-CHK
           MOVE    ACCT-SRYYM          TO  WRK-SRYYMD(1:6)
           IF      FLG-HKNCOMBI        =   1
               MOVE    1                   TO  FLG-HKN-CHK
           END-IF
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL      (IDX-D       >   31  )
                       OR     (FLG-HKN-CHK =   1   )
               IF      ACCT-DAY (1 IDX-D)  NOT =   ZERO
                   MOVE    IDX-D           TO  WRK-SRYDD
                   IF     (WRK-SRYYMD      <   COMB-TEKSTYMD)
                     OR   (WRK-SRYYMD      >   COMB-TEKEDYMD)
                       MOVE    1           TO  SPA-NAI-HKNCOMBIERR
                                                                (IDX)
                       MOVE    1           TO  FLG-HKN-CHK
                       MOVE    31          TO  IDX-D
                   END-IF
               END-IF
           END-PERFORM
      *ver.4.7
      *    院内処方点数編集
           IF     (ACCT-ZAITEN         =   ZERO )
              AND (ACCT-ZAIREQFLG      =   "1"  )
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
                   MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 920-DBFETCH-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    MCPDATA-REC     TO  SRYACCTPLUS-REC
      *                院外処方点数
                       MOVE    ACCTP-INGAITEN  TO  SPA-NAI-INGAITEN
                                                                 (IDX)
                   ELSE
                       INITIALIZE                  SRYACCTPLUS-REC
                   END-IF
               END-IF
               MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-IF
      *
      *H28.4
      *    一般名称編集
           INITIALIZE                  ORCSGETGENERICAREA
           IF      (ACCT-SRYKBN        =   "14"
                                       OR  "21"
                                       OR  "22"
                                       OR  "23" )
              AND  (ACCT-ZAITEN        =   ZERO )
              AND  (ACCT-HKNCOMBI  NOT =   "9999")
              AND  (ACCT-SRYYM         >=  "201604")
               PERFORM 31012-GENERIC-SEC
           END-IF
           MOVE    ZERO                TO  IDX-GEN
      *
      *
           ADD     1                   TO  IDG
           IF      IDG                 >   400
               GO      TO      3101-SRYACCT-SPA-EXT
           END-IF
      *****MOVE    IDX                 TO  SPA-GMN-TBANGO  (IDG)
           MOVE    WRK-BANGO           TO  SPA-GMN-TBANGO  (IDG)
           MOVE    IDG                 TO  SPA-NAI-TBANGO  (IDX)
      *
           MOVE    ZERO                TO  WRK-IDG
      *    診療行為マスター読み込み
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *    点数検索用日付編集
           MOVE    ACCT-SRYYM          TO  WRK-SRYYMD(1:6)
           MOVE    01                  TO  WRK-SRYDD
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               IF      ACCT-DAY (1 IDX-D)  NOT =   ZERO
                   MOVE    IDX-D           TO  WRK-SRYDD
                   MOVE    31              TO  IDX-D
               END-IF
           END-PERFORM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-RAED-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
           MOVE    ZERO            TO  FLG-YAKUSOKU
      *
           MOVE    ZERO                TO  FLG-RSIGAIRAI
      *
           MOVE    ZERO                TO  WRK-HOUKNS-CNT
           MOVE    ZERO                TO  WRK-HOUKNS-IDX
           MOVE    SPACE               TO  WRK-HOUKNS-NAME
      *
           MOVE    ZERO                TO  FLG-SANTEIDATA
      *
           PERFORM UNTIL       FLG-SRYACT      =   1
      *
      *        剤内１行目
               IF      SRY-RENNUM      =   1
                   MOVE    ZERO            TO  FLG-YAKUSOKU
                   PERFORM 31011-GYO1-HEN-SEC
               END-IF
      *        剤明細編集
               PERFORM VARYING     IDZ     FROM    1   BY  1
                       UNTIL      (IDZ         >   5   )  OR
                                  (IDG         >   400 )  OR
                                  (SRY-SRYCD (IDZ) =   SPACE )
      *            自費の時、金額を表示
                   IF      SRY-SRYKBN          =   "95"  OR  "96"
                       PERFORM 31013-JIHIMEI-HEN-SEC
                   ELSE
                       PERFORM 31012-ZAIMEI-HEN-SEC
                   END-IF
      *            約束セットの時
                   IF      SRY-SRYCD (IDZ)(1:1)   =   "S"
                       MOVE    1                  TO  FLG-YAKUSOKU
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-RAED-SEC
           END-PERFORM
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *!
      *    包括検査数
           IF      WRK-HOUKNS-CNT      >   1
             AND  (IDG                 <=  400 )
               PERFORM 3009-INPUT-MEISYO-SEC
           END-IF
      *!
      *!!
      *    労災読み替え判定
           IF      FLG-RSIGAIRAI       =   ZERO
               MOVE    ZERO                TO  SPA-NAI-SYUTEN2 (IDX)
           END-IF
      *!!
      *
           IF      IDG                 >   400
               GO      TO      3101-SRYACCT-SPA-EXT
           END-IF
      *
      *    自費の時、計を表示しない
           IF      ACCT-SRYKBN         =   "95"  OR  "96"
               CONTINUE
           ELSE
      *********点数　編集 （最終行に追加）
      *        IF      SPA-GMN-HEN1 (IDG)      =   SPACE  OR  ALL "　"
      * *          CONTINUE
      *        ELSE
      *           ADD    1                    TO  IDG
      *********END-IF
               PERFORM 31013-INPUTCD-HEN-SEC
               MOVE    WRK-TENSU-X         TO  SPA-GMN-TTENSU  (IDG)
               MOVE    IDG                 TO  SPA-NAI-ENDBANGO(IDX)
      *
           END-IF
      *    剤内容をすべて編集
           MOVE    1                  TO  FLG-HEN-OK
      *
      *    剤終了編集
           ADD     1                   TO  IDG
           IF      IDG                 >   400
               CONTINUE
           ELSE
               MOVE    ALL "-"             TO  SPA-GMN-TBLREC (IDG)
           END-IF
      *
           .
       3101-SRYACCT-SPA-EXT.
           EXIT.
      *
      *****************************************************************
      *     一般名編集処理
      *****************************************************************
       31012-GENERIC-SEC        SECTION.
      *
           MOVE    ACCT-HOSPNUM        TO  ORCSGETGE-HOSPNUM
           MOVE    ACCT-PTID           TO  ORCSGETGE-PTID
           MOVE    ACCT-SRYYM          TO  ORCSGETGE-SRYYM
           MOVE    ACCT-NYUGAIKBN      TO  ORCSGETGE-NYUGAIKBN
           MOVE    ACCT-ZAINUM         TO  ORCSGETGE-ZAINUM
           MOVE    SPA-KOUHATUKA       TO  ORCSGETGE-KOUHATUKA
      *
           CALL    "ORCSGETGENERIC"    USING   ORCSGETGENERICAREA
                                               SPA-AREA
      *    後発品変更不可（処方）あり
           IF      ORCSGETGE-GENEFLG   =   "1"
               MOVE    "1"             TO  SPA-NAI-GENEFLG (IDX)
           END-IF
      *    後発品変更可（処方）あり
           IF      ORCSGETGE-GENEFLG   =   "2"
               MOVE    "2"             TO  SPA-NAI-GENEFLG (IDX)
           END-IF
           .
       31012-GENERIC-EXT.
           EXIT.
      *
      *****************************************************************
      *     包括検査数再編集処理
      *****************************************************************
       3009-INPUT-MEISYO-SEC        SECTION.
      *
           INITIALIZE                  ORCSMEIHENAREA
           MOVE    "2"                 TO  ORCSMEIHEN-KBN
           MOVE    WRK-HOUKNS-CNT      TO  ORCSMEIHEN-SURYO
           MOVE    WRK-HOUKNS-NAME     TO  ORCSMEIHEN-TNS-NAME
           CALL    "ORCSMEIHEN"        USING
                                       SPA-AREA
                                       ORCSMEIHENAREA
           IF      WRK-HOUKNS-NAME(17:)    =   SPACE
               MOVE    ORCSMEIHEN-MEISYO   TO  SPA-GMN-TMEISYO
                                                  (WRK-HOUKNS-IDX)
           ELSE
               IF      IDG             =   WRK-HOUKNS-IDX
                   ADD     1               TO  IDG
                   MOVE    IDG             TO  WRK-HOUKNS-IDX
               ELSE
                   PERFORM VARYING     IDG3    FROM    IDG   BY  -1
                           UNTIL       IDG3    <=  WRK-HOUKNS-IDX
                       MOVE    SPA-GMN-TBLREC (IDG3)
                                               TO  SPA-GMN-TBLREC
                                                           (IDG3 + 1)
                   END-PERFORM
                   COMPUTE WRK-HOUKNS-IDX  =   WRK-HOUKNS-IDX  +  1
                   IF      WRK-HOUKNS-IDX      <=   400
                       INITIALIZE                  SPA-GMN-TBLREC
                                                        (WRK-HOUKNS-IDX)
                   END-IF
                   ADD     1               TO  IDG
               END-IF
               IF      WRK-HOUKNS-IDX      <=   400
                   MOVE    ORCSMEIHEN-MEISYO2
                                       TO  SPA-GMN-TMEISYO
                                                  (WRK-HOUKNS-IDX)
               END-IF
           END-IF
      *
      *
           MOVE    ZERO                TO  WRK-HOUKNS-CNT
           MOVE    ZERO                TO  WRK-HOUKNS-IDX
           MOVE    SPACE               TO  WRK-HOUKNS-NAME
           .
       3009-INPUT-MEISYO-EXT.
           EXIT.
      *
      *****************************************************************
      *   剤内１行目  編集処理
      *****************************************************************
       31011-GYO1-HEN-SEC                  SECTION.
      *
      *     診療種別区分名称、診療行為区分セット
           SET     MSYU-IDX            TO  1
           SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT   END
                       MOVE    SPACE       TO  WRK-SRYSYUMEI
                   WHEN    SRY-SRYSYUKBN   =   MSYU-SRYSYUKBN
                                                      (MSYU-IDX)
                       MOVE    MSYU-SRYMEI (MSYU-IDX)
                                                TO  WRK-SRYSYUMEI
           END-SEARCH
      *
      *    診療種別がない時、診療行為区分より
           IF      SRY-SRYSYUKBN       =   SPACE
               SET     MSYU-IDX            TO  1
               SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-SRYSYUMEI
                   WHEN    ACCT-SRYKBN     =   MSYU-SRYKBN(MSYU-IDX)
                       MOVE    MSYU-SRYMEI (MSYU-IDX)
                                               TO  WRK-SRYSYUMEI
               END-SEARCH
           END-IF
      *
      *    包括分の名称編集
           MOVE    SPACE               TO  WRK-SRYSYUMEI2
           IF      ACCT-JIHOKBN        =   "1"
               MOVE    SPACE               TO  WRK-SRYSYUMEI2
               IF      WRK-SRYSYUMEI(15:)  =   SPACE
                   MOVE    WRK-SRYSYUMEI       TO  WRK-SRYSYUMEI2
                   MOVE    "【包括診療】"      TO  WRK-SRYSYUMEI2(15:)
               ELSE
                   STRING  WRK-SRYSYUMEI       DELIMITED  BY  SPACE
                           "【包括診療】"      DELIMITED  BY  SIZE
                                       INTO    WRK-SRYSYUMEI2
                   END-STRING
               END-IF
      *H28.9   MOVE    WRK-SRYSYUMEI2      TO  WRK-SRYSYUMEI
           END-IF
      *H28.9  持参薬
           IF      ACCT-ZAIKBN         =   3
                                       OR  4
               MOVE    SPACE               TO  WRK-SRYSYUMEI2
               IF      WRK-SRYSYUMEI(15:)  =   SPACE
                   MOVE    WRK-SRYSYUMEI       TO  WRK-SRYSYUMEI2
                   MOVE    "【持参薬】"        TO  WRK-SRYSYUMEI2(15:)
               ELSE
                   STRING  WRK-SRYSYUMEI       DELIMITED  BY  SPACE
                           "【持参薬】"        DELIMITED  BY  SIZE
                                       INTO    WRK-SRYSYUMEI2
                   END-STRING
               END-IF
           END-IF
           IF      WRK-SRYSYUMEI2  NOT =   SPACE
               MOVE    WRK-SRYSYUMEI2      TO  WRK-SRYSYUMEI
           END-IF
      *
           IF      SRY-SRYSYUKBN          =   SPACE
               MOVE    SPACE           TO  SPA-GMN-TMEISYO (IDG)
               MOVE    WRK-SRYSYUMEI   TO  SPA-GMN-TMEISYO (IDG)(7:)
           ELSE
               MOVE    "."             TO  SPA-GMN-TMEISYO (IDG)(1:1)
               MOVE    SRY-SRYSYUKBN   TO  SPA-GMN-TMEISYO (IDG)(2:3)
               MOVE    WRK-SRYSYUMEI   TO  SPA-GMN-TMEISYO (IDG)(7:)
           END-IF
      *    包括分は点数に＊を編集
           IF     (ACCT-HKNCOMBI       =   "9999" )
              OR  (ACCT-JIHOKBN        =   "1"    )
               MOVE    ALL "*"             TO  SPA-GMN-TTENSU (IDG)
           END-IF
      *
           MOVE    SRY-SRYSYUKBN       TO  SPA-NAI-SRYSYU (IDG)
      *保険組合せ
           MOVE    ACCT-HKNCOMBI       TO  SPA-GMN-THKNCOMBI (IDG)
      *!!
      *期間エラー
           IF      FLG-HKN-CHK         =   1
               IF      IDG             <   400
                   MOVE    ALL "#"     TO  SPA-GMN-THKNCOMBI (IDG + 1)
               END-IF
           END-IF
      *回数
           PERFORM VARYING    IDK      FROM    1   BY  1
                   UNTIL      IDK      >   31
      *        ３桁判定
               IF      ACCT-DAY  (1 IDK)   >=  100
                   MOVE    ACCT-DAY  (1 IDK)   TO  WRK-ZZZ
                   MOVE    WRK-ZZZ-X           TO  SPA-GMN-TDAY(IDG IDK)
               ELSE
                   MOVE    ACCT-DAY  (1 IDK)   TO  WRK-ZZ
                   MOVE    WRK-ZZ-X            TO  SPA-GMN-TDAY(IDG IDK)
               END-IF
           END-PERFORM
      *    合計
           IF      SPA-NAI-ZAIKAISU(IDX)   >=  100
               MOVE    SPA-NAI-ZAIKAISU(IDX)   TO  WRK-ZZ9
               MOVE    WRK-ZZ9-X               TO  SPA-GMN-TDAYGOK(IDG)
           ELSE
               MOVE    SPA-NAI-ZAIKAISU(IDX)   TO  WRK-Z9
               MOVE    WRK-Z9-X                TO  SPA-GMN-TDAYGOK(IDG)
           END-IF
      *
      *    診療科が全科の時、次の行へ科名称を編集
           IF     SPA-J01-SRYKA        =   ZERO
               MOVE    SPACE                   TO  WRK-SRYKAMEI
               PERFORM VARYING    IDK      FROM    1   BY  1
                       UNTIL      IDK      >   SPA-SRYKA-MAX
                   IF      ACCT-SRYKA      =   SPA-GMN-SRYKAL (IDK)
                       MOVE    SPA-GMN-SRYKAMEIL (IDK)
                                               TO  WRK-SRYKAMEI
                       MOVE    SPA-SRYKA-MAX   TO  IDK
                   END-IF
               END-PERFORM
      *H25.2
               IF      WRK-SRYKAMEI        =   SPACE
                   MOVE    ACCT-SRYKA      TO  WRK-SRYKAMEI
               END-IF
      *
               IF      IDG                 <   400
                   MOVE    WRK-SRYKAMEI(1:2)   TO  SPA-NAI-SRYKAMEI
                                                              (IDG + 1)
               END-IF
           END-IF
           .
       31011-GYO1-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   剤内明細  編集処理
      *****************************************************************
       31012-ZAIMEI-HEN-SEC                  SECTION.
      *
           ADD     1                   TO  IDG
           IF      IDG                 >   400
               GO      TO      31012-ZAIMEI-HEN-EXT
           END-IF
      *
      *H28.2
           ADD     1                   TO  IDX-GEN
      *H28.2
      *
      *    約束セット編集
           IF      SRY-SRYCD (IDZ)(1:1)   =   "S"
               PERFORM 310120-YAKUSOKU-SET-SEC
               GO      TO      31012-ZAIMEI-HEN-EXT
           END-IF
      *
      *    診療コードより診療行為名称を取得
           MOVE    SRY-SRYCD (IDZ)     TO  SPA-GMN-TSRYCD  (IDG)
           MOVE    SRY-SRYCD (IDZ)     TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU       NOT =   ZERO
               MOVE    SRY-SRYCD (IDZ)     TO  TNS-NAME
           END-IF
      *    行為名称
           MOVE    TNS-NAME            TO  SPA-GMN-TMEISYO (IDG)
      *
      *H28.5
      *    一般名記載
           IF     (ORCSGETGE-GENERICNAME(IDX-GEN)  NOT =   SPACE)
              AND (ORCSGETGE-SRYCD(IDX-GEN)    =   SRY-SRYCD (IDZ))
      *        【】の変更
               IF      ORCSGETGE-GENERICNAME(IDX-GEN)(1:6)
                                           =   "【般】"
                   MOVE    "≪般≫"        TO
                               ORCSGETGE-GENERICNAME(IDX-GEN)(1:6)
               END-IF
               MOVE    ORCSGETGE-GENERICNAME(IDX-GEN)
                                       TO  SPA-GMN-GENENAME (IDG)
           END-IF
      *
      *
      *    薬剤服用コメントの時、再編集
           IF      SRY-SRYCD (IDZ)(1:3)    =   "001"
                PERFORM 3100121-FUKIYO-MEISYO-SEC
                MOVE    WRK-MEISYO    TO  SPA-GMN-TMEISYO (IDG)
           END-IF
      *!
      *    用量割合再編集
           IF     (SRY-SRYCD (IDZ)(1:7)   =   "0992000")  AND
                  (SRY-SRYSURYO (IDZ) NOT =   ZERO     )
                PERFORM 3005-INPUT-MEISYO3-SEC
                MOVE    WRK-MEISYO    TO  SPA-GMN-TMEISYO (IDG)
           END-IF
      *!
      *    後発医薬品変更再編集
           IF      SRY-SRYCD (IDZ)     =   "099209903"
                                       OR  "099209905"
                                       OR  "099209906"
                                       OR  "099209907"
                                       OR  "099209908"
                PERFORM 3100122-KOUHATU-MEISYO-SEC
                MOVE    WRK-MEISYO    TO  SPA-GMN-TMEISYO (IDG)
           END-IF
      *!
      *    器材商品名の器材コード
           IF     (SRY-SRYCD (IDZ)(1:1)    =   "7"  )  AND
                  (SRY-MEISKYFLG (IDZ)     =   "1"  )
                PERFORM 3005-INPUT-MEISYO4-SEC
                MOVE    WRK-MEISYO    TO  SPA-GMN-TMEISYO (IDG)
           END-IF
      *!
      *
      *    約束の内容の時、再編集
           IF      FLG-YAKUSOKU        =   1
               MOVE    SPA-GMN-TMEISYO (IDG)   TO  WRK-MEISYO
               MOVE    SPACE                   TO  SPA-GMN-TMEISYO(IDG)
               STRING     "＞"             DELIMITED   BY  SIZE
                          WRK-MEISYO       DELIMITED   BY  SIZE
                          INTO             SPA-GMN-TMEISYO (IDG)
               END-STRING
      *H28.5
      *
               IF      SPA-GMN-GENENAME (IDG) NOT =   SPACE
                   MOVE    SPA-GMN-GENENAME (IDG)   TO  WRK-MEISYO
                   MOVE    SPACE               TO  SPA-GMN-GENENAME(IDG)
                   STRING "＞"             DELIMITED   BY  SIZE
                          WRK-MEISYO       DELIMITED   BY  SIZE
                          INTO             SPA-GMN-GENENAME (IDG)
                   END-STRING
               END-IF
           END-IF
      *
      *    コメントの時、内容設定
           IF    ((SRY-SRYCD  (IDZ)(1:1)   =   "8"  )  OR
                  (SRY-SRYCD  (IDZ)(1:3)   =   "008"))  AND
                  (SRY-INPUTNUM(IDZ)   NOT =   ZERO  )
               PERFORM 310122-KNJCOM-SET-SEC
           END-IF
      *H30.4
      *    コメントの時、内容設定
           IF     (SRY-SRYCD  (IDZ)(1:7)   =   "0992081" )
             AND  (SRY-INPUTNUM(IDZ)   NOT =   ZERO  )
               PERFORM 310122-KNJCOM-SET-SEC
           END-IF
      *
      *    数量（薬剤、器材のとき）
           IF    ((SRY-SRYCD  (IDZ)(1:1)   =   "6"  OR  "7" )
               OR (SRY-SRYCD  (IDZ)(1:3)   =   "059"   ))
             AND  (ACCT-ZAIKBN         NOT =   1            )
               MOVE    SRY-SRYSURYO (IDZ)  TO  WRK-SURYO
               PERFORM 310121-SURYO-HEN-SEC
               MOVE    WRK-SURYO-X         TO  SPA-GMN-TSURYOU (IDG)
      *********MOVE    TNS-TANINAME        TO  SPA-GMN-TTANI1  (IDG)
           END-IF
      *    分画数
           IF     (SRY-SRYCD  (IDZ)(1:1)   =   "7"  )  AND
                  (TNS-DATAKBN             =   3    )  AND
                  (SRY-SRYKAISU (IDZ)      >   ZERO )
               MOVE    SRY-SRYSURYO (IDZ)  TO  WRK-BUNGSU-Z1
               MOVE    SRY-SRYKAISU (IDZ)  TO  WRK-BUNGSU-Z2
               MOVE    SPACE               TO  SPA-GMN-TSURYOU (IDG)
               STRING  WRK-BUNGSU-Z1           DELIMITED   BY  SIZE
                       "/"                     DELIMITED   BY  SIZE
                       WRK-BUNGSU-Z2           DELIMITED   BY  SIZE
                       INTO                    SPA-GMN-TSURYOU (IDG)
               END-STRING
      *********PERFORM 310122-BUNGSU-HEN-SEC
      *        MOVE    WRK-BUNGSU-Z        TO  SPA-GMN-TTANMEI (IDG)
      *********MOVE    "分画"              TO  SPA-GMN-TTANI2  (IDG)
           END-IF
      *
      *    きざみ値（撮影回数）
           IF     (SRY-SRYCD  (IDZ)(1:1)   =   "1" )  AND
                  (SRY-AUTOKBN(IDZ)        =   "S"
                                           OR  "4"  )  AND
                  (SRY-SRYSURYO (IDZ)      >   1   )
      *        減点以外
             AND  (TNS-TEN                 >   ZERO )
             AND  (TNS-TENSIKIBETU     NOT =   7  AND 8  )
               MOVE    SRY-SRYSURYO (IDZ)  TO  WRK-SURYO
               PERFORM 310121-SURYO-HEN-SEC
               MOVE    WRK-SURYO-X         TO  SPA-GMN-TSURYOU (IDG)
           END-IF
      *!
      *    包括検査の判定
           IF     (SRY-SRYCD  (IDZ)(1:1)   =   "1" )  AND
                  (SRY-SRYKBN              =   "60" ) AND
                  (TNS-DATAKBN             =   1    ) AND
                  (TNS-HOUKNSKBN       NOT =   ZERO )
               ADD     1                   TO  WRK-HOUKNS-CNT
               MOVE    IDG                 TO  WRK-HOUKNS-IDX
               MOVE    SPA-GMN-TMEISYO(IDG)    TO  WRK-HOUKNS-NAME
           END-IF
      *!
      *
      *    老人適用区分（同一剤は同じ）
           IF      TNS-ROUTEKKBN       =   ZERO
               IF      SPA-NAI-ROUTEKKBN(IDX)  =   ZERO
                   MOVE    TNS-ROUTEKKBN   TO  SPA-NAI-ROUTEKKBN(IDX)
               END-IF
           ELSE
               MOVE    TNS-ROUTEKKBN       TO  SPA-NAI-ROUTEKKBN(IDX)
           END-IF
      *    点数識別が減額の時マイナス
           IF      TNS-TENSIKIBETU     =   7  OR  8
               IF      IDZ                 =   1
                   MOVE    1               TO  SPA-NAI-ZAITENFLG (IDX)
               END-IF
           END-IF
      *    内服の逓減の時、マイナス
           IF      SRY-SRYCD  (IDZ)    =   "630010002"
               MOVE    1                   TO  SPA-NAI-ZAITENFLG (IDX)
           END-IF
      *    内服の逓減の対象分
           IF     (SRY-SRYCD  (IDZ)    =   "820000047" )  AND
                  (SRY-SRYKBN          =   "21"        )  AND
                  (SRY-AUTOKBN (IDZ)   =   "3"         )
               MOVE    2                   TO  SPA-NAI-ZAITENFLG (IDX)
           END-IF
      *H26.10
      *    向精神薬剤逓減の対象（精減）
           IF     (SRY-SRYCD  (IDZ)    =   "820000166" )  AND
                  (SRY-SRYKBN          =   "21"
                                       OR  "22"
                                       OR  "23"        )  AND
                  (SRY-AUTOKBN (IDZ)   =   "3"         )
               MOVE    2                   TO  SPA-NAI-ZAITENFLG (IDX)
           END-IF
      *H28.4
      *    外用院内の湿布薬
           IF     (SRY-SRYYM           >=  "201604" )
              AND (SPA-NYUGAIKBN       =   "2"      )
              AND (SRY-SRYKBN          =   "23"     )
              AND (SRY-SRYCD (IDZ)     =   CONS-SPCM-SRYCD)
              AND (SRY-AUTOKBN (IDZ)   =   "3"      )
               MOVE    2                   TO  SPA-NAI-ZAITENFLG (IDX)
           END-IF
      *
      *    算定履歴情報（剤内の最初に対象となるもの）
      *H27.9       手技料を優先
           IF     (TNS-SRYCD(1:1)      =   "1"  )  AND
                  (TNS-DATAKBN         =   "1"
                                       OR  "2"  )  AND
                  (FLG-SANTEIDATA      =   ZERO )  AND
                  (TNS-SANTEIRRKKBN    NOT  =  ZERO )
               MOVE    TNS-SANTEIRRKKBN    TO  SPA-NAI-SANTEIRRKKBN(IDX)
               MOVE    TNS-JGNCNT          TO  SPA-NAI-JGNCNT    (IDX)
               MOVE    TNS-JGNCNT1D        TO  SPA-NAI-JGNCNT1D  (IDX)
               MOVE    TNS-JGNCNT1W        TO  SPA-NAI-JGNCNT1W  (IDX)
               MOVE    TNS-JGNCNTETC       TO  SPA-NAI-JGNCNTETC (IDX)
               MOVE    TNS-JGNCNTERR       TO  SPA-NAI-JGNCNTERR (IDX)
      *        内分泌負荷試験判定
               MOVE    TNS-HOUKNSKBN       TO  SPA-NAI-HOUKNSKBN (IDX)
      *        数量
               MOVE    SRY-SRYSURYO(IDZ)   TO  SPA-NAI-TSURYO (IDX)
      *H27.9
               IF      TNS-DATAKBN         =   "1"
                   MOVE    1               TO  FLG-SANTEIDATA
               END-IF
           END-IF
      *!!
      *    入院調剤料の自動発生分
           IF     (SRY-NYUGAIKBN       =   "1"         )  AND
                  (SRY-SRYKBN          =   "24"
                                       OR  "26"        )  AND
                  (SRY-SRYCD  (IDZ)    =   "120001110"
                                       OR  "120000410" )  AND
                  (SRY-AUTOKBN (IDZ)   =   "3"         )
               MOVE    1               TO  SPA-NAI-NYUAUTO  (IDX)
            END-IF
      *!!
      *
      *    労災読み替え点数判定
           IF     (SRY-NYUGAIKBN       =   "2"   )  AND
                  (SRY-SRYCD(IDZ)(1:3) =   "101" )  AND
                  (TNS-SRYKBN          =   "12"  )  AND
                  (TNS-GAIKANRIKBN     =   2     )
               MOVE    TNS-TEN             TO  WRK-RSAI-GAIRAITEN
           END-IF
      *
           IF     (SRY-NYUGAIKBN       =   "2"   )  AND
                  (SRY-SRYSYUKBN   NOT =   "409" )  AND
                  (SRY-SRYCD(IDZ)(1:1)
                                       =   "1"   )  AND
                  (TNS-DATAKBN         =   "1"   )  AND
                  (TNS-GAIKANRIKBN     =   1     )  AND
                  (SPA-NAI-SYUTEN2(IDX)    >   ZERO )
      *H25.3
               IF     ((TNS-TEN            =   SPA-NAI-SYUTEN2(IDX)) AND
                       (TNS-TEN            <   WRK-RSAI-GAIRAITEN ))
      *          読み替え手技区分対応
                 OR   ( SRY-MEISKYFLG (IDZ) =   "R"    )
                   MOVE    1               TO  FLG-RSIGAIRAI
               END-IF
           END-IF
      *!!!
      *    コメント以外の最終行セット
           IF      SRY-SRYCD  (IDZ)(1:1)   =   "0"  OR  "8"
               CONTINUE
           ELSE
               MOVE    IDG             TO  WRK-IDG
           END-IF
      *
           .
       31012-ZAIMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   剤内明細（自費）  編集処理
      *****************************************************************
       31013-JIHIMEI-HEN-SEC           SECTION.
      *
           ADD     1                   TO  IDG
           IF      IDG                 >   400
               GO      TO      31013-JIHIMEI-HEN-EXT
           END-IF
      *
      *    約束セット編集
           IF      SRY-SRYCD (IDZ)(1:1)   =   "S"
               PERFORM 310120-YAKUSOKU-SET-SEC
               GO      TO      31013-JIHIMEI-HEN-EXT
           END-IF
      *
      *    診療コードより診療行為名称を取得
           MOVE    SRY-SRYCD (IDZ)     TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU       NOT =   ZERO
               MOVE    SRY-SRYCD (IDZ)     TO  TNS-NAME
           END-IF
      *    行為名称
      *****MOVE    TNS-NAME            TO  SPA-GMN-TMEISYO2(IDG)
           MOVE    TNS-NAME            TO  SPA-GMN-TMEISYO (IDG)
      *
      *    約束の内容の時、再編集
           IF      FLG-YAKUSOKU        =   1
               MOVE    SPA-GMN-TMEISYO (IDG)   TO  WRK-MEISYO
               MOVE    SPACE               TO  SPA-GMN-TMEISYO (IDG)
               STRING     "＞"             DELIMITED   BY  SIZE
                          WRK-MEISYO       DELIMITED   BY  SIZE
                          INTO             SPA-GMN-TMEISYO (IDG)
               END-STRING
           END-IF
      *
      *    コメントの時、内容設定
           IF    ((SRY-SRYCD  (IDZ)(1:1)   =   "8"   ) OR
                  (SRY-SRYCD  (IDZ)(1:3)   =   "008" )) AND
                  (SRY-INPUTNUM(IDZ)   NOT =   ZERO  )
               PERFORM 310122-KNJCOM-SET-SEC
           END-IF
      *H30.4
      *    コメントの時、内容設定
           IF     (SRY-SRYCD  (IDZ)(1:7)   =   "0992081" )
             AND  (SRY-INPUTNUM(IDZ)   NOT =   ZERO  )
               PERFORM 310122-KNJCOM-SET-SEC
           END-IF
      *
      *    計
           MOVE    SRY-JIHIMONEY (IDZ)
                                       TO  WRK-KEI
           MOVE    SPACE               TO  WRK-ZAIKEI
           MOVE    WRK-KEI-X           TO  WRK-ZAIKEI(10:)
      *
           IF      SRY-JIHIMONEY (IDZ)     =   ZERO
               MOVE    SPACE               TO  SPA-GMN-TTENSU (IDG)
           ELSE
      ******** MOVE    "円"                TO  WRK-ZAIKEIJ(37:2)
      *******  MOVE    WRK-ZAIKEI          TO  SPA-GMN-TZAIKEI (IDG)
               MOVE    WRK-KEI-X           TO  SPA-GMN-TTENSU (IDG)
           END-IF
      *
      *    コメント以外の最終行セット
           IF      SRY-SRYCD (IDZ)(1:1)    =   "0"  OR  "8"
               CONTINUE
           ELSE
               MOVE    IDG             TO  WRK-IDG
           END-IF
      *
           .
       31013-JIHIMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *     約束セットの時、再編集処理
      *****************************************************************
       310120-YAKUSOKU-SET-SEC        SECTION.
      *
           MOVE    SPACE               TO  INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
      *    セットの桁数は６とする
           MOVE    "6"                 TO  ICD-CDSYU
           MOVE    SRY-SRYCD (IDZ)(1:6)
                                       TO  ICD-INPUTCD
           MOVE    "%"                 TO  ICD-INPUTCD(7:1)
      *
           PERFORM 930-INPUTCD-READ-SEC
      *
           IF      FLG-INPUTCD         =   ZERO
               MOVE    ICD-DSPNAME         TO  SPA-GMN-TMEISYO (IDG)
           ELSE
               MOVE    SRY-SRYCD (IDZ)     TO  SPA-GMN-TMEISYO (IDG)
           END-IF
      *
      *    数量（薬剤、器材のとき）
           MOVE    SRY-SRYSURYO (IDZ)  TO  WRK-SURYO
           PERFORM 310121-SURYO-HEN-SEC
           MOVE    WRK-SURYO-X         TO  SPA-GMN-TSURYOU (IDG)
      *
           .
       310120-YAKUSOKU-SET-EXT.
           EXIT.
      *****************************************************************
      *     薬剤服用コメントの時、再編集処理
      *****************************************************************
       3100121-FUKIYO-MEISYO-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-MEISYO
      *
           STRING     "【"             DELIMITED   BY  SIZE
                      TNS-NAME         DELIMITED   BY  "    "
                      "】"             DELIMITED   BY  SIZE
                      INTO             WRK-MEISYO
           END-STRING
      *    埋め込み有
           IF     (SRY-INPUTNUM(IDZ)   NOT =   ZERO)
               PERFORM 310122-KNJCOM-SET-SEC
               MOVE    SPACE               TO  WRK-MEISYO
               STRING     "【"             DELIMITED   BY  SIZE
                          SPA-GMN-TMEISYO (IDG)
                                           DELIMITED   BY  SPACE
                          "】"             DELIMITED   BY  SIZE
                          INTO             WRK-MEISYO
               END-STRING
               MOVE    WRK-MEISYO          TO  SPA-GMN-TMEISYO (IDG)
           END-IF
           .
       3100121-FUKIYO-MEISYO-EXT.
           EXIT.
      *****************************************************************
      *     後発医薬品変更再編集処理
      *****************************************************************
       3100122-KOUHATU-MEISYO-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-MEISYO
      *
           STRING     "【"             DELIMITED   BY  SIZE
                      TNS-NAME         DELIMITED   BY  "    "
                      "】"             DELIMITED   BY  SIZE
                      INTO             WRK-MEISYO
           END-STRING
           .
       3100122-KOUHATU-MEISYO-EXT.
           EXIT.
      *****************************************************************
      *     用量割合再編集処理
      *****************************************************************
       3005-INPUT-MEISYO3-SEC        SECTION.
      *
           INITIALIZE                  ORCSMEIHENAREA
           MOVE    "1"                 TO  ORCSMEIHEN-KBN
           MOVE    SRY-SRYSURYO (IDZ)  TO  ORCSMEIHEN-SURYO
           MOVE    SRY-SRYCD (IDZ)     TO  ORCSMEIHEN-SRYCD
           MOVE    TNS-NAME            TO  ORCSMEIHEN-TNS-NAME
           CALL    "ORCSMEIHEN"        USING
                                       SPA-AREA
                                       ORCSMEIHENAREA
           MOVE    ORCSMEIHEN-MEISYO   TO  WRK-MEISYO
           .
       3100121-FUKIYO-MEISYO-EXT.
           EXIT.
      *****************************************************************
      *    器材商品名コードの器材編集
      *****************************************************************
       3005-INPUT-MEISYO4-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-MEISYO
      *
           STRING     "〔"             DELIMITED   BY  SIZE
                      TNS-NAME         DELIMITED   BY  "    "
                      "〕"             DELIMITED   BY  SIZE
                      INTO             WRK-MEISYO
           END-STRING
           .
       3005-INPUT-MEISYO4-EXT.
           EXIT.
      *
      *****************************************************************
      *    数量表示用編集処理
      *****************************************************************
       310121-SURYO-HEN-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-SURYO-X
           MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1
      *
           MOVE    ZERO                TO  FLG-SP
           MOVE    ZERO                TO  WRK-SURYO-Z3
           PERFORM VARYING     IDM     FROM    5  BY  -1
                   UNTIL       IDM     <   1
               IF      WRK-SURYO-S2(IDM:1) NOT =   ZERO
                   MOVE    1               TO  FLG-SP
               END-IF
               IF      FLG-SP          =   1
                   MOVE    WRK-SURYO-S2(IDM:1) TO  WRK-SURYO-Z3(IDM:1)
               END-IF
           END-PERFORM
      *
           IF      WRK-SURYO-Z3-X      NOT =   SPACE
               MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z19
               MOVE    "."                 TO  WRK-SURYO-Z2
           END-IF
      *
           .
       310121-SURYO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメントから編集する 
      *****************************************************************
       310122-KNJCOM-SET-SEC              SECTION.
      *
           INITIALIZE                      PTCOM-REC
           MOVE    SRY-HOSPNUM         TO  PTCOM-HOSPNUM
           MOVE    SRY-PTID            TO  PTCOM-PTID
           MOVE    SRY-ZAINUM          TO  PTCOM-ZAINUM
           MOVE    SRY-SRYCD (IDZ)     TO  PTCOM-SRYCD
           MOVE    SRY-INPUTNUM(IDZ)   TO  PTCOM-RENNUM
           PERFORM 950-PTCOM-READ-SEC
           IF      FLG-PTCOM           =   ZERO
               IF      PTCOM-INPUTCOMENT  NOT =   SPACE
                   MOVE    PTCOM-INPUTCOMENT  TO  SPA-GMN-TMEISYO (IDG)
               END-IF
               MOVE    PTCOM-INPUTCHI (1)  TO  SPA-NAI-INPUTCHI1(IDG)
               MOVE    PTCOM-INPUTCHI (2)  TO  SPA-NAI-INPUTCHI2(IDG)
               MOVE    PTCOM-INPUTCHI (3)  TO  SPA-NAI-INPUTCHI3(IDG)
               MOVE    PTCOM-INPUTCHI (4)  TO  SPA-NAI-INPUTCHI4(IDG)
               MOVE    PTCOM-INPUTCHI (5)  TO  SPA-NAI-INPUTCHI5(IDG)
           END-IF
      *
           .
       310122-KNJCOM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   入力コードの画面用再編集処理
      *****************************************************************
       31013-INPUTCD-HEN-SEC                  SECTION.
      *
      *    点数＊回数　計
           MOVE    SPA-NAI-ZAITEN  (IDX)   TO  WRK-TENSU
           IF      SPA-NAI-ZAITENFLG (IDX) =   1
               COMPUTE WRK-TENSU-2         =   SPA-NAI-ZAITEN  (IDX)
                                           *   -1
           END-IF
      *    院外処方
           IF     (SPA-NAI-ZAITEN  (IDX)   =   ZERO )
             AND  (SPA-NAI-INGAITEN(IDX)   >   ZERO )
               IF      SPA-NAI-INGAITEN(IDX)   <   100000
                   MOVE    SPA-NAI-INGAITEN(IDX)   TO  WRK-TENSU-G
                   MOVE    "("                     TO  WRK-TENSU-G1
                   MOVE    ")"                     TO  WRK-TENSU-G2
               ELSE
                   MOVE    SPA-NAI-INGAITEN(IDX)   TO  WRK-TENSU-GR
                   MOVE    ")"                     TO  WRK-TENSU-GR2
               END-IF
               MOVE    WRK-TENSU-GX            TO  WRK-TENSU-X
           END-IF
      *
           .
       31013-INPUTCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
      *    画面ＳＰＡ書込み
           PERFORM 1003-J02SPA-OUT-SEC
      *
      *    コラムリスト位置保存
           IF      FLG-GMN-INIT        =   1
               MOVE    ZERO                TO  WRK-J02-DATALIST-ROW
               MOVE    ZERO                TO  WRK-J02-RRKLIST-ROW
           ELSE
               MOVE    J02-DATALIST-ROW    TO  WRK-J02-DATALIST-ROW
               MOVE    J02-DATALIST-ROWATTR
                                           TO  WRK-J02-DATALIST-ROWATTR
               MOVE    J02-RRKLIST-ROW     TO  WRK-J02-RRKLIST-ROW
               MOVE    J02-RRKLIST-ROWATTR TO  WRK-J02-RRKLIST-ROWATTR
           END-IF
      *
           MOVE    SPACE               TO  J02
           INITIALIZE                      J02
      *
      *    コラムリストの位置指定
      *    MOVE    ZERO                TO  J02-DATALIST-ROW
      *    MOVE    ZERO                TO  J02-DATALIST-ROWATTR
      *    MOVE    ZERO                TO  J02-RRKLIST-ROW
      *    MOVE    ZERO                TO  J02-RRKLIST-ROWATTR
           IF      WRK-J02-DATALIST-ROW    NOT =   ZERO
               MOVE    WRK-J02-DATALIST-ROW    TO  J02-DATALIST-ROW
               MOVE    WRK-J02-DATALIST-ROWATTR
                                               TO  J02-DATALIST-ROWATTR
           END-IF
           IF      MAE-J02-DATALIST-ROW    NOT =   ZERO
               MOVE    MAE-J02-DATALIST-ROW
                                           TO  J02-DATALIST-ROW
               MOVE    2                   TO  J02-DATALIST-ROWATTR
           END-IF
           IF      WRK-J02-RRKLIST-ROW     NOT =   ZERO
               MOVE    WRK-J02-RRKLIST-ROW     TO  J02-RRKLIST-ROW
               MOVE    WRK-J02-RRKLIST-ROWATTR TO  J02-RRKLIST-ROWATTR
           END-IF
      *
           MOVE    SPA-GMN-PTNUM       TO  J02-PTNUM
           MOVE    SPA-GMN-KANANAME    TO  J02-KANANAME
           MOVE    SPA-GMN-NAME        TO  J02-NAME
      *    排他中
           IF      SPA-LOCK                =   ZERO
               MOVE    "black"             TO  J02-NAME-STYLE
               MOVE    "black"             TO  J02-KANANAME-STYLE
           ELSE
               MOVE    "red"               TO  J02-NAME-STYLE
               MOVE    "red"               TO  J02-KANANAME-STYLE
           END-IF
           MOVE    SPA-GMN-SEX         TO  J02-SEX
           MOVE    SPA-GMN-SRYYM       TO  J02-SRYYM
           MOVE    SPA-GMN-BIRTHDAYW   TO  J02-BIRTHDAY
      *****MOVE    SPA-SRYKAMEI        TO  J02-SRYKA
           MOVE    SPA-GMN-SRYKA-G     TO  J02-SRYKA
      *    診療科
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-SRYKA-MAX
               MOVE    SPA-GMN-SRYKA-LIST (IDX)
                                          TO  J02-SRYKA-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-SRYKA-MAX       TO  J02-SRYKA-COUNT
      *    入外区分
           MOVE    SPA-GMN-NYUGAIKBN-G TO  J02-NYUGAIKBN
           PERFORM VARYING   IDX     FROM    1   BY  1
                   UNTIL     IDX         >   SPA-NYUGAI-MAX
               MOVE    SPA-GMN-NYUGAIKBN-LST (IDX)
                                          TO  J02-NYUGAIKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-NYUGAI-MAX      TO  J02-NYUGAIKBN-COUNT
      *    診療種別
           MOVE    SPA-GMN-SRYKBN-G    TO  J02-SRYKBN
           PERFORM VARYING   IDX     FROM    1   BY  1
                   UNTIL     IDX         >   SPA-SRYKBN-MAX
               MOVE    SPA-GMN-SRYKBN-LST (IDX)
                                          TO  J02-SRYKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-SRYKBN-MAX         TO  J02-SRYKBN-COUNT
      *
      *    保険組合せ
           MOVE    SPA-GMN-HHKNCOMBI-G TO  J02-HHKNCOMBI
           PERFORM VARYING   IDX     FROM    1   BY  1
                   UNTIL     IDX         >   SPA-HKN-MAX
               MOVE    SPA-GMN-THKNCOMBINUM-G (IDX)
                                          TO  J02-HHKNCOMBI-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-HKN-MAX         TO  J02-HHKNCOMBI-COUNT
      *
      *    受診履歴
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-RRK-MAX
               MOVE    SPA-GMN-RRKNO  (IDX)    TO  WRK-ZZZ
               MOVE    WRK-ZZZ-X               TO  J02-RRKNO   (IDX)
               MOVE    SPA-GMN-RRKYMD (IDX)    TO  J02-RRKYMD  (IDX)
               IF     (SPA-NAI-RRKSELNUM       >   ZERO   )   AND
                      (SPA-GMN-RRKNO  (IDX)    =   SPA-NAI-RRKSELNUM)
                   MOVE    "T"             TO  J02-RRKLIST-SELECT(IDX)
               ELSE
                   MOVE    "F"             TO  J02-RRKLIST-SELECT(IDX)
               END-IF
      *        全科の時科指定
               IF      SPA-J01-SRYKA       =   ZERO
      *            MOVE    SPA-GMN-RRKSRYKA(IDX)  TO  J02-RRKYMD  (IDX)
      *                                                      (17:4)
      *************MOVE    SPA-GMN-RRKSRYKA(IDX)  TO  J02-RRKSRYKA(IDX)
                   MOVE    SPA-GMN-RRKSRYKA(IDX)  TO  J02-RRKSRYKA(IDX)
               END-IF
      *        保険組合せ
               MOVE    SPA-GMN-RRKHKNCOMBI (IDX)  TO  J02-RRKHKNCOMBI
                                                                   (IDX)
           END-PERFORM
           MOVE    SPA-RRK-MAX         TO  J02-RRKLIST-COUNT
      *
           MOVE    SPA-GMN-RRKSELNUM   TO  J02-RRKSELNUM
      *    訂正日
           MOVE    SPA-GMN-HENSRYYMD   TO  J02-HENSRYYMD
      *
      *    Ｊ０４からの時、変更した剤を表示する
           MOVE    ZERO                TO  WRK-BANGO
           IF      WRK-MAE-ZAINUM      NOT =   ZERO
               PERFORM 5001-J04SELECT-SEC
           END-IF
      *    明細
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-GMN-MAX
               IF      SPA-GMN-TMEISYO (IDX)   =   ALL "-"
                   MOVE    ALL "-"         TO  J02-DATALIST-ITEM(IDX)
                   MOVE    SPACE           TO  J02-TMEISYO (IDX)
                   MOVE    ALL "-"         TO  J02-TMEISYO (IDX)(1:30)
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL       IDY     >   31
                       MOVE    SPACE           TO  J02-TDAY (IDX IDY)
                       MOVE    "--"            TO  J02-TDAY (IDX IDY)
                   END-PERFORM
               ELSE
                   IF      SPA-GMN-TBANGO (IDX)    >   ZERO
                       MOVE    SPA-GMN-TBANGO  (IDX)   TO  WRK-ZZZZ
                       MOVE    WRK-ZZZZ-X          TO  J02-TNO    (IDX)
                   END-IF
                   IF      SPA-NAI-SRYKAMEI(IDX)   NOT =   SPACE
                       MOVE    SPA-NAI-SRYKAMEI(IDX)
                                                   TO  J02-TNO    (IDX)
                   END-IF
      *
                   MOVE    SPA-GMN-TMEISYO (IDX)   TO  J02-TMEISYO(IDX)
      *H28.5
      *            一般名
                   IF     (SPA-GMN-GENENAME (IDX)  NOT =   SPACE)
                    AND   (SPA-GENERICFLG          =   "1"  )
                       MOVE    SPA-GMN-GENENAME (IDX)
                                                   TO  J02-TMEISYO(IDX)
                   END-IF
      *
                   MOVE    SPA-GMN-TSURYO  (IDX)   TO  J02-TSURYO (IDX)
                   MOVE    SPA-GMN-TTENSU  (IDX)   TO  J02-TTENSU (IDX)
                   MOVE    SPA-GMN-THKNCOMBI(IDX)  TO  J02-THKNCOMBI
                                                                 (IDX)
                   MOVE    SPA-GMN-TDAYGOK (IDX)   TO  J02-TDAYGOK(IDX)
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL       IDY     >   31
                       MOVE    SPA-GMN-TDAY (IDX IDY)
                                                   TO  J02-TDAY
                                                             (IDX IDY)
                   END-PERFORM
      ***********  入院で調剤料の時、＊表示をする
      *            IF     (SPA-NAI-SRYYM       >=  SPA-SYSYMD(1:6)) AND
      *                   (SPA-GMN-NYUGAIKBN   =   "1" )  AND
      *                   (SPA-GMN-TMEISYO (IDX)(2:3)
      *                                        =   "240" OR "260")
      *                MOVE    ALL "*"         TO  J02-TDAY-G (IDX)
      *************END-IF
               END-IF
      *
               IF     (SPA-GMN-TMEISYO (IDX)   NOT =   ALL "-" )  AND
                      (SPA-GMN-TBANGO(IDX) NOT =   ZERO  )  AND
                      (SPA-GMN-TBANGO(IDX)     =   SPA-GMN-BANGO )
                   MOVE    "T"             TO  J02-DATALIST-SELECT(IDX)
                   MOVE    IDX             TO  J02-DATALIST-ROW
                   MOVE    2               TO  J02-DATALIST-ROWATTR
               ELSE
                   MOVE    "F"             TO  J02-DATALIST-SELECT(IDX)
               END-IF
      *        Ｊ０４からのリスト表示位置
               IF      SPA-GMN-TMEISYO (IDX)   =   ALL "-"
                   CONTINUE
               ELSE
                  IF      (SPA-GMN-BANGO           =   ZERO  )  AND
                          (SPA-GMN-TBANGO(IDX) NOT =   ZERO  )  AND
                          (SPA-GMN-TBANGO(IDX)     =   WRK-BANGO )
                       MOVE    IDX             TO  J02-DATALIST-ROW
                       MOVE    2               TO  J02-DATALIST-ROWATTR
                   END-IF
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-MAX         TO  J02-DATALIST-COUNT
      *
           IF      SPA-GMN-BANGO   NOT =   ZERO
               MOVE    SPA-GMN-BANGO       TO  WRK-ZZZZ
               MOVE    WRK-ZZZZ-X          TO  J02-NUM
           END-IF
      *
           MOVE    SPA-GMN-MEISYO      TO  J02-MEISYO
      *
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   31
               IF      SPA-GMN-DAY (IDX)    <   100
                   MOVE    SPA-GMN-DAY (IDX)    TO  WRK-ZZ
                   MOVE    WRK-ZZ-X             TO  J02-DAY(IDX)
               ELSE
                   MOVE    SPA-GMN-DAY (IDX)    TO  J02-DAY(IDX)
               END-IF
           END-PERFORM
      *
           IF      SPA-GMN-HKNCOMBI   NOT =   ZERO
               MOVE    SPA-GMN-HKNCOMBI   TO  J02-HKNCOMBI
           END-IF
           MOVE    SPA-GMN-HKNCOMBIMEI    TO  J02-HKNCOMBIMEI
      *
           MOVE    SPA-GMN-BSRYKAMEI      TO  J02-SRYKAMEI
      *    入院の時、入院日
           IF      SPA-GMN-NYUGAIKBN   =   "1"
               MOVE    "入院期間："       TO  J02-NYUINKIKAN
               IF     (SPA-GMN-NYUINYMD   NOT =   SPACE )  AND
                      (SPA-GMN-TAIINYMD   NOT =   SPACE )
                   MOVE    SPA-GMN-NYUINYMD   TO  J02-NYUINKIKAN(11:9)
                   MOVE    "-"                TO  J02-NYUINKIKAN(20:1)
                   MOVE    SPA-GMN-TAIINYMD   TO  J02-NYUINKIKAN(21:9)
               END-IF
           ELSE
               MOVE    SPACE              TO  J02-NYUINKIKAN
               IF     (SPA-GMN-NYUINYMD   NOT =   SPACE )  AND
                      (SPA-GMN-TAIINYMD   NOT =   SPACE )
                   STRING  "今月入院期間：" 
                           SPA-GMN-NYUINYMD
                           "-" 
                           SPA-GMN-TAIINYMD   DELIMITED  BY  SIZE
                                           INTO   J02-NYUINKIKAN
                   END-STRING
               END-IF
               IF     (SPA-GMN-NYUINYMD       =   SPACE )  AND
                      (SPA-GMN-TAIINYMD   NOT =   SPACE )
                   STRING  "最終退院日：" 
                           SPA-GMN-TAIINYMD   DELIMITED  BY  SIZE
                                           INTO   J02-NYUINKIKAN
                   END-STRING
               END-IF
           END-IF
      *
           MOVE    SPA-GMN-IKKATU      TO  J02-IKKATU
      *
           MOVE    SPA-GMN-NYUINMSG(1) TO  J02-NYUINMSG1-VALUE
           MOVE    SPA-GMN-NYUINMSG(2) TO  J02-NYUINMSG2-VALUE
      *
           MOVE    "red"               TO  J02-NYUINMSG1-STYLE
           MOVE    "red"               TO  J02-NYUINMSG2-STYLE
      *
      *    保険組合せ期間
           MOVE    SPA-GMN-HKNCOMBMSG TO  J02-HKNCOMBMSG-VALUE
           MOVE    "black"            TO  J02-HKNCOMBMSG-STYLE
      *
      *
      *    入院料包括
           MOVE    SPA-GMN-HKTMSG     TO  J02-HKTMSG-VALUE
           MOVE    "red"              TO  J02-HKTMSG-STYLE
      *
      *    収納更新メッセージ
           MOVE    SPA-GMN-SYUMSG     TO  J02-SYUMSG-VALUE
           MOVE    "blue"             TO  J02-SYUMSG-STYLE
      *
      *    非活性処理
           PERFORM 510-GSRAUTH-SEC
      *    カーソル
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   剤表示位置処理
      *****************************************************************
       5001-J04SELECT-SEC             SECTION.
      *    Ｊ０４からの時、変更した剤を表示する
           PERFORM   VARYING   IDG     FROM    1   BY  1
                     UNTIL     IDG         >   SPA-GMN-MAX
               IF      SPA-NAI-ZAINUM (IDG)    =   WRK-MAE-ZAINUM
                   MOVE    SPA-NAI-TBANGO  (IDG)   TO  IDX
                   MOVE    SPA-GMN-TBANGO (IDX)    TO  WRK-BANGO
               END-IF
           END-PERFORM
           .
       5001-J04SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *   非活性処理
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
      *    履歴選択
      *    MOVE    WIDGET-INSENSITIVE  TO  J02-RRKSELNUM-STATE
      *
      *    IF      SPA-J02-SYORI       =   1  OR  2
      *        MOVE    WIDGET-NORMAL       TO  J02-RRKSELNUM-STATE
      *    END-IF
      *
      *    訂正日入力
      *    MOVE    WIDGET-INSENSITIVE  TO  J02-HENSRYYMD-STATE
      *
      *    IF      SPA-J02-SYORI       =   2
      *        MOVE    WIDGET-NORMAL       TO  J02-HENSRYYMD-STATE
      *    END-IF
      *    排他中
           IF      SPA-LOCK            =   ZERO
               MOVE    WIDGET-NORMAL       TO  J02-B04-STATE
               MOVE    WIDGET-NORMAL       TO  J02-B05-STATE
               MOVE    WIDGET-NORMAL       TO  J02-B08-STATE
               MOVE    WIDGET-NORMAL       TO  J02-B12-STATE
               MOVE    WIDGET-NORMAL       TO  J02-B02S-STATE
               MOVE    WIDGET-NORMAL       TO  J02-B04S-STATE
               MOVE    WIDGET-NORMAL       TO  J02-B09S-STATE
               MOVE    WIDGET-NORMAL       TO  J02-B10S-STATE
               MOVE    WIDGET-NORMAL       TO  J02-B11S-STATE
               MOVE    WIDGET-NORMAL       TO  J02-B01S-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  J02-B04-STATE
               MOVE    WIDGET-INSENSITIVE  TO  J02-B05-STATE
               MOVE    WIDGET-INSENSITIVE  TO  J02-B08-STATE
               MOVE    WIDGET-INSENSITIVE  TO  J02-B12-STATE
               MOVE    WIDGET-INSENSITIVE  TO  J02-B02S-STATE
               MOVE    WIDGET-INSENSITIVE  TO  J02-B04S-STATE
               MOVE    WIDGET-INSENSITIVE  TO  J02-B09S-STATE
               MOVE    WIDGET-INSENSITIVE  TO  J02-B10S-STATE
               MOVE    WIDGET-INSENSITIVE  TO  J02-B11S-STATE
               MOVE    WIDGET-INSENSITIVE  TO  J02-B01S-STATE
           END-IF
           .
       51O-GSRAUTH-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
      *
      *    排他エラーの時、患者番号へ
           IF      SPA-ERRCD           =   "9999"
               MOVE    00              TO  SPA-GMN-CUR
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    00
                   MOVE    "PTNUM"         TO  MCP-WIDGET
               WHEN    01
                   MOVE    "NUM"           TO  MCP-WIDGET
               WHEN    02
                   MOVE    "DAY"           TO  MCP-WIDGET
                   IF      SPA-DAY-IDX         =   ZERO
      ***********                              >   SPA-SRYMATU
                       MOVE    1               TO  SPA-DAY-IDX
                   END-IF
                   IF     (SPA-BIRTHDAY(1:6)   =   SPA-NAI-SRYYM )  AND
                          (SPA-DAY-IDX         <   SPA-BIRTHDAY(7:2))
                       MOVE    SPA-BIRTHDAY(7:2)   TO  SPA-DAY-IDX
                   END-IF
                   MOVE    SPA-DAY-IDX         TO  MCP-WIDGET(4:2)
               WHEN    03
                   MOVE    "RRKSELNUM"     TO  MCP-WIDGET
               WHEN    04
                   MOVE    "HENSRYYMD"     TO  MCP-WIDGET
               WHEN    05
                   MOVE    "B08"           TO  MCP-WIDGET
               WHEN    06
                   MOVE    "SRYKA"         TO  MCP-WIDGET
               WHEN    07
                   MOVE    "SRYYM"         TO  MCP-WIDGET
               WHEN    08
                   MOVE    "NYUGAIKBN"     TO  MCP-WIDGET
               WHEN    09
                   MOVE    "SRYKBN"        TO  MCP-WIDGET
               WHEN    10
                   MOVE    "HHKNCOMBI"     TO  MCP-WIDGET
               WHEN    11
                   MOVE    "IKKATU"        TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *    クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 420-CLEA-SEC
      *    剤削除
               WHEN    "CLICKED"       ALSO    "B02S"
                   PERFORM 4200-ZAICLIEA-SEC
      *    剤変更へ
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 420-ZAIHENKOU-SEC
      *    前回患者処理
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 430-MAEKPTNUM-SEC
      *    チェック処理
               WHEN    "CLICKED"       ALSO    "B03S"
                   PERFORM 430-CHEKU-SEC
      *    フリーコメント処理
               WHEN    "CLICKED"       ALSO    "B04S"
                   PERFORM 430-FREEMSG-SEC
      *    受診日訂正へ
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 440-SRYYMD-TEISEI-SEC
      *    前月選択
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 41018-ZENGETU-HEN-SEC
      **   前頁
               WHEN    "CLICKED"       ALSO    "B06S"
                   PERFORM 440-MAEGMN-SEC
      *    次頁
               WHEN    "CLICKED"       ALSO    "B07S"
                   PERFORM 450-ATOGMN-SEC
      *    次月選択
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 41019-JIGETU-HEN-SEC
      *    変更確定
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 470-KAKUTEI-SEC
      *    氏名検索（Ｐ９７）
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 470-SIMEIKEN-SEC
      *    予約登録へ
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 480-YOYAKU-SEC
      *    受付一覧へ
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 490-UKEICHI-SEC
      *     登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-TOROKU-SEC
      *     保険一括変更
               WHEN    "CLICKED"       ALSO    "B10S"
                   PERFORM 4100-HKNHEN-SEC
      *     入院調剤料変更
               WHEN    "CLICKED"       ALSO    "B11S"
                   PERFORM 4110-CHOZAI-SEC
      *     印刷
               WHEN    "CLICKED"       ALSO    "B12S"
                   PERFORM 4120-PRINT-SEC
      *    収納更新
               WHEN    "CLICKED"       ALSO    "B09S"
                   PERFORM 490-SYUNOU-CHK-SEC
      *    プレビュー
               WHEN    "CLICKED"       ALSO    "B01S"
                   PERFORM 4010-PREVIEW-SYORI-SEC
      *!!!
      *    算定履歴画面へ
               WHEN    "CLICKED"       ALSO    "B08S"
                   PERFORM 4801-K06-SYORI-SEC
      *!!!
      *H28.5
      *        処方名変換
               WHEN    "CLICKED"       ALSO    "B05S"
                   PERFORM 440-GENERIC-HEN-SEC
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
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        患者番号入力
               WHEN    "ACTIVATE"      ALSO    "PTNUM"
                   PERFORM 41010-PTNUM-SYORI-SEC
      *        診療科入力
               WHEN    "ACTIVATE"      ALSO    "SRYKA"
                   PERFORM 41013-SRYKAI-SYORI-SEC
      *        診療年月
               WHEN    "ACTIVATE"      ALSO    "SRYYM"
                   PERFORM 41018-SRYYMD-SYORI-SEC
      *        番号選択
               WHEN    "ACTIVATE"      ALSO    "NUM"
                   PERFORM 41011-BANGO-CHK-SEC
      *        明細選択
               WHEN    ANY             ALSO    "DATALIST"
                   PERFORM 41012-DATALIST-SEC
      *        保険組合せ
               WHEN    "ACTIVATE"      ALSO    "HHKNCOMBI"
                   PERFORM 41017-HKNCOMBI-CHK-SEC
      *        受診履歴選択
               WHEN    "ACTIVATE"      ALSO    "RRKSELNUM"
                   PERFORM 41014-RRKSELNUMI-CHK-SEC
      *        明細選択
               WHEN    ANY             ALSO    "RRKLIST"
                   PERFORM 41015-RRKLIST-SEC
      *        変更受診日
               WHEN    "ACTIVATE"      ALSO    "HENSRYYMD"
                   PERFORM 41016-HENSRYYMD-CHK-SEC
      *        入外区分
               WHEN    "ACTIVATE"      ALSO    "NYUGAIKBN"
                   PERFORM 41017-NYUGAIKBN-CHK-SEC
      *        診療種別
               WHEN    "ACTIVATE"      ALSO    "SRYKBN"
                   PERFORM 41018-SRYKBN-CHK-SEC
      *        一括変更
               WHEN    "ACTIVATE"      ALSO    "IKKATU"
                   PERFORM 41019-IKKATU-CHK-SEC
      *
               WHEN    OTHER
      *
      *            剤回数
                   IF    (MCP-EVENT       =   "ACTIVATE"  ) AND
                         (MCP-WIDGET(1:3) =   "DAY"       )
                       MOVE    MCP-WIDGET(4:2)     TO  SPA-DAY-IDX
                       PERFORM 41012-DAY-CHK-SEC
                   END-IF
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号入力処理
      *****************************************************************
       41010-PTNUM-SYORI-SEC         SECTION.
      *
           MOVE    J02-PTNUM           TO  SPA-GMN-PTNUM
      *
           MOVE    SPACE               TO  SPA-UPDFLG
           MOVE    ZERO                TO  SPA-SYUNOU-KOUSIN
      *
           INITIALIZE                      ORCSPTNUMAREA
           MOVE    SPA-GMN-PTNUM       TO  SPTNUM-PTNUM
           CALL    "ORCSPTNUM"         USING   ORCSPTNUMAREA
                                               SPA-AREA
           MOVE    SPTNUM-PTNUM        TO  SPA-GMN-PTNUM
           EVALUATE    SPTNUM-RC
               WHEN    00
                   MOVE    SPTNUM-PTNUM        TO  SPTID-PTNUM
                   MOVE    SPTNUM-PTID         TO  SPTID-PTID
               WHEN   10
                   INITIALIZE                  SPA-J02-AREA
                   INITIALIZE                  SPA-SCR-REC
                   INITIALIZE                  SPA-GMN-NYUIN-AREA
                   MOVE    SPA-J02-SRYYMDW(1:6)    TO  SPA-GMN-SRYYM
                   MOVE    SPA-J02-SRYYMD (1:6)    TO  SPA-NAI-SRYYM
                   MOVE    "0017"              TO  SPA-ERRCD
                   GO      TO     41010-PTNUM-SYORI-EXT
               WHEN    20
                   MOVE    SPTNUM-PTNUM        TO  SPA-GMN-PTNUM
      *            氏名検索へ
                   MOVE    SPACE               TO  LINKAREA
                   MOVE    SPA-J01KYOUTU       TO  SPA-WORK
                   MOVE    SPA-KYOUTU          TO  LNK-COMMON
                   MOVE    SPA-J02             TO  LNK-SCRSPA
      *
                   MOVE    "J02"               TO  SPA-MOTOPG
                   MOVE    "P97"               TO  SPA-SAKIPG
      *            氏名セット
                   MOVE    SPA-GMN-PTNUM       TO  SPA-NAME
                   MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
                   MOVE    "CHANGE"            TO  MCP-PUTTYPE
                   MOVE    "P97"               TO  MCP-WINDOW
                   PERFORM 900-PUT-WINDOW
                   MOVE    1                   TO  FLG-END
                   GO      TO     41010-PTNUM-SYORI-EXT
               WHEN    OTHER
                   INITIALIZE                  SPA-J02-AREA
                   INITIALIZE                  SPA-SCR-REC
                   INITIALIZE                  SPA-GMN-NYUIN-AREA
                   MOVE    SPA-J02-SRYYMDW(1:6)    TO  SPA-GMN-SRYYM
                   MOVE    SPA-J02-SRYYMD (1:6)    TO  SPA-NAI-SRYYM
                   MOVE    "0017"              TO  SPA-ERRCD
                   GO      TO     41010-PTNUM-SYORI-EXT
           END-EVALUATE
      *
           INITIALIZE                      SPA-J02-SET
      *
           MOVE    SPTID-PTNUM         TO  SPA-GMN-PTNUM
           MOVE    SPTID-PTID          TO  SPA-GMN-PTID
      *
      *    患者マスター初期編集・チェック
           PERFORM 410100-PTINF-HEN-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41010-PTNUM-SYORI-EXT
           END-IF
      *    患者マスター存在チェック
      *    MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
      *    MOVE    SPA-GMN-PTID       TO  PTINF-PTID
      *    PERFORM 900-PTINF-READ-SEC
      *    IF      FLG-PTINF      NOT =   ZERO
      *        MOVE    SPACE               TO  SPA-GMN-NAME
      *        MOVE    "0017"              TO  SPA-ERRCD
      *        GO      TO      41010-PTNUM-SYORI-EXT
      *    END-IF
      *
      *    MOVE    PTINF-NAME          TO  SPA-GMN-NAME
      *    MOVE    PTINF-KANANAME      TO  SPA-GMN-KANANAME
      *    EVALUATE    PTINF-SEX
      *        WHEN    "1"
      *            MOVE    "男"                TO  SPA-GMN-SEX
      *        WHEN    "2"
      *            MOVE    "女"                TO  SPA-GMN-SEX
      *    END-EVALUATE
      *
      *    MOVE    PTINF-BIRTHDAY      TO  SPA-GMN-BIRTHDAY
      *    MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
      *    PERFORM 41012-SEIWA-HEN-SEC
      *    MOVE    WRK-HENYMDG         TO  SPA-GMN-BIRTHDAYW
      *    患者の入金方法
      *    IF      PTINF-NYUKIN-HOHO   =   SPACE
      *        MOVE    "01"                TO  SPA-NYUKIN-HOHO
      *    ELSE
      *        MOVE    PTINF-NYUKIN-HOHO   TO  SPA-NYUKIN-HOHO
      *    END-IF
      *    入金状態編集
      *****PERFORM 410109-JYOTAI-SET-SEC
      *
      *    排他制御処理
           PERFORM 990-LOCK-IN-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41010-PTNUM-SYORI-EXT
           END-IF
      *
      *    前回患者
           MOVE    SPA-GMN-PTNUM       TO  SPA-LAST-PTNUM
           MOVE    SPA-GMN-PTID        TO  SPA-LAST-PTID
      *
      *    診療科をクリアする
           IF      SPA-SRYKA-MAX       =   1
               MOVE    SPA-GMN-SRYKA-LIST(01)
                                           TO  SPA-GMN-SRYKA-G
               MOVE    SPA-GMN-SRYKA       TO  SPA-J01-SRYKA
               MOVE    SPA-GMN-SRYKAMEI    TO  SPA-J01-SRYKAMEI
               MOVE    1                   TO  FLG-HENSYU
           ELSE
      *        受診履歴より
               IF      SPA-NAI-SRYKA-FLG   =   0
                                           OR  1
                   MOVE    SPACE               TO  SPA-GMN-SRYKA-G
                   MOVE    SPACE               TO  SPA-J01-SRYKA
                   MOVE    SPACE               TO  SPA-J01-SRYKAMEI
                   MOVE    1                   TO  FLG-HENSYU
               END-IF
      *        全科
               IF      SPA-NAI-SRYKA-FLG   =   2
                   MOVE    SPA-GMN-SRYKA-LIST(01)
                                               TO  SPA-GMN-SRYKA-G
                   MOVE    SPA-GMN-SRYKA       TO  SPA-J01-SRYKA
                   MOVE    SPA-GMN-SRYKAMEI    TO  SPA-J01-SRYKAMEI
                   MOVE    1                   TO  FLG-HENSYU
               END-IF
           END-IF
      *    入院が存在するかのチェック
           IF     (SPA-BEDSU           >   ZERO )
               PERFORM 410101-NYUINRRK-CHK-SEC
               IF      SPA-NAI-SRYKA-FLG   =   2
                   CONTINUE
               ELSE
      *        全科以外
                   IF     (SPA-NAI-NYUINKA NOT =   SPACE )
                     AND  (SPA-NYUGAIKBN       =   "1"   )
                       MOVE    SPA-NAI-NYUINKA     TO  SPA-J01-SRYKA
                       PERFORM 3101-SRYKA-HEN-SEC
                   END-IF
               END-IF
           END-IF
      *
           INITIALIZE                  SPA-GMN-RRKTBL
           INITIALIZE                  SPA-NAI-RRKTBL
           INITIALIZE                  SPA-NAIGMN-AREA
           INITIALIZE                  SPA-GMN-NYUIN-AREA
      *
      *    パラメタファイルのクリア
           PERFORM 3106-PARA-DEL-SEC
      *
           PERFORM 310-SPA-SET-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (FLG-HENSYU          =   ZERO  )
               GO      TO      41010-PTNUM-SYORI-EXT
           END-IF
      *    全科の時
           IF     (SPA-J01-SRYKA       =   "00" ) AND
                  (SPA-GMN-SRYKBN      =   "00"  OR  "01" )
               IF      SPA-NAI-SRYKBN-FLG  =   1
                   MOVE    SPA-GMN-SRYKBN-LST (02)
                                           TO  SPA-GMN-SRYKBN-G
               ELSE
                   MOVE    SPA-GMN-SRYKBN-LST (01)
                                           TO  SPA-GMN-SRYKBN-G
               END-IF
           END-IF
      *
      *    受診履歴編集
      *????PERFORM 3103-JYURRK-HEN-SEC
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
      *    保存用パラメタ作成
           PERFORM 4201-SRYACCT-PARA-SEC
           MOVE    1                   TO  SPA-GMN-PAGE
           MOVE    1                   TO  SPA-STRNUM
      *
      *    診療会計マスターより編集をする
           PERFORM 410181-SRYACCT-HEN-SEC
      *
           IF     (SPA-GMN-SRYKA-G     =   SPACE )  OR
                  (SPA-GMN-MAX         =   ZERO  )
               MOVE    6                   TO  SPA-GMN-CUR
           ELSE
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *    受診履歴が無い時、メッセージ表示
           IF      SPA-RRK-MAX         =   ZERO
               IF      SPA-SRYKA-MAX       =   1
                   MOVE    07              TO  SPA-GMN-CUR
               ELSE
                   MOVE    06              TO  SPA-GMN-CUR
               END-IF
           END-IF
           MOVE    WRK-HZN-ERRCD       TO  SPA-ERRCD
      *
           .
       41010-PTNUM-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター初期編集・チェック処理
      *****************************************************************
       410100-PTINF-HEN-SEC         SECTION.
      *
      *    患者マスター存在チェック
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-GMN-PTID       TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF      NOT =   ZERO
               MOVE    SPACE               TO  SPA-GMN-NAME
               MOVE    "0017"              TO  SPA-ERRCD
               GO      TO      410100-PTINF-HEN-EXT
           END-IF
      *
           MOVE    PTINF-NAME          TO  SPA-GMN-NAME
           MOVE    PTINF-KANANAME      TO  SPA-GMN-KANANAME
           EVALUATE    PTINF-SEX
               WHEN    "1"
                   MOVE    "男"                TO  SPA-GMN-SEX
               WHEN    "2"
                   MOVE    "女"                TO  SPA-GMN-SEX
           END-EVALUATE
      *
           MOVE    PTINF-BIRTHDAY      TO  SPA-GMN-BIRTHDAY
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 41012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-BIRTHDAYW
      *    患者の入金方法
           IF      PTINF-NYUKIN-HOHO   =   SPACE
               MOVE    "01"                TO  SPA-NYUKIN-HOHO
           ELSE
               MOVE    PTINF-NYUKIN-HOHO   TO  SPA-NYUKIN-HOHO
           END-IF
      *    入金状態編集
           PERFORM 410109-JYOTAI-SET-SEC
           .
       410100-PTINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入金状態編集処理
      *****************************************************************
       410109-JYOTAI-SET-SEC         SECTION.
      *
           MOVE    SPACE               TO  SYS-1041-REC
           MOVE    "1041"              TO  SYS-1041-KANRICD
           MOVE    SPA-NYUKIN-HOHO     TO  SYS-1041-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1041-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1041-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1041-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1041-REC        TO  SYSKANRI-REC
           PERFORM 9001-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1041-REC
               MOVE    SYS-1041-NYKN-JYOTAI
                                           TO  SPA-NYUKIN-JYOTAI
           ELSE
               MOVE    "1"                 TO  SPA-NYUKIN-JYOTAI
           END-IF
      *
           .
       410109-JYOTAI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院存在チェック処理
      *****************************************************************
       410101-NYUINRRK-CHK-SEC         SECTION.
      *
      *    入院履歴判定
           MOVE    SPACE               TO  PTNYUINRRK-REC
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTNYUINRRK-PTID
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key34"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key34"             TO  MCP-PATHNAME
               PERFORM 910-PTNYUINRRK-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
           MOVE    SPACE               TO  SPA-NAI-NYUINYMD
           MOVE    SPACE               TO  SPA-GMN-TAIINYMD
           MOVE    SPACE               TO  SPA-NAI-NYUINYMD
           MOVE    SPACE               TO  SPA-GMN-NYUINYMD
           MOVE    SPACE               TO  SPA-NAI-NYUINKA
      *入院期間設定
           INITIALIZE                  SPA-NAI-NYUIN-KIKAN
      *
      *    入院が１件もないとき、外来とする
           IF      FLG-PTNYUINRRK      =   1
               MOVE    "2"             TO  SPA-NYUGAIKBN
           END-IF
           MOVE    ZERO                TO  FLG-OK
           MOVE    SPACE               TO  WRK-OLD-NYUINYMD
           MOVE    SPACE               TO  WRK-OLD-TAIINYMD
           PERFORM
                   UNTIL      (FLG-PTNYUINRRK      =   1 )
      ******************* OR  (FLG-OK              =   1 )
      *        転入日
               IF      PTNYUINRRK-TENNYUYMD(1:6)   <=  SPA-NAI-SRYYM
      *            転出日
                   IF     ( PTNYUINRRK-TENSTUYMD   =   SPACE )  OR
                          ((PTNYUINRRK-TENSTUYMD   NOT =   SPACE ) AND
                           (PTNYUINRRK-TENSTUYMD(1:6)  >=
                                                       SPA-NAI-SRYYM))
      *                入院日
                       MOVE    PTNYUINRRK-NYUINYMD TO  SPA-NAI-NYUINYMD
                       MOVE    PTNYUINRRK-NYUINYMD TO  WRK-SYMD
                       PERFORM 41012-SEIWA-HEN-SEC
                       MOVE    WRK-HENYMDG         TO  SPA-GMN-NYUINYMD
      *                退院日
                       MOVE    PTNYUINRRK-TAIINYMD TO  SPA-NAI-TAIINYMD
                       MOVE    PTNYUINRRK-TAIINYMD TO  WRK-SYMD
                       PERFORM 41012-SEIWA-HEN-SEC
                       MOVE    WRK-HENYMDG         TO  SPA-GMN-TAIINYMD
      *                入院科
                       MOVE    PTNYUINRRK-NYUINKA  TO  SPA-NAI-NYUINKA
                       MOVE    1                   TO  FLG-OK
                   END-IF
      *H25.4
      *            退院日
                   IF      (PTNYUINRRK-TAIINYMD    =   SPACE )  OR
                           (PTNYUINRRK-TAIINYMD(1:6)
                                               >=  SPA-NAI-SRYYM )
      ****             入院日＝転入日のデータを対象
      ****             IF      PTNYUINRRK-NYUINYMD
      ****                                     =  PTNYUINRRK-TENNYUYMD
      *                入退院日で１回
                       IF     (PTNYUINRRK-NYUINYMD
                                               =  WRK-OLD-NYUINYMD)
                        AND   (PTNYUINRRK-TAIINYMD
                                               =  WRK-OLD-TAIINYMD)
                           CONTINUE
                       ELSE
      *                    入院期間設定
                           PERFORM 4101012-NYUINKIKAN-SEC
                       END-IF
                   END-IF
      *
                   MOVE    PTNYUINRRK-NYUINYMD TO  WRK-OLD-NYUINYMD
                   MOVE    PTNYUINRRK-TAIINYMD TO  WRK-OLD-TAIINYMD
      *
      *            最終退院日
                   IF     (WRK-TAIINYMD        =   SPACE   )  OR
                          (WRK-TAIINYMD        <   PTNYUINRRK-TAIINYMD)
                       MOVE    PTNYUINRRK-TAIINYMD TO  WRK-TAIINYMD
                   END-IF
               END-IF
      *
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key34"             TO  MCP-PATHNAME
               PERFORM 910-PTNYUINRRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key34"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-OK              =   1
      *        入院中の場合入院とする
               IF      (SPA-NAI-NYUINYMD(1:6)  <   SPA-NAI-SRYYM )  AND
                       (SPA-NAI-TAIINYMD(1:6)  >   SPA-NAI-SRYYM )
                   MOVE    "1"             TO  SPA-NYUGAIKBN
               END-IF
      *        初期の設定
               IF      SPA-NYUGAIKBN       =   "2"
                   MOVE    SPA-J01-SRYKA   TO  WRK-SRYKA
                   MOVE    "00"            TO  SPA-J01-SRYKA
                   PERFORM 3103-JYURRK-HEN-SEC
                   IF      SPA-RRK-MAX     =   ZERO
                       MOVE    "1"             TO  SPA-NYUGAIKBN
                   END-IF
                   MOVE    WRK-SRYKA       TO  SPA-J01-SRYKA
               END-IF
           ELSE
      *       外来とする
               MOVE    "2"             TO  SPA-NYUGAIKBN
      *        最終退院日
               IF     (WRK-TAIINYMD    NOT =   SPACE ) AND
                      (SPA-GMN-TAIINYMD    =   SPACE )
                   MOVE    WRK-TAIINYMD        TO  WRK-SYMD
                   PERFORM 41012-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG         TO  SPA-GMN-TAIINYMD
               END-IF
           END-IF
      *
           .
       410101-NYUINRRK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院期間設定処理
      *****************************************************************
       4101012-NYUINKIKAN-SEC           SECTION.
      *
      *    入院日
           IF      PTNYUINRRK-NYUINYMD(1:6)
                                       =   SPA-NAI-SRYYM
               MOVE    PTNYUINRRK-TENNYUYMD(7:2)
                                           TO  WRK-STDD
           ELSE
               MOVE    1                   TO  WRK-STDD
           END-IF
      *    退院日
           IF      PTNYUINRRK-TAIINYMD(1:6)
                                       =   SPA-NAI-SRYYM
               MOVE    PTNYUINRRK-TAIINYMD(7:2)
                                           TO  WRK-EDDD
           ELSE
               MOVE    31                  TO  WRK-EDDD
           END-IF
      *    入院中としない
           PERFORM VARYING     IDX-D   FROM    WRK-STDD BY  1
                   UNTIL       IDX-D   >   WRK-EDDD
               IF    ((PTNYUINRRK-NYUINYMD(1:6)
                                           =   SPA-NAI-SRYYM )
                  AND (IDX-D               =   WRK-STDD      ))
               OR    ((PTNYUINRRK-TAIINYMD(1:6)
                                           =   SPA-NAI-SRYYM )
                  AND (IDX-D               =   WRK-EDDD      ))
      *            入院日・退院日
                   IF      SPA-NAI-NYUIN (IDX-D) =   ZERO
                       MOVE    2           TO  SPA-NAI-NYUIN (IDX-D)
                   ELSE
      *            同日再入院日
                        MOVE    3          TO  SPA-NAI-NYUIN (IDX-D)
                   END-IF
      *H30.7
      *            自院歴分は対象外
                   IF     (PTNYUINRRK-JTIKBN    =   "6")  
                        MOVE    1          TO  SPA-NAI-NYUIN-JR(IDX-D)
                   END-IF
               ELSE
                   MOVE    1           TO  SPA-NAI-NYUIN (IDX-D)
      *H30.7
      *            自院歴分は対象外
                   IF     (PTNYUINRRK-JTIKBN    =   "6")  
                        MOVE    1          TO  SPA-NAI-NYUIN-JR(IDX-D)
                   END-IF
               END-IF
           END-PERFORM
           .
       4101012-NYUINKIKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科入力処理
      *****************************************************************
       41013-SRYKAI-SYORI-SEC         SECTION.
      *
           MOVE    6                   TO  SPA-GMN-CUR
      *
      *    診療科変更なし
      *    IF     (J02-SRYKA       NOT =   SPACE )  AND
      *           (J02-SRYKA           =   SPA-GMN-SRYKA-G )
      *
      *        IF      SPA-GMN-MAX         >   ZERO
      *            MOVE    1                   TO  SPA-GMN-CUR
      *        END-IF
      *        GO      TO      41013-SRYKAI-SYORI-EXT
      *    END-IF
      *
           MOVE    SPA-GMN-SRYKA-G     TO  SPA-MAE-SRYKA-G
           MOVE    J02-SRYKA           TO  SPA-GMN-SRYKA-G
      *    診療科選択なし
           IF     (SPA-GMN-SRYKA       =   SPACE ) AND
                  (SPA-GMN-PTID        =   ZERO  )
               GO      TO      41013-SRYKAI-SYORI-EXT
           END-IF
      *    診療科
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SRYKA-MAX
               IF      SPA-GMN-SRYKA   =   SPA-GMN-SRYKAL (IDX)
                   MOVE    SPA-GMN-SRYKA-LIST (IDX)
                                           TO  SPA-GMN-SRYKA-G
                   MOVE    SPA-SRYKA-MAX   TO  IDX
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
      *    科が無い時、前の科に戻す
           IF      FLG-CHK             =   ZERO
               MOVE    SPA-MAE-SRYKA-G TO  SPA-GMN-SRYKA-G
               GO      TO      41013-SRYKAI-SYORI-EXT
           END-IF
      *    診療科選択
           IF     (SPA-MAE-SRYKA       =   SPACE )  OR
                  (SPA-UPDFLG          =   SPACE )
               PERFORM 41013-SRYKA-CHANGE-SEC
               GO      TO      41013-SRYKAI-SYORI-EXT
           END-IF
      *
      *    確認表示
           MOVE    "0107"              TO  SPA-JIDCD
           .
       41013-SRYKAI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科変更処理
      *****************************************************************
       41013-SRYKA-CHANGE-SEC             SECTION.
      *
      *    現在表示されている内容を更新する
      *    診療会計マスター更新
           IF      SPA-UPDFLG          =   "1"
               IF      SPA-GMN-MAX         >   ZERO
                   PERFORM 4901-SRYACCT-UPD-SEC
               END-IF
               IF     (SPA-ERRCD       NOT =   SPACE )  OR
                      (FLG-END             =   1     )
                   MOVE    1               TO  SPA-J025-SYORI
               ELSE
                   PERFORM 41013-SRYKA-CHANGE-SYORI-SEC
               END-IF
           ELSE
               PERFORM 41013-SRYKA-CHANGE-SYORI-SEC
           END-IF
           .
       41013-SRYKA-CHANGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科変更処理
      *****************************************************************
       41013-SRYKA-CHANGE-SYORI-SEC             SECTION.
      *
           MOVE    SPA-GMN-SRYKA       TO  SPA-J01-SRYKA
           MOVE    SPA-GMN-SRYKAMEI    TO  SPA-J01-SRYKAMEI
           MOVE    SPA-GMN-SRYYM       TO  SPA-J02-SRYYMDW(1:6)
           MOVE    SPA-NAI-SRYYM       TO  SPA-J02-SRYYMD (1:6)
           MOVE    SPA-GMN-NYUGAIKBN-G TO  WRK-NYUGAIKBN-G
           MOVE    SPA-GMN-SRYKBN-G    TO  WRK-SRYKBN-G
           MOVE    SPA-GMN-HHKNCOMBI-G TO  WRK-HKNCOMBI-G
           INITIALIZE                  SPA-J02-AREA
           MOVE    WRK-NYUGAIKBN-G     TO  SPA-GMN-NYUGAIKBN-G
           MOVE    WRK-SRYKBN-G        TO  SPA-GMN-SRYKBN-G
           MOVE    WRK-HKNCOMBI-G      TO  SPA-GMN-HHKNCOMBI-G
      *    全科の時
           IF     (SPA-J01-SRYKA       =   "00" ) AND
                  (SPA-GMN-SRYKBN      =   "00"  OR  "01" )
               IF      SPA-NAI-SRYKBN-FLG  =   1
                   MOVE    SPA-GMN-SRYKBN-LST (02)
                                           TO  SPA-GMN-SRYKBN-G
               ELSE
                   MOVE    SPA-GMN-SRYKBN-LST (01)
                                           TO  SPA-GMN-SRYKBN-G
               END-IF
           END-IF
      *
           MOVE    SPA-J02-SRYYMDW(1:6)    TO  SPA-GMN-SRYYM
           MOVE    SPA-J02-SRYYMD (1:6)    TO  SPA-NAI-SRYYM
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-SRYKA-MAX
               IF      SPA-J01-SRYKA       =   SPA-GMN-SRYKAL (IDX)
                   MOVE    SPA-GMN-SRYKA-LIST(IDX)
                                           TO  SPA-GMN-SRYKA-G
               END-IF
           END-PERFORM
      *
      *    受診履歴編集
           PERFORM 3103-JYURRK-HEN-SEC
      *
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
      *    保存用パラメタ作成
           PERFORM 4201-SRYACCT-PARA-SEC
           MOVE    1                   TO  SPA-GMN-PAGE
           MOVE    1                   TO  SPA-STRNUM
      *
      *    診療会計マスターより編集をする
           PERFORM 410181-SRYACCT-HEN-SEC
      *    収納更新チェック
           PERFORM 3104-SYUNOU-CHECK-SEC
      *
      *    入院料包括診療チェック処理
           PERFORM 30091-HKTCD-CHK-SEC
      *
           IF      SPA-GMN-MAX         =   ZERO
               MOVE    6                   TO  SPA-GMN-CUR
           ELSE
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *    受診履歴が無い時、メッセージ表示
           IF      SPA-RRK-MAX         =   ZERO
              AND  SPA-GMN-MAX         =   ZERO
               IF      SPA-SRYKA-MAX       =   1
                   MOVE    07              TO  SPA-GMN-CUR
               ELSE
                   MOVE    06              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       41013-SRYKA-CHANGE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療年月入力処理
      *****************************************************************
       41018-SRYYMD-SYORI-SEC             SECTION.
      *
           MOVE    7                   TO  SPA-GMN-CUR
      *
           MOVE    J02-SRYYM           TO  SPA-GMN-SRYYM
      *
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-GMN-SRYYM       TO  SGDAY-INDAY
           MOVE    "1"                 TO  SGDAY-INTYPE
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY(1:6)    TO  SPA-GMN-SRYYM
      *        患者の確定してない時
               IF      SPA-GMN-PTID        =   ZERO
                    MOVE    SGDAY-SDAY(1:6)     TO  SPA-NAI-SRYYM
                    MOVE    ZERO                TO  SPA-GMN-CUR
                    GO      TO      41018-SRYYMD-SYORI-EXT
               END-IF
      *        誕生日以前の時、エラー
               IF      SGDAY-SDAY(1:6)     <   SPA-GMN-BIRTHDAY(1:6)
                    MOVE    "0015"              TO  SPA-ERRCD
                    GO      TO      41018-SRYYMD-SYORI-EXT
               END-IF
      *        変更のない時、以降の処理はそのまま
               IF      SGDAY-SDAY(1:6)     =   SPA-NAI-SRYYM
                   IF      SPA-GMN-MAX         >   ZERO
                        MOVE    1                   TO  SPA-GMN-CUR
                        GO      TO      41018-SRYYMD-SYORI-EXT
                   END-IF
               END-IF
               MOVE    SGDAY-SDAY(1:6)     TO  SPA-NAI-SRYYM
           ELSE
               MOVE    "0018"          TO  SPA-ERRCD
               GO      TO      41018-SRYYMD-SYORI-EXT
           END-IF
      *
           IF      SPA-UPDFLG          =   SPACE
               PERFORM 410181-SRYYM-CHANGE-SEC
           ELSE
               MOVE    "0108"              TO  SPA-JIDCD
           END-IF
      *
           .
       41018-SRYYMD-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日変更処理
      *****************************************************************
       410181-SRYYM-CHANGE-SEC             SECTION.
      *
      *    診療会計マスター更新
           IF      SPA-UPDFLG          =   "1"
               MOVE    SPA-NAI-SRYYM       TO  WRK-NAI-SRYYM
               MOVE    SPA-GMN-SRYYM       TO  WRK-GMN-SRYYM
               MOVE    SPA-J02-SRYYMDW(1:6)    TO  SPA-GMN-SRYYM
               MOVE    SPA-J02-SRYYMD (1:6)    TO  SPA-NAI-SRYYM
               PERFORM 4901-SRYACCT-UPD-SEC
               IF     (SPA-ERRCD       NOT =   SPACE )  OR
                      (FLG-END             =   1     )
                   MOVE    2               TO  SPA-J025-SYORI
                   IF      SPA-ERRCD       =   SPACE
                       MOVE    WRK-NAI-SRYYM       TO  SPA-NAI-SRYYM
                       MOVE    WRK-GMN-SRYYM       TO  SPA-GMN-SRYYM
                   END-IF
               ELSE
                   PERFORM 410181-SRYYM-CHANGE-SYORI-SEC
               END-IF
           ELSE
               PERFORM 410181-SRYYM-CHANGE-SYORI-SEC
           END-IF
           .
       410181-SRYYM-CHANGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療月変更処理
      *****************************************************************
       410181-SRYYM-CHANGE-SYORI-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-RRK-MAX
           INITIALIZE                  SPA-GMN-RRKTBL
           INITIALIZE                  SPA-NAI-RRKTBL
           MOVE    ZERO                TO  SPA-GMN-MAX
           INITIALIZE                  SPA-GMN-TBL
           INITIALIZE                  SPA-NAI-TBL
           INITIALIZE                  SPA-GMN-INPUT
           INITIALIZE                  SPA-GMN-NYUIN-AREA
      *
      *    入外区分・病床数
           PERFORM 3009-NYUGAIKBN-HEN-SEC
      *    診療科一覧編集
           PERFORM 3005-SRYKA-HEN-SEC
      *
      *    保険一覧編集
           PERFORM 3008-HKNCOMBI-HEN-SEC
      *
      *H28.4
      *    一般名記載設定処理
           PERFORM 3008-GENERIC-HEN-SEC
      *
      *    診療年月の末日算定
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    SPA-NAI-SRYYM       TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI(7:2)    TO  SPA-SRYMATU
      *
      *    受診履歴より
           IF      SPA-NAI-SRYKA-FLG   =   0
               MOVE    SPACE               TO  SPA-J01-SRYKA
               MOVE    SPACE               TO  SPA-J01-SRYKAMEI
           END-IF
      *    全科
           IF      SPA-NAI-SRYKA-FLG   =   2
               MOVE    SPA-GMN-SRYKA-LIST(01)
                                           TO  SPA-GMN-SRYKA-G
               MOVE    SPA-GMN-SRYKA       TO  SPA-J01-SRYKA
               MOVE    SPA-GMN-SRYKAMEI    TO  SPA-J01-SRYKAMEI
           END-IF
      *    受診履歴より診療科決定
           IF      SPA-J01-SRYKA       =   SPACE
               PERFORM 310310-JYURRK-CHK-SEC
               IF      SPA-J01-SRYKA       =   SPACE
                   MOVE    "0020"          TO  SPA-ERRCD
                   MOVE    07              TO  SPA-GMN-CUR
                   GO      TO       410181-SRYYM-CHANGE-SYORI-EXT
               END-IF
           END-IF
      *
           PERFORM 3101-SRYKA-HEN-SEC
      *    入院が存在するかのチェック
           IF      SPA-BEDSU           >   ZERO
               PERFORM 410101-NYUINRRK-CHK-SEC
               PERFORM 410102-NYUINRRK-LST-SEC
           END-IF
      *
      *    受診履歴編集
           PERFORM 3103-JYURRK-HEN-SEC
      *    収納更新チェック
           PERFORM 3104-SYUNOU-CHECK-SEC
      *
      *    入院料包括診療チェック処理
           PERFORM 30091-HKTCD-CHK-SEC
      *
      *    保存用パラメタ作成
           PERFORM 4201-SRYACCT-PARA-SEC
           MOVE    1                   TO  SPA-GMN-PAGE
           MOVE    1                   TO  SPA-STRNUM
      *
      *    診療会計マスターより編集をする
           PERFORM 410181-SRYACCT-HEN-SEC
      *
      *
           IF      SPA-GMN-MAX         =   ZERO
               MOVE    7                   TO  SPA-GMN-CUR
           ELSE
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *    受診履歴が無い時、メッセージ表示
           IF      SPA-RRK-MAX         =   ZERO
              AND  SPA-GMN-MAX         =   ZERO
               IF      SPA-SRYKA-MAX       =   1
                   MOVE    07              TO  SPA-GMN-CUR
               ELSE
                   MOVE    06              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       410181-SRYYM-CHANGE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    番号選択入力処理
      *****************************************************************
       41011-BANGO-CHK-SEC             SECTION.
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           IF      J02-NUM             =   SPACE
               MOVE    ZERO                TO  J02-NUM
           END-IF
           INITIALIZE                      ORCSNUMAREA
           MOVE    J02-NUM             TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "0001"              TO  SPA-ERRCD
               GO      TO      41011-BANGO-CHK-EXT
           ELSE
               MOVE    SNUM-OUTNUM         TO  SPA-GMN-BANGO
           END-IF
      *
      *    IF      SPA-GMN-BANGO       >   SPA-NAI-MAX
      *        MOVE    "0002"              TO  SPA-ERRCD
      *        GO      TO      41011-BANGO-CHK-EXT
      *    END-IF
           MOVE    ZERO                TO  SPA-NAI-BANGO
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX
               IF      SPA-GMN-TBLREC(IDX)(1:3)    =   "---"
                   CONTINUE
               ELSE
                   IF      SPA-GMN-BANGO       =   SPA-GMN-TBANGO  (IDX)
                       MOVE    IDX             TO  WRK-BANGO
                       MOVE    SPA-GMN-MAX     TO  IDX
                   END-IF
               END-IF
           END-PERFORM
           IF      WRK-BANGO           =   ZERO
               MOVE    "0002"              TO  SPA-ERRCD
               GO      TO      41011-BANGO-CHK-EXT
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-NAI-MAX
               IF      WRK-BANGO       =   SPA-NAI-TBANGO (IDX)
                   MOVE    IDX             TO  SPA-NAI-BANGO
                   MOVE    SPA-NAI-MAX     TO  IDX
               END-IF
           END-PERFORM
      *H25.2
      *    レセ電移行分はエラー
           IF     (SPA-GMN-BANGO   NOT =   ZERO )
             AND  (SPA-NAI-SRYKA   (SPA-NAI-BANGO)(1:1)
                                       =   "A"   )
               MOVE    "7011"              TO  SPA-ERRCD
               GO      TO      41011-BANGO-CHK-EXT
           END-IF
      *R01.5
           IF     (SPA-GMN-BANGO   NOT =   ZERO )
           AND    (SPA-NAI-BANGO       =   ZERO )
               MOVE    "0002"              TO  SPA-ERRCD
               GO      TO      41011-BANGO-CHK-EXT
           END-IF
      *
           IF      SPA-GMN-BANGO       =   ZERO
               INITIALIZE                  SPA-GMN-INPUT
               INITIALIZE                  SPA-NAIGMN-AREA
               MOVE    ZERO                TO  SPA-J02-SYORI
      *
               MOVE    1                   TO  SPA-GMN-CUR
           ELSE
               INITIALIZE                  SPA-NAIGMN-AREA
               MOVE    ZERO                TO  SPA-J02-SYORI
      *
               MOVE    SPA-NAI-TBANGO (SPA-NAI-BANGO)
                                           TO  IDX
               MOVE    SPA-GMN-TMEISYO  (IDX)
                                           TO  SPA-GMN-MEISYO
               MOVE    SPA-NAI-DAY-AREA (SPA-NAI-BANGO)
                                           TO  SPA-GNAI-DAY-G
               MOVE    SPA-GNAI-DAY-TBL (01)
                                           TO  SPA-GMN-DAY-G
      *        保険番号
               MOVE    SPA-NAI-HKNCOMBI (SPA-NAI-BANGO)
                                           TO  SPA-GMN-HKNCOMBI
               MOVE    SPA-NAI-ROUJIN   (SPA-NAI-BANGO)
                                           TO  SPA-GMN-ROUJIN
      *        剤番号
               MOVE    SPA-NAI-ZAINUM  (SPA-NAI-BANGO)
                                           TO  SPA-GMN-ZAINUM
      *        診療科
               MOVE    SPA-NAI-SRYKA   (SPA-NAI-BANGO)
                                           TO  SPA-NAIGMN-SRYKA
               MOVE    SPA-NAI-SRYKAMEI(IDX + 1)
                                           TO  SPA-GMN-BSRYKAMEI
      *        診療区分
               MOVE    SPA-NAI-SRYKBN  (SPA-NAI-BANGO)
                                           TO  SPA-NAIGMN-SRYKBN
      *        保険内容編集
               PERFORM VARYING     IDH     FROM    1   BY  1
                       UNTIL       IDH     >   SPA-HKN-MAX
                   IF      SPA-GMN-THKNCOMBINUM (IDH)
                                               =   SPA-GMN-HKNCOMBI
                       MOVE    SPA-GMN-THKNCOMBIMEI (IDH)
                                               TO  SPA-GMN-HKNCOMBIMEI
                       MOVE    SPA-NAI-THKNNUM(IDH)
                                               TO  SPA-NAI-HKNNUM
                       MOVE    SPA-NAI-THKNKBN (IDH)
                                               TO  SPA-NAI-HKNKBN
                       MOVE    SPA-NAI-TRSI-SISIKBN (IDH)
                                               TO  SPA-NAI-SISIKBN
                       MOVE    SPA-NAI-TRSI-HKNKBN (IDH)
                                               TO  SPA-NAI-RSI-HKNKBN
                       MOVE    SPA-NAI-TRSI-SHOBYOYMD (IDH)
                                               TO  SPA-RSI-SHOBYOYMD
                       MOVE    SPA-NAI-TRSI-JUNKYO (IDH)
                                               TO  SPA-NAI-RSI-JUNKYO
      *                期間外の日があれば期間編集
                       IF     (SPA-NAI-TTEKSTYMD (IDH)(1:6)
                                                   =   SPA-NAI-SRYYM)
                       OR     (SPA-NAI-TTEKEDYMD (IDH)(1:6)
                                                   =   SPA-NAI-SRYYM)
                           PERFORM 410110-HKNCOMBMSG-SEC
                       ELSE
                           MOVE    SPACE       TO  SPA-GMN-HKNCOMBMSG
                       END-IF
                   END-IF
               END-PERFORM
      *        入院料包括チェック
               MOVE    SPA-NAI-JIHOKBN  (SPA-NAI-BANGO)
                                           TO  SPA-JIHOKBN
               MOVE    ZERO                TO  SPA-NAI-HKTGAIFLG
               IF     (SPA-GMN-NYUGAIKBN   =   "1"   )
                  AND (SPA-GMN-HKNCOMBI    NOT =   "9999" )
                   PERFORM 410112-NYUHKTKBN-HEN-SEC
               END-IF
      *
               MOVE    1                   TO  SPA-IDXZ
      *
               MOVE    11                  TO  SPA-GMN-CUR
               MOVE    ZERO                TO  SPA-DAY-IDX
           END-IF
      *
           .
       41011-BANGO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ期間編集処理
      *****************************************************************
       410110-HKNCOMBMSG-SEC             SECTION.
      *
           IF    ((SPA-NAI-TTEKSTYMD (IDH)(1:6)   =   SPA-NAI-SRYYM)
              AND (SPA-NAI-TTEKSTYMD (IDH)(7:2)   >   "01"         ))
           OR    ((SPA-NAI-TTEKEDYMD (IDH)(1:6)   =   SPA-NAI-SRYYM)
              AND (SPA-NAI-TTEKEDYMD (IDH)(7:2)   <   SPA-SRYMATU ))
               MOVE    SPACE               TO  SPA-GMN-HKNCOMBMSG
               STRING  "（"
                   SPA-GMN-TTEKSTYMD  (IDH)
                   "〜"
                   SPA-GMN-TTEKEDYMD  (IDH)
                   "）"                DELIMITED  BY  SIZE
                                       INTO    SPA-GMN-HKNCOMBMSG
               END-STRING
           END-IF
           .
       410110-HKNCOMBMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院料包括チェック処理
      *****************************************************************
       410112-NYUHKTKBN-HEN-SEC             SECTION.
      *
           INITIALIZE                  SPA-NAI-NYUHKT-G
      *    入院料包括診療チェック
           INITIALIZE                  ORCSCHKTALLAREA
           MOVE    SPA-NAI-SRYYM       TO  ORCSCHKTALL-SRYYM
           MOVE    "01"                TO  ORCSCHKTALL-SRYDD
           MOVE    SPA-GMN-PTID        TO  ORCSCHKTALL-PTID
      *    判定内容
           MOVE    SPA-NAI-SRYKBN  (SPA-NAI-BANGO)
                                       TO  ORCSCHKTALL-HKTSRYKBN
           MOVE    SPA-NAI-SRYSYU (SPA-NAI-BANGO)
                                       TO  ORCSCHKTALL-HKTSRYSYUKBN
      *    対象の診療行為
           MOVE    ZERO                TO  FLG-DEKIDAKA
           MOVE    ZERO                TO  IDX-T
           MOVE    ZERO                TO  WRK-CCNT
           PERFORM VARYING     IDX-H   FROM    IDX   BY  1
                   UNTIL      (IDX-H   >   SPA-GMN-MAX )
                           OR (IDX-T   >=  10          )
               IF      SPA-GMN-TSRYCD (IDX-H)      =   "099999903"
                   MOVE    1                   TO  FLG-DEKIDAKA
               END-IF
               IF      SPA-GMN-TSRYCD (IDX-H)(1:1) =   "1"
                   ADD     1           TO  IDX-T
                   MOVE    SPA-GMN-TSRYCD (IDX-H)
                                       TO  ORCSCHKTALL-HKTSRYCD
                                                    (IDX-T)
               END-IF
      *H27.1
      *        診療・器材・薬剤コードのチェック
               IF     (SPA-GMN-TSRYCD (IDX-H)(1:1) =   "1"
                                                   OR  "6"
                                                   OR  "7")
                 OR   (SPA-GMN-TSRYCD (IDX-H)(1:3) =  "059")
                   ADD     1           TO  WRK-CCNT
               END-IF
      *
               IF     (SPA-GMN-TMEISYO (IDX-H) =   ALL "-" )
                  OR ((IDX             NOT =   IDX-H   )  AND
                      (SPA-GMN-TBANGO  (IDX-H) >   ZERO   ))
                   MOVE    SPA-GMN-MAX         TO  IDX-H
               END-IF
           END-PERFORM
      *    出来高算定コード算定は、チェックなし
           IF     (FLG-DEKIDAKA        =   1  )
           OR     (SPA-NAI-SRYKBN(SPA-NAI-BANGO)
                                       =   "95"
                                       OR  "96"
                                       OR  "99"
                                       OR  "98" )
               MOVE    ZERO                TO  SPA-NAI-HKTGAIFLG
           ELSE
               MOVE    1                   TO  SPA-NAI-HKTGAIFLG
           END-IF
      *H27.1
      *    診療・器材・薬剤なし、院外投薬対象外
           IF     (WRK-CCNT            =   ZERO )
           OR     ((SPA-NAI-SRYKBN(SPA-NAI-BANGO)
                                       =   "21"
                                       OR  "22"
                                       OR  "23" )  AND
                   (SPA-NAI-ZAITEN(SPA-NAI-BANGO)
                                       =   ZERO ) )
           OR     ( SPA-NAI-SRYSYU (SPA-NAI-BANGO)
                                       =   "148"
                                       OR  "149" )
               MOVE    ZERO                TO  SPA-NAI-HKTGAIFLG
           END-IF
      *H28.9
      *    持参薬は対象外とする
           IF     (SPA-NAI-ZAIKBN (SPA-NAI-BANGO)  =   3
                                                   OR  4   )
               MOVE    ZERO                TO  SPA-NAI-HKTGAIFLG
           END-IF
      *
           IF     (SPA-NAI-HKTGAIFLG   =   1    )  AND
                  (SPA-NAI-NYUHKTKBN   =   1    )
               CALL    "ORCSCHKTALL"       USING
                                           ORCSCHKTALLAREA
                                           SPA-AREA
      *        日毎の包括設定
               PERFORM VARYING     IDX-D   FROM    1   BY  1
                       UNTIL       IDX-D   >   31
                   MOVE    ORCSCHKTALL-HKTCHKFLG (IDX-D)
                                           TO  SPA-NAI-HKTCHKFLG (IDX-D)
               END-PERFORM
           END-IF
      *    入院料包括日編集処理
           IF      SPA-NAI-HKTGAIFLG   =   1
               PERFORM 410112-NYUHKTMSG-SEC
           END-IF
           .
       410112-NYUHKTKBN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院料包括日編集処理
      *****************************************************************
       410112-NYUHKTMSG-SEC             SECTION.
      *
           INITIALIZE                  WRK-HKTSRY-AREA
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               IF      SPA-GMN-DAY (IDX-D) >   ZERO
                   IF      SPA-JIHOKBN         =   "1"
                       IF      SPA-NAI-HKTCHKFLG (IDX-D)   =   ZERO
      *                    包括診療剤で包括でない日
                           ADD     1               TO  IDD
                           MOVE    IDX-D           TO  WRK-ERRDAY(IDD)
                       END-IF
                   ELSE
                       IF      SPA-NAI-HKTCHKFLG (IDX-D)   =   1
      *                    包括診療でない剤で包括日
                           ADD     1               TO  IDD
                           MOVE    IDX-D           TO  WRK-ERRDAY(IDD)
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      IDD                 >   ZERO
               IF      SPA-JIHOKBN         =   "1"
                   MOVE    "出来高算定日："    TO  WRK-HKTCHKMSG
               ELSE
                   MOVE    "包括算定日："      TO  WRK-HKTCHKMSG
               END-IF
               PERFORM VARYING     IDX-D   FROM    1   BY  1
                       UNTIL      (IDX-D   >   31  )
                               OR (WRK-ERRDAY  (IDX-D) =   ZERO)
                   MOVE    WRK-HKTCHKMSG       TO  WRK-HKTCHKMSG-H
                   MOVE    SPACE               TO  WRK-HKTCHKMSG
                   IF      IDX-D               =   1
                       STRING  WRK-HKTCHKMSG-H
                               WRK-ERRDAY  (IDX-D)
                                               DELIMITED  BY  SPACE
                                               INTO    WRK-HKTCHKMSG
                       END-STRING
                   ELSE
                       STRING  WRK-HKTCHKMSG-H
                               ","
                               WRK-ERRDAY  (IDX-D)
                                               DELIMITED  BY  SPACE
                                               INTO    WRK-HKTCHKMSG
                       END-STRING
                   END-IF
               END-PERFORM
           END-IF
           MOVE    WRK-HKTCHKMSG       TO  SPA-GMN-HKTMSG
           .
       410112-NYUHKTMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤回数入力処理
      *****************************************************************
       41012-DAY-CHK-SEC             SECTION.
      *
           MOVE    2                   TO  SPA-GMN-CUR
      *
      *    剤未選択の時エラー
           IF     (SPA-GMN-BANGO       =   ZERO )
               OR (SPA-NAI-BANGO       =   ZERO )
               MOVE    1                   TO  SPA-GMN-CUR
               MOVE    "0013"              TO  SPA-ERRCD
               GO      TO      41012-DAY-CHK-EXT
           END-IF
      *    誕生日以前はエラー
           IF      SPA-BIRTHDAY(1:6)   =   SPA-NAI-SRYYM
               IF      SPA-DAY-IDX        <   SPA-BIRTHDAY(7:2)
                  AND (SPA-DAY-IDX        >   ZERO   )
                   MOVE    "0015"              TO  SPA-ERRCD
                   GO      TO      41012-DAY-CHK-EXT
               END-IF
           END-IF
      *
      *    一括入力とする
           MOVE    ZERO                TO  WRK-IDX
           MOVE    ZERO                TO  WRK-IDX-P
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   31
               IF     (J02-DAY(IDX)(1:1)   =   "/" )
                   OR (J02-DAY(IDX)(2:1)   =   "/" )
                   OR (J02-DAY(IDX)(3:1)   =   "/" )
                   IF     WRK-IDX-P        =   ZERO
                       MOVE    IDX             TO  WRK-IDX-P
                   END-IF
               ELSE
      *
               INITIALIZE                      ORCSNUMAREA
               MOVE    J02-DAY(IDX)     TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   IF      WRK-IDX             =   ZERO
                       MOVE    "0003"              TO  SPA-ERRCD
                       MOVE    IDX                 TO  WRK-IDX
                   END-IF
               ELSE
                   MOVE    SNUM-OUTNUM     TO  SPA-GMN-DAY (IDX)
               END-IF
      *
               END-IF
           END-PERFORM
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    WRK-IDX         TO  SPA-DAY-IDX
               GO      TO     41012-DAY-CHK-EXT
           END-IF
      *ver.4.7
      *    Ｊ０２２の判定
           IF      WRK-IDX-P       NOT =   ZERO
               MOVE    WRK-IDX-P       TO  SPA-DAY-IDX
               IF      SPA-GMN-NYUGAIKBN   =   "1"
                   IF     (SPA-NAI-NYUIN (SPA-DAY-IDX)   =   3     )
                       MOVE    2               TO  SPA-J022-MAX
                       PERFORM 410121-J022-SET-SEC
                   ELSE
                       MOVE    "0080"          TO  SPA-ERRCD
                   END-IF
               ELSE
      *            受診歴有無のチェック処理
                   PERFORM 410122-DAY-JYRRKCHK-SEC
                   IF      WRK-RENNUM          >   1
                       MOVE    WRK-RENNUM          TO  SPA-J022-MAX
                       PERFORM 410121-J022-SET-SEC
                   ELSE
                       MOVE    "0081"              TO  SPA-ERRCD
                   END-IF
               END-IF
               GO      TO     41012-DAY-CHK-EXT
           END-IF
      *
      *    受診歴有無のチェック
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL    (IDX         >   31  )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-GMN-DAY (IDX)   >   ZERO
                   IF      IDX             >   SPA-SRYMATU
                       MOVE    "0062"              TO  SPA-ERRCD
                   END-IF
      *H30.12      誕生日以前はエラー
                   IF     (SPA-ERRCD       =   SPACE )
                      AND (SPA-BIRTHDAY(1:6)   =   SPA-NAI-SRYYM)
                       MOVE    IDX                 TO  IDD
                       IF      IDD        <   SPA-BIRTHDAY(7:2)
                           MOVE    "0015"              TO  SPA-ERRCD
                       END-IF
                   END-IF
      *
                   IF     (SPA-ERRCD       =   SPACE )
      *                受診歴有無のチェック
                       PERFORM 410121-RRK-DAYCHK-SEC
                   END-IF
                   IF      SPA-ERRCD       NOT =   SPACE
                       MOVE    IDX             TO  WRK-IDX
                       MOVE    WRK-IDX         TO  SPA-DAY-IDX
                   END-IF
               END-IF
           END-PERFORM
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    WRK-IDX         TO  SPA-DAY-IDX
               GO      TO     41012-DAY-CHK-EXT
           END-IF
      *
      *
      *    IF      SPA-GMN-DAY (SPA-DAY-IDX)   =   SPA-GNAI-DAY
      *                                            (1 SPA-DAY-IDX)
      *        CONTINUE
      *****ELSE
      *    今回入力位置
           IF    (MCP-EVENT       =   "ACTIVATE"  ) AND
                 (MCP-WIDGET(1:3) =   "DAY"       )
               MOVE    MCP-WIDGET(4:2)     TO  IDX-IN
           ELSE
               MOVE    ZERO                TO  IDX-IN
           END-IF
      *    同日複数回数の判定
           PERFORM 410129-DAY-NAICHK-SEC
      *
           IF      FLG-END             =   1
               GO      TO      41012-DAY-CHK-EXT
           END-IF
      *    入院料包括日編集処理
           IF      SPA-NAI-HKTGAIFLG   =   1
               PERFORM 410112-NYUHKTMSG-SEC
           END-IF
      *
      *    ＯＫの時、次ぎの日へ
           IF    (MCP-EVENT       =   "ACTIVATE"  ) AND
                 (MCP-WIDGET(1:3) =   "DAY"       )
               MOVE    MCP-WIDGET(4:2)     TO  SPA-DAY-IDX
           END-IF
           IF      SPA-ERRCD           =   SPACE
               ADD     1               TO  SPA-DAY-IDX
               IF      SPA-DAY-IDX         >   SPA-SRYMATU
                   MOVE    5                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       41012-DAY-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診歴有無のチェック処理
      *****************************************************************
       410122-DAY-JYRRKCHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  WRK-RENNUM
           MOVE    SPA-NAI-SRYYM       TO  WRK-YM
           MOVE    SPA-DAY-IDX         TO  WRK-DD
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-RRK-MAX )
               IF     (SPA-RRK-RRKYMD (IDX)
                                           =   WRK-YMD )
               AND    (SPA-RRK-SRYKA (IDX)
                                           =   SPA-NAIGMN-SRYKA )
               AND    (SPA-RRK-HKNCOMBI (IDX)
                                           =   SPA-GMN-HKNCOMBI)
                   IF      SPA-RRK-RENNUM (IDX)    >   WRK-RENNUM
                       MOVE    SPA-RRK-RENNUM (IDX)    TO  WRK-RENNUM
                   END-IF
               END-IF
           END-PERFORM
           .
       410122-DAY-JYRRKCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診歴有無のチェック処理
      *****************************************************************
       410129-DAY-NAICHK-SEC           SECTION.
      *
           PERFORM VARYING     IDX-IN2         FROM    1   BY  1
                   UNTIL      (IDX-IN2         >   31  )  OR
                              (SPA-ERRCD   NOT =   SPACE) OR
                              (FLG-END         =   1    )
               MOVE    IDX-IN2             TO  SPA-DAY-IDX
               IF     (SPA-GMN-DAY (SPA-DAY-IDX)
                                       NOT =   SPA-GNAI-DAY
                                                    (1 SPA-DAY-IDX))
                   OR (SPA-DAY-IDX         =   IDX-IN )
      *            今回変更があり、履歴がある時
      *!           IF     (SPA-GNAI-DAY (3 SPA-DAY-IDX)  >   ZERO  )  OR
      *!                  (SPA-GNAI-DAY (4 SPA-DAY-IDX)  >   ZERO  )
      *            IF    ((SPA-GNAI-DAY (2 SPA-DAY-IDX)  >   ZERO  ) AND
      *                   (SPA-GNAI-DAY (3 SPA-DAY-IDX)  >   ZERO  )) OR
      *                  ((SPA-GNAI-DAY (2 SPA-DAY-IDX)  >   ZERO  ) AND
      *                   (SPA-GNAI-DAY (4 SPA-DAY-IDX)  >   ZERO  )) OR
      *                  ((SPA-GNAI-DAY (3 SPA-DAY-IDX)  >   ZERO  ) AND
      ***                 (SPA-GNAI-DAY (4 SPA-DAY-IDX)  >   ZERO  ))
      *            履歴の回数
                   PERFORM 4101291-GMNDAY-DAYCHK-SEC
                   IF      WRK-CNT-DAY         >   1
                       PERFORM 410122-DAY-JYRRKCHK-SEC
                       MOVE    WRK-RENNUM          TO  SPA-J022-MAX
                       PERFORM 410121-J022-SET-SEC
                   ELSE
      *ver.4.7
      *                入院同日再入院日
                       IF      (SPA-NYUGAIKBN          =   "1"  )  AND
                               (SPA-NAI-NYUIN (SPA-DAY-IDX)  =   3 )
                         AND   (SPA-GMN-DAY (SPA-DAY-IDX)  NOT =   ZERO)
                           PERFORM 410121-J022-SET-SEC
                       ELSE
                           PERFORM 4101291-GMNDAY-NAI-SEC
                       END-IF
                   END-IF
                END-IF
           END-PERFORM
      *
           .
       410129-DAY-NAICHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    変更日の編集処理
      *****************************************************************
       4101291-GMNDAY-DAYCHK-SEC        SECTION.
      *
           MOVE    ZERO                TO  WRK-CNT-DAY
           PERFORM VARYING     IDXD     FROM    2   BY  1
                   UNTIL       IDXD     >   10
               IF      SPA-GNAI-DAY (IDXD SPA-DAY-IDX)   >   ZERO
                   ADD     1              TO  WRK-CNT-DAY
               END-IF
           END-PERFORM
      *
           .
       4101291-GMNDAY-DAYCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    変更日の編集処理
      *****************************************************************
       4101291-GMNDAY-NAI-SEC           SECTION.
      *
           MOVE    SPA-GMN-DAY (SPA-DAY-IDX)
                                       TO  SPA-GNAI-DAY (1 SPA-DAY-IDX)
      ***  MOVE    2                   TO  IDR2
      *    IF     (SPA-GNAI-DAY (3 SPA-DAY-IDX)  >   ZERO  )
      *        MOVE    3               TO  IDR2
      *    ELSE
      *        IF     (SPA-GNAI-DAY (4 SPA-DAY-IDX)  >   ZERO  )
      *            MOVE    4               TO  IDR2
      *        END-IF
      **** END-IF
           MOVE    ZERO                TO  IDR2
           PERFORM VARYING     IDXD    FROM    2   BY  1
                   UNTIL      (IDXD        >   10  )
                         OR   (IDR2        >   ZERO )
               IF      SPA-GNAI-DAY (IDXD SPA-DAY-IDX)   >   ZERO
                   MOVE    IDXD            TO  IDR2
               END-IF
           END-PERFORM
           IF      IDR2                =   ZERO
               MOVE    2               TO  IDR2
           END-IF
           MOVE    SPA-GMN-DAY (SPA-DAY-IDX)
                                       TO  SPA-GNAI-DAY
                                                    (IDR2 SPA-DAY-IDX)
      *
           .
       4101291-GMNDAY-NAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診歴有無のチェック処理
      *****************************************************************
       410121-RRK-DAYCHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDR     FROM    1   BY   1
                   UNTIL      (IDR     >   SPA-RRK-MAX  )  OR
                              (FLG-OK      =   1        )
               IF     (SPA-NAIGMN-SRYKA    =   SPA-RRK-SRYKA (IDR)) AND
                      (SPA-GMN-HKNCOMBI    =   SPA-RRK-HKNCOMBI(IDR))
                   MOVE    SPA-RRK-RRKYMD(IDR)(7:2)    TO  IDR2
                   IF      IDR2                =   IDX
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-NAI-SRYYM       TO  WRK-YMD(1:6)
               MOVE    IDX                 TO  IDX-D2
               MOVE    IDX-D2              TO  WRK-YMD(7:2)
      *        外来の時エラー
               IF      SPA-GMN-NYUGAIKBN   =   "2"
      *************MOVE    "0004"              TO  SPA-ERRCD
      *            入院期間中判定
                   IF     (SPA-NAI-NYUIN (IDX) =   1 )
      *H30.7         自院歴は対象
                      AND (SPA-NAI-NYUIN-JR(IDX)   =   ZERO )
                       IF    ((SPA-NAI-HKNNUM      =   "971" ) AND
                              (SPA-NAI-RSI-HKNKBN  =   "3"   ))
                         OR  ( SPA-NAI-HKNNUM(1:2) =   "98"  )
                           CONTINUE
                       ELSE
                           MOVE    "0028"              TO  SPA-ERRCD
                       END-IF
                   END-IF
               ELSE
      *            入院の時、入院期間中ならＯＫ
                   IF      SPA-NAI-NYUIN (IDX) =   ZERO
                       MOVE    "0026"              TO  SPA-ERRCD
                   END-IF
      *            アフターケアはエラー
                   IF     (SPA-NAI-HKNNUM      =   "971" ) AND
                          (SPA-NAI-RSI-HKNKBN  =   "3"   )
                       MOVE    "0029"              TO  SPA-ERRCD
                   END-IF
               END-IF
      *        保険が有効期間かの判定
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING     IDR     FROM    1   BY   1
                       UNTIL      (IDR     >   SPA-HKN-MAX  )  OR
                                  (SPA-ERRCD   NOT =   SPACE)  OR
                                  (FLG-OK      =   1        )
                   IF     SPA-GMN-HKNCOMBI     =   SPA-GMN-THKNCOMBINUM
                                                              (IDR)
                       IF     (SPA-NAI-TTEKSTYMD(IDR)  <=  WRK-YMD) AND
                              (SPA-NAI-TTEKEDYMD(IDR)  >=  WRK-YMD)
                           MOVE    1               TO  FLG-OK
                      END-IF
                   END-IF
               END-PERFORM
               IF      FLG-OK              =   ZERO
                   IF      SPA-ERRCD           =   SPACE
                       MOVE    "0025"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           .
       410121-RRK-DAYCHK-EXT.
           EXIT.
      *****************************************************************
      *    回数分割入力画面へ（Ｊ０２２）処理
      *****************************************************************
       410121-J022-SET-SEC             SECTION.
      *
           MOVE    SPA-GMN-DAY (SPA-DAY-IDX)
                                       TO  SPA-GNAI-DAY
                                                        (1 SPA-DAY-IDX)
      *    INITIALIZE                  SPA-J022-AREA
      *    MOVE    1                   TO  SPA-NAI-DAYCHK
      *                                                 (SPA-DAY-IDX)
      *    MOVE    SPA-GNAI-DAY (2 SPA-DAY-IDX)
      *                                TO  SPA-NAI-DAY1 (SPA-DAY-IDX)
      *    MOVE    SPA-GNAI-DAY (3 SPA-DAY-IDX)
      *                                TO  SPA-NAI-DAY2 (SPA-DAY-IDX)
      *    MOVE    SPA-GNAI-DAY (4 SPA-DAY-IDX)
      *                                TO  SPA-NAI-DAY3 (SPA-DAY-IDX)
      *
      ***  MOVE    SPA-DAY-IDX         TO  SPA-J022-IDX
      **** 開始位置
      *    PERFORM VARYING     IDX     FROM    1   BY  1
      *            UNTIL       IDX     >   31
      *        IF      SPA-NAI-DAYCHK (IDX)    =   1
      *            MOVE    IDX             TO  SPA-J022-IDX
      *            MOVE    31              TO  IDX
      *        END-IF
      *****END-PERFORM
      *
      *    MOVE    SPA-GNAI-DAY (2 SPA-J022-IDX) 
      *                                TO  SPA-J022-DAY1
      *    MOVE    SPA-GNAI-DAY (3 SPA-J022-IDX) 
      *                                TO  SPA-J022-DAY2
      *    MOVE    SPA-GNAI-DAY (4 SPA-J022-IDX) 
      *                                TO  SPA-J022-DAY3
      *
      *    日付変換
      *    MOVE    SPA-NAI-SRYYM       TO  WRK-SYMD(1:6)
      *    MOVE    SPA-J022-IDX        TO  WRK-SDD
      *    INITIALIZE                  LNK-DAY2-AREA
      *    MOVE    "21"                TO  LNK-DAY2-IRAI
      *    MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
      *    CALL    "ORCSDAY"           USING  STS-AREA-DAY
      *                                       LNK-DAY2-AREA
      *    IF      STS-DAY-RC1         =   ZERO
      *        MOVE  LNK-DAY2-EDTYMD3      TO  SPA-HENNYMD
      *    END-IF
      *
      *    MOVE    SPACE               TO  J022
      *
      *    画面編集
      *    MOVE    SPA-HENNYMD(11:)    TO  J022-SRYYMD
      *    MOVE    SPA-J022-DAY1       TO  J022-DAY1
      *    MOVE    SPA-J022-DAY2       TO  J022-DAY2
      *    MOVE    SPA-J022-DAY3       TO  J022-DAY3
      *
      **   INITIALIZE                  SPA-J021-AREA
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-J01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-J02             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGJ022"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-J02
           MOVE    SPA-WORK        TO  SPA-J01KYOUTU
      *
           MOVE    1                   TO  SPA-J022-CUR
      *
      *    カーソル移動
           MOVE    "DAY1"              TO  MCP-WIDGET
      *
      *
           MOVE    "J02"               TO  SPA-MOTOPG
           MOVE    "J022"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "J022"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       410121-J022-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤回数一括入力処理
      *****************************************************************
       41019-IKKATU-CHK-SEC             SECTION.
      *
           MOVE    11                  TO  SPA-GMN-CUR
      *
      *    剤未選択の時エラー
           IF     (SPA-GMN-BANGO       =   ZERO )
               OR (SPA-NAI-BANGO       =   ZERO )
               MOVE    1                   TO  SPA-GMN-CUR
               MOVE    "0013"              TO  SPA-ERRCD
               GO      TO      41019-IKKATU-CHK-EXT
           END-IF
      *
           MOVE    J02-IKKATU          TO  SPA-GMN-IKKATU
      *
           INITIALIZE                      WRK-DAY-AREA
           MOVE    ZERO                TO  IDW3
           MOVE    ZERO                TO  IDW2
           MOVE    ZERO                TO  IDW3
           MOVE    ZERO                TO  IDW4
           MOVE    ZERO                TO  FLG-K1
           MOVE    ZERO                TO  FLG-K2
           MOVE    1                   TO  IDW1
           MOVE    ZERO                TO  FLG-IKKATUFLG
      *
           MOVE    SPACE               TO  WRK-HZNSU
           MOVE    ZERO                TO  IDW1
           MOVE    ZERO                TO  IDW9
      *
           PERFORM VARYING    IDW5     FROM    1   BY  1
      **********   UNTIL     (IDW5     >   20  )  OR
                   UNTIL     (IDW5     >   99  )  OR
                             (SPA-ERRCD    NOT =   SPACE )
      *
               EVALUATE    SPA-GMN-IKKATU(IDW5:1)
                   WHEN    "/"
                       MOVE    1           TO  FLG-K1
                       IF      IDW9        >   ZERO
      *                回数
                           ADD     1              TO  IDW1
                           MOVE    WRK-HZNSU      TO  WRK-KAISUT(IDW1)
                           MOVE    SPACE          TO  WRK-HZNSU
                           MOVE    ZERO           TO  IDW9
                           MOVE    ZERO           TO  IDW2
                       ELSE
                           IF      IDW1            =   ZERO
                               ADD     1               TO  IDW1
                           END-IF
                       END-IF
                   WHEN    ","
                   WHEN    "."
      *H30.8
      *            回数がない
                     IF     (FLG-K1              =   ZERO  )
                        MOVE    "0065"              TO  SPA-ERRCD
                     ELSE
      *
                       IF      FLG-K2      =   1
      *                終了日
                           MOVE    WRK-HZNSU       TO  WRK-DAY2
                                                            (IDW1 IDW2)
                           MOVE    SPACE           TO  WRK-HZNSU
                           MOVE    ZERO            TO  IDW9
                       ELSE
      *                開始日
                           IF      IDW9        >   ZERO
                               ADD     1               TO  IDW2
                               MOVE    WRK-HZNSU       TO  WRK-DAY1
                                                           (IDW1 IDW2)
                               MOVE    SPACE           TO  WRK-HZNSU
                               MOVE    ZERO            TO  IDW9
                           END-IF
                       END-IF
                       MOVE    ZERO        TO  FLG-K2
      *H30.8
                     END-IF
                   WHEN    "-"
      *H30.8
      *            回数がない
                     IF     (FLG-K1              =   ZERO  )
                        MOVE    "0065"              TO  SPA-ERRCD
                     ELSE
                       MOVE    1           TO  FLG-K2
      *                開始日
                       IF      IDW9        >   ZERO
                           ADD     1               TO  IDW2
                           MOVE    WRK-HZNSU       TO  WRK-DAY1
                                                           (IDW1 IDW2)
                           MOVE    SPACE           TO  WRK-HZNSU
                           MOVE    ZERO            TO  IDW9
                       END-IF
      *H30.8
                     END-IF
                   WHEN    "0"  THRU   "9"
                       ADD     1               TO  IDW9
                       MOVE    SPA-GMN-IKKATU (IDW5:1)
                                               TO  WRK-HZNSU (IDW9:1)
                   WHEN    SPACE
                       CONTINUE
                   WHEN    OTHER
                       MOVE    "0061"              TO  SPA-ERRCD
               END-EVALUATE
           END-PERFORM
      *
      *H30.8
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41019-IKKATU-CHK-EXT
           END-IF
      *
           IF      IDW9            >   ZERO
               IF      IDW1            =   ZERO
                   ADD     1               TO  IDW1
               END-IF
               IF      FLG-K2          =   1
      *        終了日
                   MOVE    WRK-HZNSU       TO  WRK-DAY2(IDW1 IDW2)
               ELSE
      *        開始日
                   ADD     1               TO  IDW2
                   MOVE    WRK-HZNSU       TO  WRK-DAY1(IDW1 IDW2)
               END-IF
           END-IF
      *    回数がない
           IF     (FLG-K1              =   ZERO  )  AND
                  (SPA-GMN-IKKATU  NOT =   SPACE )
               MOVE    "0061"              TO  SPA-ERRCD
           END-IF
      *
           PERFORM VARYING    IDW1     FROM    1   BY  1
      ****         UNTIL     (IDW1     >   60  )  OR
                   UNTIL     (IDW1     >   99  )  OR
                             (SPA-ERRCD    NOT =   SPACE ) OR
                            ((WRK-KAISUT(IDW1) =   SPACE )  AND
                             (WRK-DAY1  (IDW1 1) =   SPACE )  AND
                             (WRK-DAY2  (IDW1 1) =   SPACE ))
      *        回数
               IF      WRK-KAISUT(IDW1)    =   SPACE
                   MOVE    1               TO  WRK-KAISU-9
               ELSE
                   INITIALIZE                      ORCSNUMAREA
                   MOVE    WRK-KAISUT(IDW1)    TO  SNUM-INX
                   CALL    "ORCSNUM"           USING
                                               ORCSNUMAREA
                   IF     (SNUM-RC         NOT =   ZERO  )   OR
                          (SNUM-MINKBN     NOT =   SPACE )   OR
                          (SNUM-SYOKBN     NOT =   SPACE )   OR
                          (SNUM-SEISUU         >   3     )
                       MOVE    "0061"              TO  SPA-ERRCD
                   ELSE
                       MOVE    SNUM-OUTNUM         TO  WRK-KAISU-9
                   END-IF
               END-IF
               PERFORM VARYING    IDW2     FROM    1   BY  1
     ***               UNTIL     (IDW2     >   30  )  OR
                       UNTIL     (IDW2     >   99  )  OR
                             (SPA-ERRCD    NOT =   SPACE ) OR
                            ((WRK-DAY1  (IDW1 IDW2) =   SPACE )  AND
                             (WRK-DAY2  (IDW1 IDW2) =   SPACE ))
      *        開始日
               IF      WRK-DAY1  (IDW1 IDW2)   =   SPACE
                   MOVE    ZERO            TO  WRK-DAY1-9(IDW2)
               ELSE
                   INITIALIZE                      ORCSNUMAREA
                   MOVE    WRK-DAY1 (IDW1 IDW2)     TO  SNUM-INX
                   CALL    "ORCSNUM"           USING
                                               ORCSNUMAREA
                   IF     (SNUM-RC         NOT =   ZERO  )   OR
                          (SNUM-MINKBN     NOT =   SPACE )   OR
                          (SNUM-SYOKBN     NOT =   SPACE )   OR
                          (SNUM-SEISUU         >   2     )
                       MOVE    "0061"              TO  SPA-ERRCD
                   ELSE
                       MOVE    SNUM-OUTNUM         TO  WRK-DAY1-9(IDW2)
                   END-IF
               END-IF
      *        終了日
               IF      WRK-DAY2  (IDW1 IDW2)    =   SPACE
                   MOVE    ZERO            TO  WRK-DAY2-9(IDW2)
               ELSE
                   INITIALIZE                      ORCSNUMAREA
                   MOVE    WRK-DAY2 (IDW1 IDW2)     TO  SNUM-INX
                   CALL    "ORCSNUM"           USING
                                               ORCSNUMAREA
                   IF     (SNUM-RC         NOT =   ZERO  )   OR
                          (SNUM-MINKBN     NOT =   SPACE )   OR
                          (SNUM-SYOKBN     NOT =   SPACE )   OR
                          (SNUM-SEISUU         >   3     )
                       MOVE    "0061"              TO  SPA-ERRCD
                   ELSE
                       MOVE    SNUM-OUTNUM         TO  WRK-DAY2-9(IDW2)
                   END-IF
               END-IF
      *        回数編集
               IF     (WRK-DAY1-9 (IDW2)   >   ZERO )  AND
                      (WRK-DAY2-9 (IDW2)   =   ZERO )
                   MOVE    WRK-DAY1-9 (IDW2)   TO  WRK-DAY2-9(IDW2)
               END-IF
               IF     (WRK-DAY1-9(IDW2)    >   WRK-DAY2-9(IDW2) )
                   MOVE    "0061"              TO  SPA-ERRCD
               END-IF
               IF     (WRK-DAY1-9 (IDW2)   >   SPA-SRYMATU )  OR
                      (WRK-DAY2-9 (IDW2)   >   SPA-SRYMATU )
                   MOVE    "0062"              TO  SPA-ERRCD
               END-IF
      *
               PERFORM VARYING IDW3    FROM    WRK-DAY1-9(IDW2)  BY  1
                       UNTIL  (IDW3        >   WRK-DAY2-9 (IDW2))  OR
                              (IDW3        >   31         )  OR
                              (SPA-ERRCD   NOT =   SPACE )
                   IF      WRK-DAY(IDW3)   NOT =   ZERO
                       MOVE    "0063"              TO  SPA-ERRCD
                   ELSE
                       MOVE    WRK-KAISU-9     TO  WRK-DAY(IDW3)
                       IF      WRK-KAISU-9     =   ZERO
                           MOVE    1               TO  WRK-DAYDEL (IDW3)
                       END-IF
                   END-IF
               END-PERFORM
      *
               END-PERFORM
      *
           END-PERFORM
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41019-IKKATU-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-DAY-G       TO  WRK-SPA-GMN-DAY-G
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               IF      WRK-DAY(IDX-D)      >   ZERO
      *            誕生日以前はエラー
                   IF     (SPA-BIRTHDAY(1:6)   =   SPA-NAI-SRYYM ) AND
                          (IDX-D               <   SPA-BIRTHDAY(7:2))
                       MOVE    "0015"              TO  SPA-ERRCD
                   ELSE
                       MOVE    WRK-DAY(IDX-D)  TO  SPA-GMN-DAY  (IDX-D)
                   END-IF
               ELSE
                   IF      WRK-DAYDEL(IDX-D)   =   1
                       MOVE    ZERO            TO  SPA-GMN-DAY (IDX-D)
                   END-IF
               END-IF
           END-PERFORM
      *    受診歴有無のチェック
           PERFORM VARYING   IDX     FROM    1   BY  1
                   UNTIL    (IDX       >   31  )  OR
                            (SPA-ERRCD NOT =   SPACE )
               IF      SPA-GMN-DAY (IDX)   >   ZERO
      *            受診歴有無のチェック
                   PERFORM 410121-RRK-DAYCHK-SEC
               END-IF
           END-PERFORM
           IF      SPA-ERRCD           =   SPACE
      *        同日複数回の判定
               MOVE    ZERO                TO  IDX-IN
      *        同日複数回数の判定
               PERFORM 410129-DAY-NAICHK-SEC
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    WRK-SPA-GMN-DAY-G   TO  SPA-GMN-DAY-G
           END-IF
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (FLG-END             =   ZERO )
               MOVE    SPACE               TO  SPA-GMN-IKKATU
      *        入院料包括日編集処理
               IF      SPA-NAI-HKTGAIFLG   =   1
                   PERFORM 410112-NYUHKTMSG-SEC
               END-IF
           END-IF
      *
           .
       41019-IKKATU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細選択処理
      *****************************************************************
       41012-DATALIST-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
           MOVE    ZERO                TO  WRK-SELNUM2
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX
              IF      J02-DATALIST-SELECT (IDX)   =  "T"
      ************IF      SPA-GMN-TBANGO(IDX)     =   SPA-GMN-BANGO
      *               MOVE    SPA-GMN-TBANGO (IDX)    TO  WRK-SELNUM
      *           ELSE
      *************** MOVE    SPA-GMN-TBANGO (IDX)    TO  WRK-SELNUM2
                  IF      SPA-NAI-TBANGO2(IDX)    =   SPA-GMN-BANGO
                      MOVE    SPA-NAI-TBANGO2 (IDX)   TO  WRK-SELNUM
                  ELSE
                      MOVE    SPA-NAI-TBANGO2 (IDX)   TO  WRK-SELNUM2
                  END-IF
              END-IF
           END-PERFORM
      *
           IF     (WRK-SELNUM          =   ZERO )  AND
                  (WRK-SELNUM2     NOT =   ZERO )
               MOVE    WRK-SELNUM2         TO  SPA-GMN-BANGO
           ELSE
               IF      WRK-SELNUM2         =   ZERO
                   CONTINUE
               ELSE
                   MOVE    SPA-GMN-BANGO       TO  SPA-MAE-BANGO
                   MOVE    SPA-NAI-BANGO       TO  SPA-MAEN-BANGO
                   MOVE    WRK-SELNUM2         TO  SPA-GMN-BANGO
                   MOVE    "0103"              TO  SPA-JIDCD
                   GO      TO      41012-DATALIST-EXT
               END-IF
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-CUR
           MOVE    SPA-GMN-BANGO       TO  J02-NUM
           PERFORM 41011-BANGO-CHK-SEC
      *
           .
       41012-DATALIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴選択処理
      *****************************************************************
       41014-RRKSELNUMI-CHK-SEC             SECTION.
      *
      *    剤修正のチェック
      **** IF      SPA-GMN-BANGO   NOT =   ZERO
      *        MOVE    "0009"          TO  SPA-ERRCD
      *        GO      TO      41014-RRKSELNUMI-CHK-EXT
      **** END-IF
      *
           MOVE    3                   TO  SPA-GMN-CUR
      *
           MOVE    J02-RRKSELNUM       TO  SPA-GMN-RRKSELNUM
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    J02-RRKSELNUM       TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    ZERO                TO  SPA-NAI-RRKSELNUM
               GO      TO      41014-RRKSELNUMI-CHK-EXT
           ELSE
               MOVE    SNUM-OUTNUM         TO  SPA-NAI-RRKSELNUM
               MOVE    SPA-NAI-RRKSELNUM   TO  WRK-ZZZZ
               MOVE    WRK-ZZZZ-X          TO  SPA-GMN-RRKSELNUM
           END-IF
      *
           IF      SPA-NAI-RRKSELNUM   >   SPA-RRK-MAX
               MOVE    "1005"          TO  SPA-ERRCD
           ELSE
               IF      SPA-NAI-RRKSELNUM   =   ZERO
                   MOVE    "1005"          TO  SPA-ERRCD
               ELSE
      *            受診日変更とする
                   MOVE    2               TO  SPA-J02-SYORI
                   MOVE    4               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *H25.2
      *    レセ電移行分はエラー
           IF     (SPA-ERRCD           =   SPACE    )
             AND  (SPA-NAI-RRKSELNUM   NOT =   ZERO )
             AND  (SPA-RRK-SRYKA (SPA-NAI-RRKSELNUM)(1:1)
                                       =   "A"   )
               MOVE    "7011"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       41014-RRKSELNUMI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細選択処理
      *****************************************************************
       41015-RRKLIST-SEC             SECTION.
      *
      *    剤修正のチェック
      *    IF      SPA-GMN-BANGO   NOT =   ZERO
      *        MOVE    "0009"          TO  SPA-ERRCD
      *        GO      TO      41015-RRKLIST-EXT
      *    END-IF
      *
           MOVE    ZERO                TO  WRK-SELNUM
           MOVE    ZERO                TO  WRK-SELNUM2
      *
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-RRK-MAX
              IF      J02-RRKLIST-SELECT (IDX)    =  "T"
                  IF      SPA-GMN-RRKNO (IDX)     =   SPA-NAI-RRKSELNUM
                      MOVE    SPA-GMN-RRKNO (IDX)     TO  WRK-SELNUM
                  ELSE
                      MOVE    SPA-GMN-RRKNO (IDX)     TO  WRK-SELNUM2
                  END-IF
              END-IF
           END-PERFORM
      *
           IF     (WRK-SELNUM          =   ZERO )  AND
                  (WRK-SELNUM2     NOT =   ZERO )
               MOVE    WRK-SELNUM2         TO  SPA-NAI-RRKSELNUM
           ELSE
               IF      WRK-SELNUM2         =   ZERO
                   CONTINUE
               ELSE
                   MOVE    WRK-SELNUM2         TO  SPA-NAI-RRKSELNUM
               END-IF
           END-IF
      *
      *    IF     (WRK-SELNUM          =   ZERO )  AND
      *           (WRK-SELNUM2     NOT =   ZERO )
      *        MOVE    WRK-SELNUM2         TO  SPA-NAI-RRKSELNUM
      *    ELSE
      *        IF     (WRK-SELNUM      NOT =   ZERO )  AND
      *               (WRK-SELNUM2         =   ZERO )
      *            MOVE    WRK-SELNUM          TO  SPA-NAI-RRKSELNUM
      *        ELSE
      *            MOVE    WRK-SELNUM2         TO  SPA-NAI-RRKSELNUM
      *        END-IF
      *    END-IF
      *
      *
           IF      SPA-NAI-RRKSELNUM    =   ZERO
               MOVE    SPACE               TO  SPA-GMN-RRKSELNUM
           ELSE
               MOVE    SPA-NAI-RRKSELNUM   TO  J02-RRKSELNUM
               PERFORM 41014-RRKSELNUMI-CHK-SEC
           END-IF
      *
           .
       41015-RRKLIST-EXT.
           EXIT.
      *
      *****************************************************************
      *   変更受診日処理
      *****************************************************************
       41016-HENSRYYMD-CHK-SEC             SECTION.
      *
           IF      SPA-LOCK            =   ZERO
               CONTINUE
           ELSE
               MOVE    "0076"          TO  SPA-ERRCD
           END-IF
      *    剤修正のチェック
           IF      SPA-GMN-BANGO   NOT =   ZERO
               IF      SPA-ERRCD           =   SPACE
                   MOVE    "0009"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF     (SPA-J02-SYORI       =   2  )  AND
                  (SPA-NAI-RRKSELNUM   >   ZERO )
               CONTINUE
           ELSE
               IF      SPA-ERRCD           =   SPACE
                   MOVE    "0012"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        変更受診日チェック処理
               PERFORM 410161-HENSRYYMD-INCHK-SEC
           END-IF
      *
           .
       41016-HENSRYYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *   変更受診日チェック処理
      *****************************************************************
       410161-HENSRYYMD-INCHK-SEC             SECTION.
      *
           MOVE    4                   TO  SPA-GMN-CUR
           IF      J02-HENSRYYMD   NOT =   SPA-GMN-HENSRYYMD
               MOVE    ZERO                TO  SPA-NAI-KEIFLG
           END-IF
           MOVE    ZERO                TO  SPA-NAI-KEIFLG2
      *
           MOVE    J02-HENSRYYMD       TO  SPA-GMN-HENSRYYMD
      *
      *    数字２桁とする
           INITIALIZE                      ORCSNUMAREA
           MOVE    J02-HENSRYYMD(1:2)  TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "0007"              TO  SPA-ERRCD
           ELSE
               MOVE    SNUM-OUTNUM         TO  WRK-DD
               MOVE    WRK-DD              TO  SPA-GMN-HENSRYYMD
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPA-NAI-SRYYM       TO  SPA-NAI-HENSRYYMD(1:6)
               MOVE    SPA-GMN-HENSRYYMD   TO  SPA-NAI-HENSRYYMD(7:2)
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY1-AREA
               MOVE    "11"                TO  LNK-DAY1-IRAI
               MOVE    SPA-NAI-HENSRYYMD   TO  LNK-DAY1-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY1-AREA
               IF      STS-DAY-RC1         =   ZERO
                   IF      SPA-BIRTHDAY        >   SPA-NAI-HENSRYYMD
                       MOVE    "0015"              TO  SPA-ERRCD
                   END-IF
               ELSE
                   MOVE    "0007"          TO  SPA-ERRCD
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
      *        受診日チェック
               PERFORM 410161-HENSRYDAY-CHK-SEC
           END-IF
           .
       410161-HENSRYYMD-INCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *   変更受診日チェック処理
      *****************************************************************
       410161-HENSRYDAY-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    SPACE               TO  WRK-HKNNUM
           MOVE    SPACE               TO  WRK-RSI-HKNKBN
           PERFORM VARYING     IDR     FROM    1   BY   1
                   UNTIL      (IDR     >   SPA-HKN-MAX  )  OR
                              (FLG-OK      =   1        )
                IF     SPA-RRK-HKNCOMBI(SPA-NAI-RRKSELNUM)
                                           =   SPA-GMN-THKNCOMBINUM
                                                              (IDR)
                   MOVE    SPA-NAI-THKNNUM (IDR)   TO  WRK-HKNNUM
                   MOVE    SPA-NAI-TRSI-HKNKBN(IDR)
                                                   TO  WRK-RSI-HKNKBN
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           MOVE    SPA-NAI-HENSRYYMD(7:2)  TO  IDX-D2
           IF      SPA-NYUGAIKBN   =   "1"
      *        入院の時、入院期間中ならＯＫ
               IF      SPA-NAI-NYUIN (IDX-D2)  =   ZERO
                   MOVE    "0026"              TO  SPA-ERRCD
               END-IF
      *        入院の時、同日再入院はエラー
               IF      SPA-NAI-NYUIN (IDX-D2)  =   3
                   MOVE    "0083"              TO  SPA-ERRCD
               END-IF
      *        労災アフターケアは算定不可
               IF     (WRK-HKNNUM          =   "971" ) AND
                      (WRK-RSI-HKNKBN      =   "3"   )
                   MOVE    "0029"              TO  SPA-ERRCD
               END-IF
           ELSE
      *        外来の時、入院期間中ならエラー
               IF     (SPA-NAI-NYUIN (IDX-D2)  =   1  )
      *H30.7      自院歴は対象
                  AND (SPA-NAI-NYUIN-JR(IDX-D2)    =   ZERO )
                   IF    ((WRK-HKNNUM          =   "971" ) AND
                          (WRK-RSI-HKNKBN      =   "3"   ))
                     OR  ( WRK-HKNNUM(1:2)     =   "98"  )
                       CONTINUE
                   ELSE
                       MOVE    "0028"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *    既に算定のある日には変更できないものとする。
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  FLG-OK2
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-RRK-MAX
               IF     (SPA-RRK-RRKYMD (IDX)   =   SPA-NAI-HENSRYYMD) AND
                      (SPA-RRK-SRYKA  (SPA-NAI-RRKSELNUM)
                                           =   SPA-RRK-SRYKA (IDX))
                   IF      SPA-NYUGAIKBN   =   "1"
      *                入院の時、同じ保険はエラー
                       IF      SPA-RRK-HKNCOMBI(SPA-NAI-RRKSELNUM)
                                           =   SPA-RRK-HKNCOMBI(IDX)
                           MOVE    1               TO  FLG-OK2
                       ELSE
                           MOVE    1               TO  FLG-OK
                       END-IF
                   ELSE
      *                連番が９であればエラー
                       IF      SPA-RRK-RENNUM (IDX)    =   9
                           MOVE    "0078"              TO  SPA-ERRCD
                       END-IF
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-IF
      *!保険判定
      *        AND    (SPA-RRK-HKNCOMBI(SPA-NAI-RRKSELNUM)
      *                                    =   SPA-RRK-HKNCOMBI(IDX))
      *            MOVE    1               TO  FLG-OK
      *DDDDDDDDD   IF      SPA-NYUGAIKBN   =   "1"
      *                入院の時、同じ保険はエラー
      *                IF      SPA-RRK-HKNCOMBI(SPA-NAI-RRKSELNUM)
      *                                    =   SPA-RRK-HKNCOMBI(IDX)
      *                    MOVE    1               TO  FLG-OK
      *                END-IF
      *            ELSE
      *                外来の時、包括まとめ以外があればエラー
      *                IF    (SPA-RRK-HKNCOMBI(SPA-NAI-RRKSELNUM)
      *                                    =   "9999"  )  OR
      *                      (SPA-RRK-HKNCOMBI(IDX)
      *                                    =   "9999"  )
      *                    IF      SPA-RRK-HKNCOMBI(SPA-NAI-RRKSELNUM)
      *                                    =   SPA-RRK-HKNCOMBI(IDX)
      *                        MOVE    1               TO  FLG-OK
      *                    END-IF
      *                ELSE
      *                    MOVE    2               TO  FLG-OK
      ***************  END-IF
      *************END-IF
      ******** END-IF
           END-PERFORM
           MOVE    ZERO                TO  WRK-RRK-DOUJI-RENNUM
           MOVE    ZERO                TO  WRK-RRK-RENNUM
           IF     (SPA-RRK-GRP-DENPNUM(SPA-NAI-RRKSELNUM)
                                   NOT =   ZERO )  OR
                  (FLG-OK              =   ZERO )
               PERFORM 410162-GRP-DENPNUM-CHK-SEC
           END-IF
      *
      *    既に算定のある日には変更できないものとする。
           IF      SPA-ERRCD           =   SPACE
               IF      FLG-OK2             =   1
                   MOVE    "0011"          TO  SPA-ERRCD
               ELSE
                   IF      FLG-OK              =   1
                       MOVE    "0123"          TO  SPA-JIDCD
                   END-IF
               END-IF
      *        受診連番が３以上
               IF     (SPA-ERRCD       =   SPACE ) AND
                      (SPA-JIDCD       =   SPACE )
                   IF      SPA-NAI-KEIFLG2     =   1
                       MOVE    "0124"          TO  SPA-JIDCD
                   END-IF
               END-IF
           END-IF
           IF     (SPA-ERRCD           =   SPACE ) AND
                  (SPA-JIDCD           =   SPACE )
      *        更新の確認メッセージ
               PERFORM 41016-HENSRYYMD-KMSG-SEC
           END-IF
      *
           .
       410161-HENSRYDAY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    更新の確認メッセージ処理
      *****************************************************************
       41016-HENSRYYMD-KMSG-SEC      SECTION.
      *
           MOVE    "0115"              TO  SPA-JIDCD2
           IF      SPA-RRK-GRP-DENPNUM(SPA-NAI-RRKSELNUM)
                                   NOT =   ZERO
               MOVE    "0118"              TO  SPA-JIDCD2
           END-IF
           .
       41016-HENSRYYMD-KMSG-EXT.
           EXIT.
      *****************************************************************
      *    まとめ入力伝票番号チェック処理
      *****************************************************************
       410162-GRP-DENPNUM-CHK-SEC      SECTION.
      *
           INITIALIZE                  TBL-GRP-AREA
      *    受診履歴変更
           IF      SPA-RRK-GRP-DENPNUM(SPA-NAI-RRKSELNUM)
                                       =   ZERO
               MOVE    SPA-RRK-SRYKA  (SPA-NAI-RRKSELNUM)
                                           TO  TBL-SRYKA (1)
               MOVE    SPA-RRK-HKNCOMBI(SPA-NAI-RRKSELNUM)
                                           TO  TBL-HKNCOMBI(1 1)
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    SPA-HOSPNUM          TO  JYURRK-HOSPNUM
               MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
               MOVE    SPA-GMN-NYUGAIKBN   TO  JYURRK-NYUGAIKBN
               MOVE    SPA-RRK-GRP-DENPNUM(SPA-NAI-RRKSELNUM)
                                           TO  JYURRK-GRP-DENPNUM
      *
               MOVE    JYURRK-REC          TO  MCPDATA-REC
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key26"             TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                   MOVE    "key26"             TO  MCP-PATHNAME
                   PERFORM 970-JYURRK-READ-SEC
               ELSE
                   INITIALIZE                  JYURRK-REC
                   MOVE    1               TO  FLG-JYURRK
               END-IF
               PERFORM
                   UNTIL    FLG-JYURRK         =   1
                   PERFORM VARYING     IDX     FROM    1   BY  1
                           UNTIL       IDX     >   10
                       IF      TBL-SRYKA (IDX)     =   SPACE
                           MOVE    JYURRK-SRYKA    TO  TBL-SRYKA (IDX)
                       END-IF
                       IF      JYURRK-SRYKA    =   TBL-SRYKA (IDX)
                           PERFORM VARYING     IDX2    FROM    1   BY  1
                                   UNTIL       IDX2    >   10
                               IF      TBL-HKNCOMBI(IDX IDX2)  =   SPACE
                                   MOVE    JYURRK-HKNCOMBINUM
                                                   TO  TBL-HKNCOMBI
                                                              (IDX IDX2)
                               END-IF
                               IF      TBL-HKNCOMBI(IDX IDX2)
                                               =   JYURRK-HKNCOMBINUM
                                   MOVE    10              TO  IDX2
                               END-IF
                           END-PERFORM
                           MOVE    10              TO  IDX
                       END-IF
                   END-PERFORM
      *
                   MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                   MOVE    "key26"             TO  MCP-PATHNAME
                   PERFORM 970-JYURRK-READ-SEC
               END-PERFORM
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key26"             TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-IF
      *    複数の科の時
           IF     (SPA-GMN-SRYKA   NOT =   "00" )  AND
                  (TBL-SRYKA (2)   NOT =   SPACE )
               MOVE    "0060"              TO  SPA-ERRCD
               MOVE    06                  TO  SPA-GMN-CUR
           ELSE
               PERFORM VARYING     IDT     FROM    1   BY  1
                       UNTIL      (IDT     >   10      )  OR
                                  (TBL-SRYKA (IDT) =   SPACE )  OR
                                  (FLG-OK      =   1       )
                   MOVE    TBL-SRYKA (IDT)     TO  WRK-SRYKA
                   PERFORM VARYING     IDX     FROM    1   BY  1
                           UNTIL      (IDX     >   SPA-RRK-MAX )
      *********************      OR   (FLG-OK      =   1       )
                       IF     (SPA-RRK-RRKYMD (IDX)
                                               =   SPA-NAI-HENSRYYMD)
                       AND    (SPA-RRK-SRYKA (IDX)
                                               =   WRK-SRYKA   )
      *v48   連番上限＝９
      ****                 IF      SPA-RRK-RENNUM (IDX)    >=  3
      *                        MOVE    1           TO  SPA-NAI-KEIFLG2
      **                   END-IF
                           MOVE    1               TO  FLG-OK
                       END-IF
      *
      *                PERFORM VARYING     IDT2    FROM    1   BY  1
      *                        UNTIL      (IDT2    >   10      )  OR
      *                                   (TBL-HKNCOMBI(IDT IDT2)
      *                                                =   SPACE )
      *                    IF      SPA-RRK-HKNCOMBI (IDX)
      *                                            =   TBL-HKNCOMBI
      *                                                    (IDT IDT2)
      *                            MOVE    1               TO  FLG-OK
      *                    END-IF
      *****************END-PERFORM
      ****             END-IF
                   END-PERFORM
               END-PERFORM
           END-IF
      *
           .
       410162-GRP-DENPNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    受診日変更処理
      *****************************************************************
       410161-HENSRYYMD-UPDSYORI-SEC      SECTION.
      *
      *    現在表示されている内容を更新する
      *    診療会計マスター更新
           IF      SPA-UPDFLG          =   "1"
               PERFORM 4901-SRYACCT-UPD-SEC
               IF     (SPA-ERRCD      NOT  =   SPACE ) OR
                      (FLG-END             =   1     )
                   MOVE    3               TO  SPA-J025-SYORI
               ELSE
                   PERFORM 410161-HENSRYYMD-SYORI-SEC
               END-IF
           ELSE
               PERFORM 410161-HENSRYYMD-SYORI-SEC
           END-IF
           .
       410161-HENSRYYMD-UPDSYORI-EXT.
           EXIT.
      *****************************************************************
      *    受診日変更回数チェック処理
      *****************************************************************
       4101610-HENSRYYMD-CHK-SEC      SECTION.
      *
      *    受診履歴変更
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM          TO  JYURRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           MOVE    SPA-GMN-NYUGAIKBN   TO  JYURRK-NYUGAIKBN
           MOVE    SPA-RRK-SRYKA  (SPA-NAI-RRKSELNUM)
                                       TO  JYURRK-SRYKA
           MOVE    SPA-RRK-RRKYMD (SPA-NAI-RRKSELNUM)
                                       TO  JYURRK-SRYYMD
           MOVE    SPA-RRK-RENNUM (SPA-NAI-RRKSELNUM)
                                       TO  JYURRK-RENNUM
           MOVE    SPA-RRK-GRP-DENPNUM(SPA-NAI-RRKSELNUM)
                                       TO  JYURRK-GRP-DENPNUM
      *    入院の場合、保険毎に変更する
           IF      SPA-NYUGAIKBN       =   "1"
               MOVE    SPA-RRK-HKNCOMBI(SPA-NAI-RRKSELNUM)
                                           TO  JYURRK-HKNCOMBINUM
               MOVE    "key19"             TO  WRK-ORC-DBPATH
           ELSE
               IF      SPA-RRK-GRP-DENPNUM(SPA-NAI-RRKSELNUM)
                                           =   ZERO
                   MOVE    "key5"              TO  WRK-ORC-DBPATH
               ELSE
                   MOVE    "key26"             TO  WRK-ORC-DBPATH
               END-IF
           END-IF
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
      *    変更前日
           MOVE    SPA-RRK-RRKYMD (SPA-NAI-RRKSELNUM)(7:2)
                                       TO  IDX-D1
      *    変更後日
           MOVE    SPA-NAI-HENSRYYMD(7:2)
                                       TO  IDX-D2
      *
           PERFORM
               UNTIL   (FLG-JYURRK         =   1     )  OR
                       (JYURRK-HKNCOMBINUM =   "9999")
               PERFORM VARYING     IDX-Z   FROM    1  BY  1
                       UNTIL       IDX-Z   >   15
                   IF      JYURRK-ZAINUM (IDX-Z)  NOT =   ZERO
                       PERFORM 41016101-ACCT-CHKSYORI-SEC
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    SPACE           TO  SPA-JIDCD
           END-IF
           .
       4101610-HENSRYYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    連番判定処理
      *****************************************************************
       41016101-RENNUM-SET-SEC      SECTION.
      *
           MOVE    ZERO                TO  WRK-RRK-RENNUM
           MOVE    ZERO                TO  WRK-RRK-DOUJI-RENNUM 
           MOVE    JYURRK-SRYKA        TO  WRK-SRYKA
           MOVE    ZERO                TO  FLG-RRKNUM-ARI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-RRK-MAX )
               IF     (SPA-RRK-RRKYMD (IDX)
                                           =   SPA-NAI-HENSRYYMD)
               AND    (SPA-RRK-SRYKA (IDX)
                                           =   WRK-SRYKA   )
                   IF     (WRK-RRK-RENNUM  =   ZERO   ) OR
                          (WRK-RRK-RENNUM  <   SPA-RRK-RENNUM (IDX))
                       MOVE    SPA-RRK-RENNUM (IDX)
                                                TO  WRK-RRK-RENNUM
                   END-IF
      *            入院は同日連番
                   IF     (WRK-RRK-DOUJI-RENNUM  =   ZERO ) OR
                          (WRK-RRK-DOUJI-RENNUM
                                          <   SPA-RRK-DOUJI-RENNUM
                                                             (IDX))
                       MOVE    SPA-RRK-DOUJI-RENNUM (IDX)
                                          TO  WRK-RRK-DOUJI-RENNUM
                   END-IF
                   MOVE    1              TO  FLG-RRKNUM-ARI
                END-IF
           END-PERFORM
           IF      SPA-NYUGAIKBN       =   "1"
               MOVE    1               TO  WRK-RRK-RENNUM
               IF      FLG-RRKNUM-ARI  =   1
                   ADD     1               TO  WRK-RRK-DOUJI-RENNUM
               ELSE
                   MOVE    ZERO            TO  WRK-RRK-DOUJI-RENNUM
               END-IF
           ELSE
               ADD     1               TO  WRK-RRK-RENNUM
               MOVE    ZERO            TO  WRK-RRK-DOUJI-RENNUM
           END-IF
           .
       41016101-RENNUM-SET-EXT.
           EXIT.
      *****************************************************************
      *    受診日変更処理
      *****************************************************************
       41016101-ACCT-CHKSYORI-SEC      SECTION.
      *
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    JYURRK-NYUGAIKBN    TO  ACCT-NYUGAIKBN
           MOVE    JYURRK-PTID         TO  ACCT-PTID
           MOVE    JYURRK-SRYKA        TO  ACCT-SRYKA
           MOVE    JYURRK-SRYYMD(1:6)  TO  ACCT-SRYYM
           MOVE    JYURRK-ZAINUM(IDX-Z)
                                       TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SRYACCT          =   ZERO
      *        自費の時対象外
               IF      ACCT-SRYKBN          =   "95"  OR  "96"
      *                包括診療対象外
                  OR  (ACCT-JIHOKBN        =   "1"   )
                   CONTINUE
               ELSE
                   MOVE    1                TO  IDX
                   MOVE    1                TO  IDY
      *            診療日の追加
                   MOVE    JYURRK-RENNUM   TO  IDX-D3
                   COMPUTE IDX-D3          =   IDX-D3    +   1
      *****        IF      IDX-D3          >   4
      *                MOVE    4               TO  IDX-D3
      *******      END-IF
                   MOVE    ACCT-DAY (IDX-D3 IDX-D1)
                                           TO  WRK-DAY-S
                   MOVE    WRK-DAY-S       TO  ACCT-DAY (2 IDX-D2)
                   MOVE    WRK-DAY-S       TO  ACCT-DAY (1 IDX-D2)
      *            受診日・回数変更時、回数チェック
                   PERFORM 410161011-SANTEI-CHK-SEC
               END-IF
           END-IF
      *
           .
       41016101-ACCT-CHKSYORI-EXT.
           EXIT.
      *****************************************************************
      *    回数チェック処理
      *****************************************************************
       410161011-SANTEI-CHK-SEC      SECTION.
      *    診療行為マスター読み込み
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *    点数検索用日付編集
           MOVE    ACCT-SRYYM          TO  WRK-SRYYMD(1:6)
           MOVE    ZERO                TO  WRK-SRYDD
           MOVE    ZERO                TO  WRK-CNT-TUKI
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               IF     (ACCT-DAY (1 IDX-D)  NOT =   ZERO ) AND
                      (WRK-SRYDD           >   ZERO    )
                   MOVE    IDX-D           TO  WRK-SRYDD
               END-IF
               IF      ACCT-DAY (1 IDX-D)  NOT =   ZERO
                   ADD     ACCT-DAY (1 IDX-D)  TO  WRK-CNT-TUKI
               END-IF
           END-PERFORM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-RAED-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
           PERFORM UNTIL       FLG-SRYACT      =   1
      *        剤明細編集
               PERFORM VARYING     IDZ     FROM    1   BY  1
                       UNTIL      (IDZ         >   5        )  OR
                                  (SRY-SRYCD (IDZ) =   SPACE)
      *            手技料の時、算定歴を判定する
                   IF     (SRY-SRYCD (IDZ)(1:1)    =   "1" )
      *                システム予約コード
                       OR (SRY-SRYCD (IDZ)(1:3)    =   "099" )
      *
                       MOVE    SRY-SRYCD (IDZ)     TO  TNS-SRYCD
                       MOVE    SRY-SRYCD (IDZ)     TO  WRK-SRYCD
      *                算定用の診療コードへ変更
                       PERFORM 910-TENSU-READ-SEC
                       PERFORM 45020-SANTEI-SRYCD-SEC
                       IF      SRY-SRYCD (IDZ)
                                               NOT =   WRK-SRYCD
                           MOVE    WRK-SRYCD       TO  TNS-SRYCD
                           PERFORM 910-TENSU-READ-SEC
                       END-IF
                       IF     (TNS-SANTEIRRKKBN    NOT =   ZERO ) AND
                              (TNS-JGNCNTERR       NOT =   9    )
                           IF    ((TNS-JGNCNT      >   ZERO     )  AND
                                (TNS-JGNCNT        <   WRK-CNT-TUKI))
                             OR ((TNS-JGNCNTETC    >   ZERO     )  AND
                                 (TNS-JGNCNTETC    <   WRK-CNT-TUKI))
      *                    内分泌負荷試験判定
                               IF      TNS-HOUKNSKBN       =   08
                                   MOVE    "1008"          TO  SPA-ERRCD
                               ELSE
                                   MOVE    "0119"          TO  SPA-JIDCD
                               END-IF
                           END-IF
      *                    週上限
                           IF      (SPA-ERRCD          =   SPACE )
                              AND  (SPA-JIDCD          =   SPACE )
                              AND  (TNS-JGNCNT1W       >   ZERO  )
                               PERFORM 410161011-WEEK-CHEK-SEC
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-RAED-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       410161011-SANTEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    受診日変更週上限チェック処理
      *****************************************************************
       410161011-WEEK-CHEK-SEC      SECTION.
      *
           MOVE    ACCT-SRYYM          TO  WRK-CHK-SRYYMD(1:6)
           MOVE    ZERO                TO  WRK-CHK-SRYDD
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL      (IDX-D   >   31  )
                        OR    (SPA-ERRCD   NOT =   SPACE )
                        OR    (SPA-JIDCD   NOT =   SPACE )
               IF     (ACCT-DAY (1 IDX-D)  NOT =   ZERO )
                   MOVE    IDX-D               TO  WRK-CHK-SRYDD
                   PERFORM 47021-WEEKCNT-CHEK-SEC
                   MOVE    ZERO                TO  WRK-WEEKCNT
                   PERFORM VARYING     IDD     FROM    IDX-ST  BY  1
                           UNTIL       IDD     >   IDX-ED
                       ADD     ACCT-DAY (1 IDD)    TO  WRK-WEEKCNT
                   END-PERFORM
                   IF     (WRK-WEEKCNT         >   TNS-JGNCNT1W )
                       MOVE    "0131"          TO  SPA-JIDCD
                   END-IF
               END-IF
           END-PERFORM
           .
       410161011-WEEK-CHEK-EXT.
           EXIT.
      *****************************************************************
      *    受診日変更処理
      *****************************************************************
       410161-HENSRYYMD-SYORI-SEC      SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           INITIALIZE                  TBL-GRP-AREA
      *    受診履歴変更
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM          TO  JYURRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           MOVE    SPA-GMN-NYUGAIKBN   TO  JYURRK-NYUGAIKBN
           MOVE    SPA-RRK-SRYKA  (SPA-NAI-RRKSELNUM)
                                       TO  JYURRK-SRYKA
           MOVE    SPA-RRK-RRKYMD (SPA-NAI-RRKSELNUM)
                                       TO  JYURRK-SRYYMD
           MOVE    SPA-RRK-RENNUM (SPA-NAI-RRKSELNUM)
                                       TO  JYURRK-RENNUM
           MOVE    SPA-RRK-GRP-DENPNUM(SPA-NAI-RRKSELNUM)
                                       TO  JYURRK-GRP-DENPNUM
      *    入院の場合、保険毎に変更する
           IF      SPA-NYUGAIKBN       =   "1"
               MOVE    SPA-RRK-HKNCOMBI(SPA-NAI-RRKSELNUM)
                                           TO  JYURRK-HKNCOMBINUM
               MOVE    "key19"             TO  WRK-ORC-DBPATH
           ELSE
               IF      SPA-RRK-GRP-DENPNUM(SPA-NAI-RRKSELNUM)
                                           =   ZERO
                   MOVE    "key5"          TO  WRK-ORC-DBPATH
               ELSE
                   MOVE    "key26"         TO  WRK-ORC-DBPATH
               END-IF
           END-IF
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
      *    変更前日
           MOVE    SPA-RRK-RRKYMD (SPA-NAI-RRKSELNUM)(7:2)
                                       TO  IDX-D1
      *    変更後日
           MOVE    SPA-NAI-HENSRYYMD(7:2)
                                       TO  IDX-D2
      *    変更前と変更後が同日の時、変更なし
           IF      (IDX-D1             =   IDX-D2 )
               MOVE    1                   TO  FLG-JYURRK
               MOVE    "0075"              TO  SPA-ERRCD
           END-IF
           PERFORM
               UNTIL   (FLG-JYURRK         =   1  )
                    OR (SPA-ERRCD      NOT =   SPACE )
      *        連番判定
               IF      JYURRK-EDANUM       =   1
                   PERFORM 41016101-RENNUM-SET-SEC
               END-IF
      *        対象の診療会計を更新する
      *        診療会計更新
               MOVE    1               TO  FLG-SYORI
               PERFORM VARYING     IDX-Z   FROM    1  BY  1
                       UNTIL       IDX-Z   >   15
                   IF      JYURRK-ZAINUM (IDX-Z)  NOT =   ZERO
                       PERFORM 4101612-ACCT-UPDSYORI-SEC
                   END-IF
               END-PERFORM
      *        伝票番号保存
               IF     (JYURRK-HKNCOMBINUM  =   "9999" )   OR
                      (JYURRK-NYUGAIKBN    =   "1"    )
                   CONTINUE
               ELSE
                   PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   10
                       IF      TBL-DENPNUM (IDX)   =   ZERO
                           MOVE    JYURRK-DENPNUM  TO  TBL-DENPNUM (IDX)
                       END-IF
                       IF      JYURRK-DENPNUM  =   TBL-DENPNUM (IDX)
                           MOVE    10              TO  IDX
                       END-IF
                   END-PERFORM
               END-IF
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   10
                   IF      TBL-SRYKA (IDX)     =   SPACE
                       MOVE    JYURRK-SRYKA    TO  TBL-SRYKA (IDX)
                   END-IF
                   IF      JYURRK-SRYKA        =   TBL-SRYKA (IDX)
                       MOVE    10              TO  IDX
                   END-IF
               END-PERFORM
      *        受診履歴変更
               MOVE    JYURRK-KEY          TO  JYURRK-UPKEY
      *
               MOVE    SPA-NAI-HENSRYYMD   TO  JYURRK-SRYYMD
      *****    MOVE    1                   TO  JYURRK-RENNUM
               MOVE    WRK-RRK-RENNUM      TO  JYURRK-RENNUM
      *        同時連番
               IF      JYURRK-NYUGAIKBN    =   "1"
                   MOVE    WRK-RRK-DOUJI-RENNUM
                                           TO  JYURRK-DOUJI-RENNUM
               END-IF
      *
               MOVE    SMCNDATE-YMD        TO  JYURRK-UPYMD
               MOVE    SMCNDATE-HMS        TO  JYURRK-UPHMS
               MOVE    SPA-OPID            TO  JYURRK-OPID
      *
               MOVE    JYURRK-REC          TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "J02 JYURRK UPD ERR:" MCP-RC
                           ",KEY:" JYURRK-KEY
                           ",UPDKEY:" JYURRK-UPKEY
                   MOVE    "2001"              TO  SPA-ERRCD
               END-IF
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    外来の時、収納の更新
           IF     (SPA-NYUGAIKBN       =   "2"   )  AND
                  (SPA-RRK-HKNCOMBI(SPA-NAI-RRKSELNUM)
                                   NOT =   "9999" )
             AND  (SPA-ERRCD           =   SPACE  )
               PERFORM 4101613-SYUNOU-UPD-SEC
           END-IF
      *
      *    診療科履歴変更
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   10  )   OR
                              (TBL-SRYKA (IDX)     =   SPACE)  OR
                              (SPA-ERRCD       NOT =   SPACE)
               INITIALIZE                  ORCSKARRKAREA
               MOVE    SPA-GMN-PTID        TO  SPA-PTID
               MOVE    SPA-NAI-SRYYM       TO  SKARRK-SRYYM
               MOVE    TBL-SRYKA (IDX)     TO  SKARRK-SRYKA
               IF     (SPA-NYUGAIKBN       =   "1"  )  AND
                      (TBL-SRYKA (IDX)     =   SPA-NAI-NYUINKA) AND
                      (SPA-NAI-TAIINYMD    NOT =   "99999999"
                                               AND  SPACE    )
                   MOVE    SPA-NAI-TAIINYMD    TO  SKARRK-LASTYMD
               END-IF
               CALL    "ORCSKARRK"         USING
                                           ORCSKARRKAREA
                                           SPA-AREA
               IF      SKARRK-RC       NOT =   ZERO
                   MOVE    "0010"              TO  SPA-ERRCD
               END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4101611-HENSRYYMD-UPDSYORI2-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *
      *    保存用パラメタ作成
           PERFORM 4201-SRYACCT-PARA-SEC
           MOVE    1                   TO  SPA-GMN-PAGE
           MOVE    1                   TO  SPA-STRNUM
      *
      *    診療会計マスターより編集をする
           PERFORM 410181-SRYACCT-HEN-SEC
      *    受診履歴の連番を変更した時、メッセージを表示
           IF     (FLG-JYURRK-UPD      =   1   )  AND
                  (SPA-NYUGAIKBN       =   "2" )
               MOVE    "K001"              TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-RRK-MAX         =   ZERO
              AND  SPA-GMN-MAX         =   ZERO
               IF      SPA-SRYKA-MAX       =   1
                   MOVE    07              TO  SPA-GMN-CUR
               ELSE
                   MOVE    06              TO  SPA-GMN-CUR
               END-IF
           ELSE
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
      *
           END-IF
           .
       410161-HENSRYYMD-UPDSYORI-EXT.
           EXIT.
      *****************************************************************
      *    受診日変更処理処理
      *****************************************************************
       4101611-HENSRYYMD-UPDSYORI2-SEC             SECTION.
      *
      *    受診連番の変更日再編集
           MOVE    ZERO                TO  FLG-JYURRK-UPD
           PERFORM 4101613-JYURRK-SEISET-SEC
      *!
      *    主科設定
           PERFORM 410029-ORCSSYUKAMAIN-SEC
      *
           MOVE    ZERO                TO  SPA-NAI-RRKSELNUM
           MOVE    SPACE               TO  SPA-NAI-HENSRYYMD
           MOVE    SPACE               TO  SPA-GMN-RRKSELNUM
           MOVE    SPACE               TO  SPA-GMN-HENSRYYMD
      *
      *    受診履歴編集
           PERFORM 3103-JYURRK-HEN-SEC
           .
       4101611-HENSRYYMD-UPDSYORI2-EXT.
           EXIT.
      *****************************************************************
      *    主科設定処理
      *****************************************************************
       410029-ORCSSYUKAMAIN-SEC             SECTION.
      *
           INITIALIZE                      ORCSSYUACCAREA
           MOVE    "03"                TO  LNK-SYUACC-I-KBN
      *    診療年月
           MOVE    SPA-NAI-SRYYM       TO  LNK-SYUACC-I-SRYYM
           CALL    "ORCSSYUKAMAIN"     USING
                                       SPA-AREA
                                       ORCSSYUACCAREA
           .
      *
       410029-ORCSSYUKAMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診日変更診療会計変更処理
      *****************************************************************
       4101612-ACCT-UPDSYORI-SEC      SECTION.
      *
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM          TO  ACCT-HOSPNUM
           MOVE    JYURRK-NYUGAIKBN    TO  ACCT-NYUGAIKBN
           MOVE    JYURRK-PTID         TO  ACCT-PTID
           MOVE    JYURRK-SRYKA        TO  ACCT-SRYKA
           MOVE    JYURRK-SRYYMD(1:6)  TO  ACCT-SRYYM
           MOVE    JYURRK-ZAINUM(IDX-Z)
                                       TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SRYACCT         =   ZERO
               IF      JYURRK-HKNCOMBINUM   =   ACCT-HKNCOMBI
                   MOVE    1                TO  IDX
                   MOVE    1                TO  IDY
                   MOVE    ACCT-DAY-AREA    TO  SPA-NAI-DAY-AREA (IDX)
      *
                   EVALUATE    FLG-SYORI
                       WHEN    1
      *                    受診日の変更
                           PERFORM 41016111-YMDHENKOU-SEC
                        WHEN    2
      *                     連番の変更
                           PERFORM 41016111-RENHENKOU-SEC
                        WHEN    3
      *                    診療日の追加
                           PERFORM 41016111-YMDINS-SEC
                   END-EVALUATE
      *            受診日・回数変更時、算定履歴を変更する
                   IF     (SPA-NAI-DAY-AREA (IDX)  NOT =  ACCT-DAY-AREA)
                   AND    (FLG-SYORI           =   1   OR
                                                   3  )
                       PERFORM 4902-SANTEI-KOUSIN-SEC
                   END-IF
      *
                   MOVE    SPA-NAI-DAY-AREA (IDX)  TO  ACCT-DAY-AREA
      *
                   MOVE    SMCNDATE-YMD        TO  ACCT-UPYMD
                   MOVE    SMCNDATE-HMS        TO  ACCT-UPHMS
                   MOVE    SPA-OPID            TO  ACCT-OPID
      *
                   MOVE    SRYACCT-REC         TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                       DISPLAY "J02 SRYACCT UPD ERR:" MCP-RC
                               ",KEY:" ACCT-KEY
                       MOVE    "2000"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
           .
       4101612-ACCT-UPDSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納の更新編集処理
      *****************************************************************
       4101613-SYUNOU-UPD-SEC      SECTION.
      *
      *    伝票番号ゼロをエラーとする
           IF      SPA-RRK-DENPNUM(SPA-NAI-RRKSELNUM)
                                       =   ZERO
               MOVE    "1006"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   10  )   OR
                              (TBL-DENPNUM (IDX)   =   ZERO )  OR
                              (SPA-ERRCD       NOT =   SPACE)
               PERFORM 41016131-SYUNOU-UPDSYORI-SEC
           END-PERFORM
           .
       4101613-SYUNOU-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納の更新編集処理
      *****************************************************************
       41016131-SYUNOU-UPDSYORI-SEC      SECTION.
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
           MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    TBL-DENPNUM(IDX)    TO  SYU-DENPNUM
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
      *
      *    診療日が変更された時、収納も変更する
           IF     (SYU-SRYYMD      NOT =   SPA-NAI-HENSRYYMD) AND
                  (FLG-SYUNOU          =   ZERO   )
               MOVE    SPA-NAI-HENSRYYMD   TO  SYU-SRYYMD
               MOVE    SPA-NAI-HENSRYYMD   TO  SYU-SKYSTYMD
      *
               MOVE    SMCNDATE-YMD        TO  SYU-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYU-UPHMS
               MOVE    SPA-OPID            TO  SYU-OPID
      *
               MOVE    SYUNOU-REC          TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *
      *        収納合計
               IF     (SYU-GRP-DENPNUM     >   ZERO )  AND
                      (SYU-FUKU-DENPNUM    >   ZERO )  AND
                      (SYU-FUKU-KBN        =   "1"  )
                   PERFORM 410161312-SYUTOTAL-UPD-SEC
               END-IF
           END-IF
      *
           .
       41016131-SYUNOU-UPDSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納合計更新処理
      *****************************************************************
       410161312-SYUTOTAL-UPD-SEC      SECTION.
      *
           INITIALIZE                  SYUTTL-REC
           MOVE    SYU-HOSPNUM          TO  SYUTTL-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO  SYUTTL-NYUGAIKBN
           MOVE    SYU-PTID            TO  SYUTTL-PTID
           MOVE    SYU-FUKU-DENPNUM    TO  SYUTTL-DENPNUM
      *
           MOVE    SYUTTL-REC          TO  MCPDATA-REC
           MOVE    "tbl_syutotal"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syutotal"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 981-SYUTTL-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYUTTL
               INITIALIZE              SYUTTL-REC
           END-IF
           MOVE    "tbl_syutotal"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SYUTTL          =   ZERO
               MOVE    SYU-SRYYMD          TO  SYUTTL-SRYYMD
      *
               MOVE    SMCNDATE-YMD        TO  SYUTTL-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYUTTL-UPHMS
               MOVE    SPA-OPID            TO  SYUTTL-OPID
      *
               MOVE    SYUTTL-REC          TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_syutotal"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "J02 SYUTTL UPD ERR:" SYUTTL-KEY
                       ",MCP-RC:"  MCP-RC
                   MOVE    "2003"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       410161312-SYUTOTAL-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診連番の変更日再編集処理
      *****************************************************************
       4101613-JYURRK-SEISET-SEC      SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   10  )   OR
                              (TBL-SRYKA (IDX)     =   SPACE)  OR
                              (SPA-ERRCD       NOT =   SPACE)
               PERFORM 41016131-JYURRK-SEISETUPD-SEC
           END-PERFORM
           .
       4101613-JYURRK-SEISET-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診連番の変更日再編集処理
      *****************************************************************
       41016131-JYURRK-SEISETUPD-SEC   SECTION.
      *
      *    受診履歴変更
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    TBL-SRYKA (IDX)
                                       TO  JYURRK-SRYKA
           MOVE    SPA-RRK-RRKYMD (SPA-NAI-RRKSELNUM)
                                       TO  JYURRK-SRYYMD
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *    変更前日
           MOVE    SPA-RRK-RRKYMD (SPA-NAI-RRKSELNUM)(7:2)
                                       TO  IDX-D1
      *
           MOVE    ZERO                TO  WRK-RENNUM
           MOVE    ZERO                TO  WRK-MAE-RENNUM
           MOVE    ZERO                TO  WRK-DOUJI-RENNUM
           PERFORM
               UNTIL    FLG-JYURRK         =   1
      *        受診履歴変更
               MOVE    JYURRK-KEY          TO  JYURRK-UPKEY
      *
               IF      WRK-MAE-RENNUM      =   JYURRK-RENNUM
                   MOVE    WRK-RENNUM          TO  JYURRK-RENNUM
                   IF      SPA-NYUGAIKBN       =   "1"
      *                 同時連番
                       IF      JYURRK-HKNCOMBINUM
                                           NOT =   WRK-MAE-HKNCOMBINUM
                           ADD     1               TO  WRK-DOUJI-RENNUM
                       END-IF
                       MOVE    WRK-DOUJI-RENNUM
                                               TO  JYURRK-DOUJI-RENNUM
                       MOVE    JYURRK-HKNCOMBINUM
                                               TO  WRK-MAE-HKNCOMBINUM
                   END-IF
               ELSE
                   MOVE    JYURRK-RENNUM       TO  WRK-MAE-RENNUM
                   IF      JYURRK-RENNUM       NOT =   WRK-RENNUM
                       ADD     1               TO  WRK-RENNUM
                   END-IF
                   IF      SPA-NYUGAIKBN       =   "1"
                       MOVE    ZERO            TO  WRK-DOUJI-RENNUM
                       MOVE    WRK-DOUJI-RENNUM
                                               TO  JYURRK-DOUJI-RENNUM
                       MOVE    JYURRK-HKNCOMBINUM
                                               TO  WRK-MAE-HKNCOMBINUM
                   END-IF
                   MOVE    WRK-RENNUM          TO  JYURRK-RENNUM
               END-IF
               IF      JYURRK-KEY      NOT =   JYURRK-UPKEY
      *            対象の診療会計を更新する
      *            連番の変更
                   MOVE    2               TO  FLG-SYORI
                   PERFORM VARYING     IDX-Z   FROM    1  BY  1
                           UNTIL       IDX-Z   >   15
                       IF      JYURRK-ZAINUM (IDX-Z)  NOT =   ZERO
                           PERFORM 4101612-ACCT-UPDSYORI-SEC
                       END-IF
                   END-PERFORM
      *
                   MOVE    1                   TO  FLG-JYURRK-UPD
      *
                   MOVE    SMCNDATE-YMD        TO  JYURRK-UPYMD
                   MOVE    SMCNDATE-HMS        TO  JYURRK-UPHMS
                   MOVE    SPA-OPID            TO  JYURRK-OPID
      *
                   MOVE    JYURRK-REC          TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                   MOVE    "upd1"              TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                        DISPLAY "J02 JYURRK UPD ERR:" MCP-RC
                               ",KEY:" JYURRK-KEY
                        MOVE    "2001"              TO  SPA-ERRCD
                   END-IF
               END-IF
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       41016131-JYURRK-SEISETUPD-EXT.
           EXIT.
      *****************************************************************
      *    受診日追加処理
      *****************************************************************
       410162-HENSRYYMD-INSSYORI-SEC      SECTION.
      *
      *    現在表示されている内容を更新する
      *    診療会計マスター更新
           IF      SPA-UPDFLG          =   "1"
               PERFORM 4901-SRYACCT-UPD-SEC
               IF     (SPA-ERRCD      NOT  =   SPACE ) OR
                      (FLG-END             =   1     )
                   MOVE    11              TO  SPA-J025-SYORI
               ELSE
                   PERFORM 4101621-HEN-INSSYORI-SEC
               END-IF
           ELSE
               PERFORM 4101621-HEN-INSSYORI-SEC
           END-IF
           .
       410162-HENSRYYMD-INSSYORI-EXT.
           EXIT.
      *****************************************************************
      *    受診日追加処理
      *****************************************************************
       4101621-HEN-INSSYORI-SEC      SECTION.
      *
      *    上限回数チェック
           PERFORM 4101610-HENSRYYMD-CHK-SEC
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-JIDCD           =   SPACE )
      *        受診日追加処理
               PERFORM 41016211-HENYMD-INSSYORI-SEC
           END-IF
           .
       4101621-HEN-INSSYORI-EXT.
           EXIT.
      *****************************************************************
      *    受診日追加処理
      *****************************************************************
       41016211-HENYMD-INSSYORI-SEC      SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           INITIALIZE                  TBL-GRP-AREA
      *    受診履歴変更
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM          TO  JYURRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           MOVE    SPA-GMN-NYUGAIKBN   TO  JYURRK-NYUGAIKBN
           MOVE    SPA-RRK-SRYKA  (SPA-NAI-RRKSELNUM)
                                       TO  JYURRK-SRYKA
           MOVE    SPA-RRK-RRKYMD (SPA-NAI-RRKSELNUM)
                                       TO  JYURRK-SRYYMD
           MOVE    SPA-RRK-RENNUM (SPA-NAI-RRKSELNUM)
                                       TO  JYURRK-RENNUM
           MOVE    SPA-RRK-GRP-DENPNUM(SPA-NAI-RRKSELNUM)
                                       TO  JYURRK-GRP-DENPNUM
      *    入院の場合、保険毎に変更する
           IF      SPA-NYUGAIKBN       =   "1"
               MOVE    SPA-RRK-HKNCOMBI(SPA-NAI-RRKSELNUM)
                                           TO  JYURRK-HKNCOMBINUM
               MOVE    "key19"             TO  WRK-ORC-DBPATH
           ELSE
               IF      SPA-RRK-GRP-DENPNUM(SPA-NAI-RRKSELNUM)
                                           =   ZERO
                   MOVE    "key5"          TO  WRK-ORC-DBPATH
               ELSE
                   MOVE    "key26"         TO  WRK-ORC-DBPATH
               END-IF
           END-IF
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
      *    変更前日
           MOVE    SPA-RRK-RRKYMD (SPA-NAI-RRKSELNUM)(7:2)
                                       TO  IDX-D1
      *    変更後日
           MOVE    SPA-NAI-HENSRYYMD(7:2)
                                       TO  IDX-D2
           MOVE    ZERO                TO  WRK-DENPNUM
           MOVE    ZERO                TO  WRK-GRP-DENPNUM
           MOVE    ZERO                TO  WRK-KA-DENPNUM
           PERFORM
               UNTIL    FLG-JYURRK         =   1
      *        連番判定
               IF      JYURRK-EDANUM       =   1
                   PERFORM 41016101-RENNUM-SET-SEC
               END-IF
      *        対象の診療会計を更新する
               MOVE    JYURRK-REC          TO  HZN-JYURRK-REC
               IF      JYURRK-NYUGAIKBN    =   "2"
                   IF     (JYURRK-EDANUM       =   1   )  OR
                          (WRK-DENPNUM         =   ZERO )
      *               伝票番号取得と収納の作成
                      PERFORM 4101621-INS-INIT-SEC
                   END-IF
               END-IF
      *        変更後日
               MOVE    SPA-NAI-HENSRYYMD   TO  JYURRK-SRYYMD
               MOVE    WRK-DENPNUM         TO  JYURRK-DENPNUM
               IF      JYURRK-GRP-DENPNUM  NOT =   ZERO
                   MOVE    WRK-GRP-DENPNUM     TO  JYURRK-GRP-DENPNUM
               END-IF
      *
      *        診療会計更新
               MOVE    3                   TO  FLG-SYORI
               PERFORM VARYING     IDX-Z   FROM    1  BY  1
                       UNTIL       IDX-Z   >   15
                   IF      JYURRK-ZAINUM (IDX-Z)  NOT =   ZERO
                       PERFORM 4101612-ACCT-UPDSYORI-SEC
                   END-IF
               END-PERFORM
      *        伝票番号保存
               IF     (JYURRK-HKNCOMBINUM  =   "9999" )   OR
                      (JYURRK-NYUGAIKBN    =   "1"    )
                   CONTINUE
               ELSE
                   PERFORM VARYING     IDX     FROM    1   BY  1
                           UNTIL       IDX     >   10
                       IF      TBL-DENPNUM (IDX)   =   ZERO
                           MOVE    JYURRK-DENPNUM  TO  TBL-DENPNUM (IDX)
                       END-IF
                       IF      JYURRK-DENPNUM  =   TBL-DENPNUM (IDX)
                           MOVE    10              TO  IDX
                       END-IF
                   END-PERFORM
               END-IF
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   10
                   IF      TBL-SRYKA (IDX)     =   SPACE
                       MOVE    JYURRK-SRYKA    TO  TBL-SRYKA (IDX)
                   END-IF
                   IF      JYURRK-SRYKA        =   TBL-SRYKA (IDX)
                       MOVE    10              TO  IDX
                   END-IF
               END-PERFORM
      *        受診履歴追加
      *****    MOVE    1                   TO  JYURRK-RENNUM
               MOVE    WRK-RRK-RENNUM      TO  JYURRK-RENNUM
      *        同時連番
               IF      JYURRK-NYUGAIKBN    =   "1"
                   MOVE    WRK-RRK-DOUJI-RENNUM
                                           TO  JYURRK-DOUJI-RENNUM
               END-IF
      *
               MOVE    SMCNDATE-YMD        TO  JYURRK-CREYMD
               MOVE    SMCNDATE-YMD        TO  JYURRK-UPYMD
               MOVE    SMCNDATE-HMS        TO  JYURRK-UPHMS
               MOVE    SPA-OPID            TO  JYURRK-OPID
      *
               MOVE    JYURRK-REC          TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "J02 JYURRK INS ERR:" MCP-RC
                               ",KEY:" JYURRK-KEY
                   MOVE    "2001"              TO  SPA-ERRCD
               END-IF
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    診療科履歴変更
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   10  )   OR
                              (TBL-SRYKA (IDX)     =   SPACE)  OR
                              (SPA-ERRCD       NOT =   SPACE)
               INITIALIZE                  ORCSKARRKAREA
               MOVE    SPA-GMN-PTID        TO  SPA-PTID
               MOVE    SPA-NAI-SRYYM       TO  SKARRK-SRYYM
               MOVE    TBL-SRYKA (IDX)     TO  SKARRK-SRYKA
               IF     (SPA-NYUGAIKBN       =   "1"  )  AND
                      (TBL-SRYKA (IDX)     =   SPA-NAI-NYUINKA) AND
                      (SPA-NAI-TAIINYMD    NOT =   "99999999"
                                               AND  SPACE    )
                   MOVE    SPA-NAI-TAIINYMD    TO  SKARRK-LASTYMD
               END-IF
               CALL    "ORCSKARRK"         USING
                                           ORCSKARRKAREA
                                           SPA-AREA
               IF      SKARRK-RC       NOT =   ZERO
                   MOVE    "0010"              TO  SPA-ERRCD
               END-IF
           END-PERFORM
      *
           IF     (SPA-NYUGAIKBN       =   "2"   )  AND
                  (SPA-RRK-HKNCOMBI(SPA-NAI-RRKSELNUM)
                                   NOT =   "9999" )
      *        収納再計算
               PERFORM 49012-SYUNOU-SAIKEISAN-SEC
               IF     (SPA-ERRCD      NOT  =   SPACE ) OR
                      (FLG-END             =   1     )
                   MOVE    12              TO  SPA-J025-SYORI
               ELSE
                   PERFORM 41016219-INSSYORI-END-SEC
               END-IF
           ELSE
               PERFORM 41016219-INSSYORI-END-SEC
           END-IF
           MOVE    SPACE               TO  SPA-NAI-HENSRYYMD
           MOVE    SPACE               TO  SPA-GMN-HENSRYYMD
           MOVE    ZERO                TO  SPA-NAI-RRKSELNUM
           MOVE    SPACE               TO  SPA-GMN-RRKSELNUM
      *
           .
       41016211-HENYMD-INSSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    変更後処理
      *****************************************************************
       41016219-INSSYORI-END-SEC             SECTION.
      *
      *    受診履歴編集
           PERFORM 3103-JYURRK-HEN-SEC
      *
      *    保存用パラメタ作成
           PERFORM 4201-SRYACCT-PARA-SEC
           MOVE    1                   TO  SPA-GMN-PAGE
           MOVE    1                   TO  SPA-STRNUM
      *
      *    診療会計マスターより編集をする
           PERFORM 410181-SRYACCT-HEN-SEC
      *    受診履歴の連番を変更した時、メッセージを表示
           IF     (FLG-JYURRK-UPD      =   1   )  AND
                  (SPA-NYUGAIKBN       =   "2" )
               MOVE    "K001"              TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-RRK-MAX         =   ZERO
              AND  SPA-GMN-MAX         =   ZERO
               IF      SPA-SRYKA-MAX       =   1
                   MOVE    07              TO  SPA-GMN-CUR
               ELSE
                   MOVE    06              TO  SPA-GMN-CUR
               END-IF
           ELSE
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
      *
           .
       41016219-INSSYORI-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    伝票番号取得と収納の作成処理
      *****************************************************************
       4101621-INS-INIT-SEC             SECTION.
      *
      *    伝票番号取得処理
           PERFORM 49011212-DENPNUM-SET-SEC
           MOVE    JYURRK-DENPNUM      TO  WRK-DENPNUM
      *
           IF     (JYURRK-GRP-DENPNUM  NOT =   ZERO )  AND
                  (WRK-GRP-DENPNUM         =   ZERO )
               MOVE    JYURRK-DENPNUM          TO  WRK-GRP-DENPNUM
           END-IF
      *
      *    収納作成
           INITIALIZE                  SYUNOU-REC
           MOVE    JYURRK-HOSPNUM      TO  SYU-HOSPNUM
           MOVE    JYURRK-PTID         TO  SYU-PTID
           MOVE    JYURRK-NYUGAIKBN    TO  SYU-NYUGAIKBN
           MOVE    HZN-JYURRK-DENPNUM  TO  SYU-DENPNUM
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
      *
           MOVE    SYUNOU-REC          TO  WRK-SYUNOU-REC
           IF     (SYU-GRP-DENPNUM     >   ZERO        ) AND
                  (SYU-FUKU-DENPNUM    =   SYU-DENPNUM ) AND
                  (SYU-FUKU-KBN        =   "1"         )
               MOVE    WRK-DENPNUM         TO  WRK-KA-DENPNUM
           END-IF
           IF      FLG-SYUNOU          =   ZERO
               MOVE    WRK-DENPNUM         TO  SYU-DENPNUM
      *        まとめ入力分
               IF      SYU-GRP-RENNUM      >   ZERO
                   MOVE    WRK-GRP-DENPNUM     TO  SYU-GRP-DENPNUM
               ELSE
                   MOVE    ZERO                TO  SYU-GRP-DENPNUM
               END-IF
      *        複数科まとめ識別伝票番号
               IF     (SYU-GRP-DENPNUM     >   ZERO    ) AND
                      (SYU-FUKU-KBN    NOT =   SPACE   )
                   MOVE    WRK-KA-DENPNUM      TO  SYU-FUKU-DENPNUM
               END-IF
      *
      *        変更後日
               MOVE    SPA-NAI-HENSRYYMD   TO  SYU-SRYYMD
               MOVE    SPA-NAI-HENSRYYMD   TO  SYU-SKYSTYMD
               MOVE    ALL "9"             TO  SYU-SKYEDYMD
      *        伝票番号発行日
               MOVE    SPA-SYSYMD          TO  SYU-DENPPRTYMD
      *        請求書作成区分
               MOVE    "0"                 TO  SYU-SKYKUKBN
      *        会計変更区分
               MOVE    "2"                 TO  SYU-ACCT-UPDKBN
      *        継続区分
               MOVE    ZERO                TO  SYU-DOUJI-DENPNUM
               MOVE    SPACE               TO  SYU-CONTKBN
      *        伝票状態区分
               IF      SYU-DENPJTIKBN      =   "7"
                   MOVE    SPACE               TO  SYU-DENPJTIKBN
               END-IF
      *
               MOVE    SMCNDATE-YMD        TO  SYU-CREYMD
               MOVE    SMCNDATE-YMD        TO  SYU-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYU-UPHMS
               MOVE    SPA-OPID            TO  SYU-OPID
      *
               MOVE    SYUNOU-REC          TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "J02 SYUNOU INS ERR:" SYU-KEY
                                      ",MCP-RC:" MCP-RC 
                   MOVE    "2003"              TO  SPA-ERRCD
               END-IF
      *        収納合計レコード
               IF      (WRK-SYU-GRP-DENPNUM    >   ZERO) AND
                       (WRK-SYU-FUKU-DENPNUM   >   ZERO) AND
                       (WRK-SYU-FUKU-KBN       =   "1" )
                  PERFORM 41016211-SYUTTL-INS-SEC
               END-IF
      *
           END-IF
      *
           .
       4101621-INS-INIT-EXT.
           EXIT.
      *****************************************************************
      *    収納合計レコード作成処理
      *****************************************************************
       41016211-SYUTTL-INS-SEC             SECTION.
      *
           INITIALIZE                  SYUTTL-REC
           MOVE    SYU-HOSPNUM          TO  SYUTTL-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO  SYUTTL-NYUGAIKBN
           MOVE    SYU-PTID            TO  SYUTTL-PTID
           MOVE    WRK-SYU-FUKU-DENPNUM
                                       TO  SYUTTL-DENPNUM
      *
           MOVE    SYUTTL-REC          TO  MCPDATA-REC
           MOVE    "tbl_syutotal"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syutotal"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 981-SYUTTL-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYUTTL
               INITIALIZE              SYUTTL-REC
           END-IF
           MOVE    "tbl_syutotal"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SYUTTL          =   ZERO
               MOVE    SYU-FUKU-DENPNUM    TO  SYUTTL-DENPNUM
               MOVE    SYU-SRYYMD          TO  SYUTTL-SRYYMD
      *        伝票発行日
               MOVE    SYU-DENPPRTYMD      TO  SYUTTL-DENPPRTYMD
      *        新規作成
               MOVE    SMCNDATE-YMD        TO  SYUTTL-CREYMD
               MOVE    SMCNDATE-YMD        TO  SYUTTL-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYUTTL-UPHMS
               MOVE    SPA-OPID            TO  SYUTTL-OPID
      *
               MOVE    SYUTTL-REC          TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_syutotal"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "J02 SYUTTL INS ERR:" SYUTTL-KEY
                       ",MCP-RC:"  MCP-RC
                   MOVE    "2003"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       41016211-SYUTTL-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診日の変更処理
      *****************************************************************
       41016111-YMDHENKOU-SEC             SECTION.
      *
      *    日の変更（変更後は連番を１とする）
           MOVE    JYURRK-RENNUM       TO  IDX-D3
           COMPUTE IDX-D3              =   IDX-D3    +   1
           COMPUTE IDX-D4              =   WRK-RRK-RENNUM    +   1
      **** IF      IDX-D3              >   4
      *        MOVE    4               TO  IDX-D3
      *    END-IF
      *    IF      IDX-D4              >   4
      *        MOVE    4               TO  IDX-D4
      *****END-IF
      *
           MOVE    SPA-NAI-DAY (IDY IDX-D3 IDX-D1)
                                       TO  WRK-DAY-S
           MOVE    WRK-DAY-S           TO  SPA-NAI-DAY
                                                  (IDY IDX-D4 IDX-D2)
      *????MOVE    WRK-DAY-S           TO  SPA-NAI-DAY (IDY 1 IDX-D2)
           ADD     WRK-DAY-S           TO  SPA-NAI-DAY (IDY 1 IDX-D2)
      *
           COMPUTE SPA-NAI-DAY (IDY 1   IDX-D1)
                                       =   SPA-NAI-DAY
                                                  (IDY  1    IDX-D1)
                                       -   WRK-DAY-S
           MOVE    ZERO                TO  SPA-NAI-DAY
                                                   (IDY IDX-D3 IDX-D1)
           .
       41016111-YMDHENKOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    連番の変更処理
      *****************************************************************
       41016111-RENHENKOU-SEC             SECTION.
      *
      *    日の変更（変更後は連番を１とする）
      **** MOVE    JYURRK-RENNUM       TO  IDX-D3
           MOVE    JYURRK-UPRENNUM     TO  IDX-D3
           COMPUTE IDX-D3              =   IDX-D3    +   1
           COMPUTE IDX-D4              =   WRK-RENNUM    +   1
      **** IF      IDX-D3              >   4
      *        MOVE    4               TO  IDX-D3
      *    END-IF
      *    IF      IDX-D4              >   4
      *        MOVE    4               TO  IDX-D4
      **** END-IF
      *
           MOVE    SPA-NAI-DAY (IDY IDX-D3 IDX-D1)
                                       TO  WRK-DAY-S
           MOVE    WRK-DAY-S           TO  SPA-NAI-DAY
                                                (IDY IDX-D4 IDX-D1)
           MOVE    ZERO                TO  SPA-NAI-DAY
                                                (IDY IDX-D3 IDX-D1)
           .
       41016111-RENHENKOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診日の追加処理
      *****************************************************************
       41016111-YMDINS-SEC             SECTION.
      *
      *    日の変更（変更後は連番を１とする）
           MOVE    HZN-JYURRK-RENNUM   TO  IDX-D3
           COMPUTE IDX-D3              =   IDX-D3    +   1
           COMPUTE IDX-D4              =   WRK-RRK-RENNUM    +   1
      **** IF      IDX-D3              >   4
      *        MOVE    4               TO  IDX-D3
      *    END-IF
      *    IF      IDX-D4              >   4
      *        MOVE    4               TO  IDX-D4
      **** END-IF
      *
           MOVE    SPA-NAI-DAY (IDY IDX-D3 IDX-D1)
                                       TO  WRK-DAY-S
           ADD     WRK-DAY-S           TO  SPA-NAI-DAY
                                                (IDY IDX-D4 IDX-D2)
           ADD     WRK-DAY-S           TO  SPA-NAI-DAY (IDY 1 IDX-D2)
      *    剤回数
           ADD     WRK-DAY-S           TO  ACCT-ZAIKAISU
           .
       41016111-YMDINS-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ入力処理
      *****************************************************************
       41017-HKNCOMBI-CHK-SEC             SECTION.
      *
           MOVE    SPA-GMN-HHKNCOMBI-G TO  SPA-MAE-HHKNCOMBI-G
           MOVE    J02-HHKNCOMBI       TO  SPA-GMN-HHKNCOMBI-G
      *
           MOVE    ZERO                TO  FLG-OK
      *
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-HKN-MAX
              IF      SPA-GMN-THKNCOMBINUM(IDX)
                                           =   SPA-GMN-HHKNCOMBI
                   MOVE    SPA-GMN-THKNCOMBINUM-G (IDX)
                                               TO  SPA-GMN-HHKNCOMBI-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0032"              TO  SPA-ERRCD
               MOVE    10                  TO  SPA-GMN-CUR
               GO      TO      41017-HKNCOMBI-CHK-EXT
           END-IF
      *    変更の有無
           IF     (SPA-GMN-HHKNCOMBI   =   SPA-MAE-HHKNCOMBI)
               GO      TO      41017-HKNCOMBI-CHK-EXT
           END-IF
      *
           IF      SPA-UPDFLG          =   SPACE
               PERFORM 410171-NYUGAIKBN-CHG-SEC
               IF      SPA-GMN-MAX         =   ZERO
                   MOVE    10                  TO  SPA-GMN-CUR
               END-IF
           ELSE
               MOVE    "0111"              TO  SPA-JIDCD
               MOVE    4                   TO  SPA-J025-SYORI
           END-IF
      *
           .
       41017-HKNCOMBI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分処理
      *****************************************************************
       41017-NYUGAIKBN-CHK-SEC             SECTION.
      *
           MOVE    J02-NYUGAIKBN       TO  SPA-GMN-NYUGAIKBN-G
      *
           MOVE    ZERO                TO  FLG-OK
      *
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-NYUGAI-MAX
              IF      SPA-GMN-NYUGAIKBN-LST (IDX)(1:1)
                                           =   SPA-GMN-NYUGAIKBN
                   MOVE    SPA-GMN-NYUGAIKBN-LST (IDX)
                                               TO  SPA-GMN-NYUGAIKBN-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0030"              TO  SPA-ERRCD
               MOVE    08              TO  SPA-GMN-CUR
           END-IF
      *    変更の有無
           IF      SPA-NYUGAIKBN       =   SPA-GMN-NYUGAIKBN
               MOVE    1               TO  SPA-GMN-CUR
               GO      TO      41017-NYUGAIKBN-CHK-EXT
           END-IF
           IF      SPA-GMN-PTID        =   ZERO
               MOVE    00              TO  SPA-GMN-CUR
               GO      TO      41017-NYUGAIKBN-CHK-EXT
           END-IF
      *
           IF      SPA-UPDFLG          =   SPACE
               PERFORM 410171-NYUGAIKBN-CHG-SEC
           ELSE
               MOVE    "0112"              TO  SPA-JIDCD
               MOVE    4                   TO  SPA-J025-SYORI
           END-IF
      *
           .
       41017-NYUGAIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分変更処理
      *****************************************************************
       410171-NYUGAIKBN-CHG-SEC     SECTION.
      *
      *    現在表示されている内容を更新する
      *    診療会計マスター更新
           IF      SPA-UPDFLG          =   "1"
               IF      SPA-GMN-MAX         >   ZERO
                   PERFORM 4901-SRYACCT-UPD-SEC
               END-IF
               IF     (SPA-ERRCD       NOT =   SPACE )  OR
                      (FLG-END             =   1     )
                   CONTINUE
               ELSE
                   PERFORM 410171-NYUGAIKBN-CHG-SYORI-SEC
               END-IF
           ELSE
               PERFORM 410171-NYUGAIKBN-CHG-SYORI-SEC
           END-IF
      *
           .
       410171-NYUGAIKBN-CHG-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分変更処理
      *****************************************************************
       410171-NYUGAIKBN-CHG-SYORI-SEC     SECTION.
      *
           IF      SPA-GMN-NYUGAIKBN   =   SPA-NYUGAIKBN
               MOVE    ZERO                TO  FLG-NYUGAICHG
           ELSE
               MOVE    1                   TO  FLG-NYUGAICHG
           END-IF
           MOVE    SPA-GMN-NYUGAIKBN   TO  SPA-NYUGAIKBN
      *
      *****INITIALIZE                  SPA-J02-AREA
           IF      SPA-GMN-NYUGAIKBN   =   "2"
               INITIALIZE                  SPA-GMN-NYUIN-AREA
      *H30.7
               PERFORM 410101-NYUINRRK-CHK-SEC
           ELSE
      *       入院期間
               PERFORM 410101-NYUINRRK-CHK-SEC
               MOVE    "1"             TO  SPA-NYUGAIKBN
               IF     (SPA-NAI-SRYKA-FLG   =   0    )  OR
                      (SPA-J01-SRYKA       =   SPACE)
      *        標準は入院科で表示
                   IF     (SPA-NAI-NYUINKA NOT =   SPACE )
                       MOVE    SPA-NAI-NYUINKA     TO  SPA-J01-SRYKA
                       PERFORM 3101-SRYKA-HEN-SEC
                   END-IF
               END-IF
           END-IF
      *
      *    受診履歴より診療科決定
           IF      SPA-J01-SRYKA       =   SPACE
               PERFORM 310310-JYURRK-CHK-SEC
           END-IF
      *    受診履歴編集
           PERFORM 3103-JYURRK-HEN-SEC
      *    収納更新チェック
           PERFORM 3104-SYUNOU-CHECK-SEC
      *    保険一覧編集
           IF      FLG-NYUGAICHG       =   1
               PERFORM 3008-HKNCOMBI-HEN-SEC
           END-IF
      *
      *    入院料包括診療チェック処理
           PERFORM 30091-HKTCD-CHK-SEC
      *
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
      *    保存用パラメタ作成
           PERFORM 4201-SRYACCT-PARA-SEC
           MOVE    1                   TO  SPA-GMN-PAGE
           MOVE    1                   TO  SPA-STRNUM
      *
      *    診療会計マスターより編集をする
           PERFORM 410181-SRYACCT-HEN-SEC
      *
           IF      SPA-GMN-MAX         =   ZERO
               MOVE    8                   TO  SPA-GMN-CUR
           ELSE
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *    受診履歴が無い時、メッセージ表示
           IF      SPA-RRK-MAX         =   ZERO
              AND  SPA-GMN-MAX         =   ZERO
               IF      SPA-SRYKA-MAX       =   1
                   MOVE    07              TO  SPA-GMN-CUR
               ELSE
                   MOVE    09              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       410171-NYUGAIKBN-CHG-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療種別処理
      *****************************************************************
       41018-SRYKBN-CHK-SEC             SECTION.
      *
           MOVE    SPA-GMN-SRYKBN      TO  SPA-MAE-SRYKBN-G
           MOVE    J02-SRYKBN          TO  SPA-GMN-SRYKBN-G
      *
           MOVE    ZERO                TO  FLG-OK
      *
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-SRYKBN-MAX
              IF      SPA-GMN-SRYKBN-LST (IDX)(1:2)
                                           =   SPA-GMN-SRYKBN
                   MOVE    SPA-GMN-SRYKBN-LST (IDX)
                                               TO  SPA-GMN-SRYKBN-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0033"              TO  SPA-ERRCD
               MOVE    09                  TO  SPA-GMN-CUR
               GO      TO      41018-SRYKBN-CHK-EXT
           END-IF
      *    変更の有無
           IF     (SPA-GMN-SRYKBN      =   SPA-MAE-SRYKBN )  OR
                  (SPA-GMN-PTID        =   ZERO           )
               GO      TO      41018-SRYKBN-CHK-EXT
           END-IF
      *
           IF      SPA-UPDFLG          =   SPACE
               PERFORM 410171-NYUGAIKBN-CHG-SEC
           ELSE
               MOVE    "0110"              TO  SPA-JIDCD
               MOVE    4                   TO  SPA-J025-SYORI
           END-IF
      *
           .
       41018-SRYKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       41012-SEIWA-HEN-SEC             SECTION.
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
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
           .
       41012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア画面へ
      *****************************************************************
       420-CLEA-SEC        SECTION.
      *
           IF     (SPA-GMN-BANGO       =   ZERO  )  AND
                  (SPA-GMN-RRKSELNUM   =   SPACE )  AND
                  (SPA-GMN-HENSRYYMD   =   SPACE )
      *        排他制御解除処理 
               PERFORM 990-LOCK-OUT-SEC
      *        クリア
               MOVE    ZERO                TO  SPA-GMN-CUR
               INITIALIZE                  SPA-J02-AREA
               INITIALIZE                  SPA-SCR-REC
               INITIALIZE                  SPA-J02-SET
               INITIALIZE                  SPA-J02-SYOKI
               MOVE    SPA-J02-SRYYMDW(1:6)    TO  SPA-GMN-SRYYM
               MOVE    SPA-J02-SRYYMD (1:6)    TO  SPA-NAI-SRYYM
               MOVE    SPACE               TO  SPA-GMN-SRYKA-G
               MOVE    ZERO                TO  SPA-PTID
               MOVE    SPACE               TO  SPA-PTNUM
               PERFORM 310-SPA-SET-SEC
           ELSE
      *        変更項目クリア
               INITIALIZE                  SPA-GMN-INPUT
               INITIALIZE                  SPA-NAIGMN-AREA
               MOVE    ZERO                TO  SPA-J02-SYORI
      *
               MOVE    1                   TO  SPA-GMN-CUR
      *
           END-IF
      *
           .
       420-CLEA-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤削除
      *****************************************************************
       4200-ZAICLIEA-SEC        SECTION.
      *
           IF      SPA-GMN-PTNUM       =   SPACE
               CONTINUE
           ELSE
      *        剤削除
               IF      SPA-GMN-BANGO   NOT =   ZERO
                   MOVE    "0009"          TO  SPA-ERRCD
               END-IF
      *
               IF     (SPA-ERRCD           =   SPACE )  AND
                      (SPA-UPDFLG          =   "1"   )
                   MOVE    "0070"          TO  SPA-ERRCD
               END-IF
               IF      SPA-ERRCD           =   SPACE
                   MOVE    "0130"          TO  SPA-JIDCD
               END-IF
           END-IF
      *
           .
       4200-ZAICLIEA-EXT.
           EXIT.
      *
      *****************************************************************
      *    回数ゼロ剤削除処理
      *****************************************************************
       42001-ZAIDELETE-SYORI-SEC        SECTION.
      *
      *    診療会計マスターを編集
           INITIALIZE                      SRYACCT-REC 
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  ACCT-PTID
           MOVE    SPA-NAI-SRYYM       TO  ACCT-SRYYM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key39"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key39"             TO  MCP-PATHNAME
               PERFORM 900-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-ERRFLG2
           PERFORM
               UNTIL      (FLG-SRYACCT     =   1   )
                     OR   (SPA-ERRCD   NOT =   SPACE)
      *        受診履歴に存在しないこと
               MOVE    SPACE               TO  WRK-ERRFLG
               PERFORM 420012-JYURRKDEL-CHK-SEC
               IF     (SPA-ERRCD           =   SPACE )
                 AND  (WRK-ERRFLG          =   SPACE )
      *            診療行為削除
                   PERFORM 420011-SRYACT-DEL-SEC
                   MOVE    "1"                 TO  WRK-ERRFLG2
               END-IF
      *
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key39"             TO  MCP-PATHNAME
               PERFORM 900-SRYACCT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key39"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *
           IF     (SPA-ERRCD           =   SPACE )
              AND (WRK-ERRCD       NOT =   SPACE )
               MOVE    WRK-ERRCD           TO  SPA-ERRCD
           END-IF
      *
      *    入院の時、受診履歴の削除
           IF      SPA-GMN-NYUGAIKBN   =   "1"
               IF      SPA-ERRCD           =   SPACE
                   MOVE    "0140"          TO  SPA-JIDCD
               END-IF
           END-IF
      *
      *
      *H25.4
      *    レセ電移行データの一括削除
           IF     (SPA-ERRCD           =   SPACE  )
              AND (SPA-JIDCD           =   SPACE  )
              AND (SPA-GMN-HHKNCOMBI   =   "9999" )
              AND (SPA-J01-SRYKA       =   "00"   )
              AND (SPA-RRK-MAX         >   ZERO   )
              AND (SPA-GMN-RRKSRYKA(SPA-RRK-MAX)(1:1)
                                       =   "A"    )
               MOVE    "0149"          TO  SPA-JIDCD
           END-IF
      *
      *
           IF     (SPA-ERRCD           =   SPACE
              AND  SPA-JIDCD           =   SPACE )
            OR   ((WRK-ERRCD       NOT =   SPACE    ) AND
                  (SPA-ERRCD           =   WRK-ERRCD) AND
                  (WRK-ERRFLG2         =   "1"      ))
      *        保存用パラメタ作成
               PERFORM 4201-SRYACCT-PARA-SEC
               MOVE    1                   TO  SPA-GMN-PAGE
               MOVE    1                   TO  SPA-STRNUM
      *        診療会計マスターより編集をする
               PERFORM 410181-SRYACCT-HEN-SEC
           END-IF
           .
       42001-ZAIDELETE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴チェック処理
      *****************************************************************
       420012-JYURRKDEL-CHK-SEC        SECTION.
      *
           INITIALIZE                  JYURRK-REC
           MOVE    ACCT-HOSPNUM        TO  JYURRK-HOSPNUM
           MOVE    ACCT-PTID           TO  JYURRK-PTID
           MOVE    ACCT-NYUGAIKBN      TO  JYURRK-NYUGAIKBN
           MOVE    ACCT-SRYKA          TO  JYURRK-SRYKA
           MOVE    ACCT-HKNCOMBI       TO  JYURRK-HKNCOMBINUM
           PERFORM VARYING     IDZ     FROM    1   BY  1
                   UNTIL       IDZ     >   15
               MOVE    ACCT-ZAINUM         TO  JYURRK-ZAINUM (IDZ)
           END-PERFORM
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key20"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key20"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key20"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-JYURRK          =   ZERO
      ******   MOVE    "1009"          TO  SPA-ERRCD
               MOVE    "1009"          TO  WRK-ERRCD
               MOVE    "1"             TO  WRK-ERRFLG
           END-IF
      *
           .
       420012-JYURRKDEL-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    回数ゼロ剤削除処理
      *****************************************************************
       420011-SRYACT-DEL-SEC        SECTION.
      *
           MOVE    ACCT-SRYYM          TO  WRK-SRYYMD(1:6)
           MOVE    "01"                TO  WRK-SRYYMD(7:2)
      *
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
               PERFORM 930-SRYACT-RAED-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
           PERFORM UNTIL       FLG-SRYACT      =   1
      *
               PERFORM VARYING     IDZ     FROM    1   BY  1
                       UNTIL      (IDZ     >   5   )  OR
                                  (SRY-SRYCD (IDZ) =   SPACE)
      *            手技料の時、算定歴を判定する
                   IF     (SRY-SRYCD (IDZ)(1:1)    =   "1"    )
      *                システム予約コード
                       OR (SRY-SRYCD (IDZ)(1:3)    =   "099" )
                       MOVE    SRY-SRYCD (IDZ)     TO  TNS-SRYCD
                       MOVE    SRY-SRYCD (IDZ)     TO  WRK-SRYCD
      *                算定用の診療コードへ変更
                       PERFORM 910-TENSU-READ-SEC
                       PERFORM 45020-SANTEI-SRYCD-SEC
                       IF      SRY-SRYCD (IDZ) NOT =   WRK-SRYCD
                           MOVE    WRK-SRYCD       TO  TNS-SRYCD
                           PERFORM 910-TENSU-READ-SEC
                       END-IF
                       IF      TNS-SANTEIRRKKBN    NOT =   ZERO
                           PERFORM 4200111-SANTEI-DEL-SEC
                       END-IF
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-RAED-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           INITIALIZE                  SRYACT-REC
           MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "K03 SRYACT DEL ERR:" MCP-RC
                       ",KEY:" SRY-KEY
               MOVE    "2004"              TO  SPA-ERRCD
           END-IF
      *    コメント
           INITIALIZE                  PTCOM-REC
           MOVE    ACCT-HOSPNUM        TO  PTCOM-HOSPNUM
           MOVE    ACCT-PTID           TO  PTCOM-PTID
           MOVE    ACCT-ZAINUM         TO  PTCOM-ZAINUM
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "J02  PTCOM  DEL ERR:" MCP-RC
                     ",KEY:" PTCOM-KEY
               MOVE    "2004"              TO  SPA-ERRCD
           END-IF
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "J02 SRYACCT DEL ERR:" MCP-RC
                        ",KEY:" ACCT-KEY
               MOVE    "2004"              TO  SPA-ERRCD
           END-IF
      *
      *    附加情報削除
           INITIALIZE                  SRYACCTPLUS-REC
           MOVE    ACCT-HOSPNUM        TO  ACCTP-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  ACCTP-NYUGAIKBN
           MOVE    ACCT-PTID           TO  ACCTP-PTID
           MOVE    ACCT-SRYKA          TO  ACCTP-SRYKA
           MOVE    ACCT-SRYYM          TO  ACCTP-SRYYM
           MOVE    ACCT-ZAINUM         TO  ACCTP-ZAINUM
      *
           MOVE    SRYACCTPLUS-REC     TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "J02  SRYACCTPLUS  DEL ERR:" MCP-RC
                     ",KEY:" ACCTP-KEY
           END-IF
      *
           .
       420011-SRYACT-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴削除へ
      *****************************************************************
       4200111-SANTEI-DEL-SEC        SECTION.
      *
      *    保険をキーにするかのチェック
           SET     TBL-SAN             TO  1
           SEARCH  TBL-SANTEI-OCC      VARYING   TBL-SAN
               AT  END
                   MOVE    ZERO            TO  FLG-HKNSAN
               WHEN    WRK-SRYCD       =   TBL-SANTEI-SRYCD
                                                          (TBL-SAN)
                   MOVE    1               TO  FLG-HKNSAN
      *            Ｈ１８／４月から対応
                   IF      TBL-RIGAKU-RYO(TBL-SAN) =   "H"
      **************   IF      ACCT-SRYYM      >=  "200604"
                       IF      ACCT-SRYYM      <   "200604"
                           MOVE    ZERO            TO  FLG-HKNSAN
                       END-IF
                   END-IF
           END-SEARCH
      *H28.4
           IF     (WRK-SRYCD           =   "099800181"
                                       OR  "099800182")
              OR  (WRK-SRYCD(1:7)      =   "0998002"  )
      *        早期・初期リハ開始日・終了日
      *        がん患者リハ開始日・終了日
               MOVE    1                   TO  FLG-HKNSAN
           END-IF
      *
      *H25.7
      *    労災保険コード変更
           IF     (WRK-SRYCD(1:3)      =   "101"    )   AND
                  (ACCT-SRYYM          >=  "201307" )
               PERFORM 4200111-RSICD-SANTEI-CHK-SEC
           END-IF
      *
      *    労災保険判定
           MOVE    SPACE               TO  WRK-RSI-HKNKBN
           PERFORM VARYING     IDH     FROM    1   BY  1
                   UNTIL       IDH     >   SPA-HKN-MAX
               IF      SPA-GMN-THKNCOMBINUM (IDH)
                                           =   ACCT-HKNCOMBI
                   MOVE    SPA-NAI-THKNKBN (IDH)
                                               TO  WRK-RSI-HKNKBN
                   MOVE    SPA-HKN-MAX         TO  IDH
               END-IF
           END-PERFORM
      *
      *    算定履歴検索
           MOVE    SPACE               TO  SANTEI-REC
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM          TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    ACCT-SRYYM          TO  SANTEI-SRYYM
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           EVALUATE    TNS-SANTEIRRKKBN
               WHEN    1
                   MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                   MOVE    ZERO                TO  SANTEI-SRYKA
               WHEN    2
                   MOVE    ACCT-NYUGAIKBN      TO  SANTEI-NYUGAIKBN
                   MOVE    ZERO                TO  SANTEI-SRYKA
               WHEN    3
                   MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                   MOVE    ACCT-SRYKA          TO  SANTEI-SRYKA
               WHEN    4
                   MOVE    ACCT-NYUGAIKBN      TO  SANTEI-NYUGAIKBN
                   MOVE    ACCT-SRYKA          TO  SANTEI-SRYKA
           END-EVALUATE
      *
           IF     (FLG-HKNSAN          =   1   )  AND
                  (WRK-RSI-HKNKBN  NOT =   SPACE )
               MOVE     ACCT-HKNCOMBI      TO  SANTEI-HKNCOMBINUM
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
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      (FLG-SANTEI         =   ZERO )  AND
                   (SANTEI-MSANTEICNT  =   ZERO )
               MOVE    SANTEI-REC          TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "J02 SRYACCT DEL ERR:" MCP-RC
                            ",KEY:" ACCT-KEY
                   MOVE    "2004"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4200111-SANTEI-DEL-EXT.
           EXIT.
      *!!!!!!!!!!!!!! 
      *
      *****************************************************************
      *    労災コード保険毎のコードの判定処理
      *****************************************************************
       4200111-RSICD-SANTEI-CHK-SEC      SECTION.
      *
      *    労災のコードの判定
           SET     TBL-RCN             TO  1
           SEARCH  TBL-RSICHG-OCC      VARYING   TBL-RCN
               AT  END
                   MOVE    SPACE           TO  WRK-RSI-KBN
               WHEN    WRK-SRYCD       =   TBL-RSICHG-ATO-SRYCD
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
      *
           .
       4200111-RSICD-SANTEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    剤のない受診履歴の削除処理
      *****************************************************************
       43000-JYURRKDELETE-SYORI-SEC        SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-NAI-SRYYM       TO  JYURRK-SRYYMD(1:6)
           MOVE    "01"                TO  JYURRK-SRYYMD(7:2)
           MOVE    SPA-NAI-SRYYM       TO  JYURRK-UPSRYYMD(1:6)
           MOVE    "31"                TO  JYURRK-UPSRYYMD(7:2)
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           PERFORM
               UNTIL    FLG-JYURRK         =   1
               IF     JYURRK-ZAINUM-G      =   ZERO
                   PERFORM 43001-JYURRK-DELSYORI-SEC
               END-IF
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *H25.4
      *    レセ電移行データの一括削除
           IF     (SPA-ERRCD           =   SPACE  )
              AND (SPA-JIDCD           =   SPACE  )
              AND (SPA-GMN-HHKNCOMBI   =   "9999" )
              AND (SPA-J01-SRYKA       =   "00"   )
              AND (SPA-RRK-MAX         >   ZERO   )
              AND (SPA-GMN-RRKSRYKA(SPA-RRK-MAX)(1:1)
                                       =   "A"    )
               MOVE    "0149"          TO  SPA-JIDCD
           END-IF
      *
      *
           IF      SPA-ERRCD           =   SPACE
              AND  SPA-JIDCD           =   SPACE
      *
      *        受診履歴編集
               PERFORM 3103-JYURRK-HEN-SEC
      *        保存用パラメタ作成
               PERFORM 4201-SRYACCT-PARA-SEC
               MOVE    1                   TO  SPA-GMN-PAGE
               MOVE    1                   TO  SPA-STRNUM
      *        診療会計マスターより編集をする
               PERFORM 410181-SRYACCT-HEN-SEC
           END-IF
           .
       43000-JYURRKDELETE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴削除処理
      *****************************************************************
       43001-JYURRK-DELSYORI-SEC        SECTION.
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "J02 JYURRK DEL ERR:" MCP-RC
                               ",KEY:" JYURRK-KEY
               MOVE    "2001"              TO  SPA-ERRCD
           END-IF
      *
      *    同日に別の保険の履歴がある時、同日連番の再設定
           IF      JYURRK-EDANUM       =   1
              AND (JYURRK-NYUGAIKBN    =   "2"  )
               MOVE    JYURRK-REC          TO  MCPDATA-REC
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                   MOVE    "key4"              TO  MCP-PATHNAME
                   PERFORM 970-JYURRK-READ-SEC
               ELSE
                   MOVE    1               TO  FLG-JYURRK
               END-IF
               MOVE    ZERO                TO  WRK-DOUJI-RENNUM 
               MOVE    JYURRK-HKNCOMBINUM  TO  WRK-MAE-HKNCOMBINUM
               PERFORM
                   UNTIL   FLG-JYURRK      =   1
                   IF      JYURRK-HKNCOMBINUM
                                           NOT =   WRK-MAE-HKNCOMBINUM
                       ADD     1               TO  WRK-DOUJI-RENNUM
                   END-IF
                   IF      JYURRK-DOUJI-RENNUM =   WRK-DOUJI-RENNUM
                       CONTINUE
                   ELSE
                       PERFORM 430011-JYURRK-RENNUM-SEC
                   END-IF
                   MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                   MOVE    "key4"              TO  MCP-PATHNAME
                   PERFORM 970-JYURRK-READ-SEC
               END-PERFORM
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-IF
           .
       43001-JYURRK-DELSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤変更画面へ
      *****************************************************************
       430011-JYURRK-RENNUM-SEC        SECTION.
      *
           MOVE    JYURRK-KEY          TO  JYURRK-UPKEY
      *
           MOVE    WRK-DOUJI-RENNUM    TO  JYURRK-DOUJI-RENNUM
      *
           MOVE    SMCNDATE-YMD        TO  JYURRK-UPYMD
           MOVE    SMCNDATE-HMS        TO  JYURRK-UPHMS
           MOVE    SPA-OPID            TO  JYURRK-OPID
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "upd1"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "J02 JYURRK UPD ERR:" MCP-RC
                               ",KEY:" JYURRK-KEY
               MOVE    "2001"              TO  SPA-ERRCD
           END-IF
           .
       430011-JYURRK-RENNUM-EXT.
           EXIT.
      *
      *H25.4
      *****************************************************************
      *    レセ電移行データ削除
      *****************************************************************
       43990-RECEDEN-DELETE-SYORI-SEC        SECTION.
      *
           IF      SPA-JID1-FLG        =   "OK"
               PERFORM 439901-RECEDEN-DELETE-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        受診履歴編集
               PERFORM 3103-JYURRK-HEN-SEC
      *        保存用パラメタ作成
               PERFORM 4201-SRYACCT-PARA-SEC
               MOVE    1                   TO  SPA-GMN-PAGE
               MOVE    1                   TO  SPA-STRNUM
      *        診療会計マスターより編集をする
               PERFORM 410181-SRYACCT-HEN-SEC
           END-IF
      *
           .
       43990-RECEDEN-DELETE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電移行データ削除
      *****************************************************************
       439901-RECEDEN-DELETE-SEC        SECTION.
      *
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           STRING  SPA-NAI-SRYYM
                   "%"                 DELIMITED  BY  SIZE
                                       INTO    JYURRK-SRYYMD
           END-STRING
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key54"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key54"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           PERFORM
               UNTIL    FLG-JYURRK         =   1
               PERFORM VARYING     IDZ     FROM    1   BY  1
                       UNTIL       IDZ     >   15
                   INITIALIZE                      SRYACCT-REC 
                   MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
                   MOVE    JYURRK-NYUGAIKBN    TO  ACCT-NYUGAIKBN
                   MOVE    JYURRK-PTID         TO  ACCT-PTID
                   MOVE    JYURRK-SRYYMD(1:6)  TO  ACCT-SRYYM
                   MOVE    JYURRK-SRYKA        TO  ACCT-SRYKA
                   MOVE    JYURRK-ZAINUM (IDZ) TO  ACCT-ZAINUM
      *
                   MOVE    SRYACCT-REC         TO  MCPDATA-REC
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key5"              TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    "tbl_sryacct"       TO  MCP-TABLE
                       MOVE    "key5"              TO  MCP-PATHNAME
                       PERFORM 900-SRYACCT-READ-SEC
                   ELSE
                       INITIALIZE                  SRYACCT-REC
                       MOVE    1               TO  FLG-SRYACCT
                   END-IF
                   IF      FLG-SRYACCT     =   ZERO
      *                診療行為削除
                       PERFORM 439901-SRYACCT-DEL-SEC
                   END-IF
      *
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key5"             TO  MCP-PATHNAME
                   PERFORM 990-DBCLOSE-SEC
               END-PERFORM
      *
      *        受診履歴削除
               PERFORM 490112-JYURRK-DEL-SEC
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key54"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key54"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       439901-RECEDEN-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為など削除処理
      *****************************************************************
       439901-SRYACCT-DEL-SEC      SECTION.
      *
           INITIALIZE                  SRYACT-REC
           MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "K03 SRYACT DEL ERR:" MCP-RC
                       ",KEY:" SRY-KEY
               MOVE    "2004"              TO  SPA-ERRCD
           END-IF
      *    コメント
           INITIALIZE                  PTCOM-REC
           MOVE    ACCT-HOSPNUM        TO  PTCOM-HOSPNUM
           MOVE    ACCT-PTID           TO  PTCOM-PTID
           MOVE    ACCT-ZAINUM         TO  PTCOM-ZAINUM
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "J02  PTCOM  DEL ERR:" MCP-RC
                     ",KEY:" PTCOM-KEY
               MOVE    "2004"              TO  SPA-ERRCD
           END-IF
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "J02 SRYACCT DEL ERR:" MCP-RC
                        ",KEY:" ACCT-KEY
               MOVE    "2004"              TO  SPA-ERRCD
           END-IF
      *
      *    附加情報削除
           INITIALIZE                  SRYACCTPLUS-REC
           MOVE    ACCT-HOSPNUM        TO  ACCTP-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  ACCTP-NYUGAIKBN
           MOVE    ACCT-PTID           TO  ACCTP-PTID
           MOVE    ACCT-SRYKA          TO  ACCTP-SRYKA
           MOVE    ACCT-SRYYM          TO  ACCTP-SRYYM
           MOVE    ACCT-ZAINUM         TO  ACCTP-ZAINUM
      *
           MOVE    SRYACCTPLUS-REC     TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "J02  SRYACCTPLUS  DEL ERR:" MCP-RC
                     ",KEY:" ACCTP-KEY
           END-IF
           .
       439901-SRYACCT-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤変更画面へ
      *****************************************************************
       420-ZAIHENKOU-SEC        SECTION.
      *
           IF     (SPA-GMN-BANGO       =   ZERO )
             AND  (SPA-NAI-BANGO       =   ZERO )
      *        入院は新規作成可能
               IF     (SPA-GMN-NYUGAIKBN   =   "1" ) AND
                      (SPA-NAI-NYUINYMD    NOT =   SPACE )
                   IF      SPA-GMN-SRYKA       =   "00"
                       MOVE    06                  TO  SPA-GMN-CUR
                       MOVE    "0035"              TO  SPA-ERRCD
                   ELSE
                       MOVE    "0134"              TO  SPA-JIDCD
                   END-IF
               ELSE
                   MOVE    1                   TO  SPA-GMN-CUR
                   MOVE    "0013"              TO  SPA-ERRCD
               END-IF
               GO      TO      420-ZAIHENKOU-EXT
           END-IF
      *R01.5
           IF     (SPA-GMN-BANGO       =   ZERO )
             OR   (SPA-NAI-BANGO       =   ZERO )
               MOVE    1                   TO  SPA-GMN-CUR
               MOVE    "0013"              TO  SPA-ERRCD
               GO      TO      420-ZAIHENKOU-EXT
           END-IF
      *
           IF      SPA-NAI-RRKSELNUM   NOT =   ZERO
               MOVE    SPA-RRK-RRKYMD (SPA-NAI-RRKSELNUM)(7:2)
                                           TO  IDX-D
               IF      SPA-GMN-DAY (IDX-D) =   ZERO
                   MOVE    "0004"              TO  SPA-ERRCD
                   MOVE    3                   TO  SPA-GMN-CUR
                   GO      TO      420-ZAIHENKOU-EXT
               END-IF
      *        診療科・保険組合せが一致
               MOVE    SPA-NAI-BANGO       TO  IDX
               IF     (SPA-RRK-SRYKA  (SPA-NAI-RRKSELNUM)
                                       NOT =   SPA-NAI-SRYKA(IDX) )
                 OR   (SPA-RRK-HKNCOMBI(SPA-NAI-RRKSELNUM)
                                       NOT =   SPA-NAI-HKNCOMBI(IDX))
                   MOVE    "0004"              TO  SPA-ERRCD
                   MOVE    3                   TO  SPA-GMN-CUR
                   GO      TO      420-ZAIHENKOU-EXT
               END-IF
           END-IF
      *
      *    労災の時、剤点数が外来管理加算料に振り替えた時、エラー
           IF      SPA-NAI-HKNKBN      =   1
               PERFORM 4201-RIS-ZAIHENKOU-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   MOVE    1                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
      *    内服逓減の剤は対象外
           IF     (SPA-NAI-ZAITENFLG (SPA-NAI-BANGO)
                                       =   1
                                       OR  2       )  AND
                  (SPA-NAI-SRYKBN  (SPA-NAI-BANGO)
                                       =   "21"
                                       OR  "22"
                                       OR  "23"    )
               MOVE    "0027"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    "0104"              TO  SPA-JIDCD
           END-IF
      *
           .
       420-ZAIHENKOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災の時剤変更チェック処理
      *****************************************************************
       4201-RIS-ZAIHENKOU-SEC        SECTION.
      *
           IF     (SPA-NAI-SYUTEN2 (SPA-NAI-BANGO)
                                        >   ZERO )  AND
                  (SPA-NAI-ZAITEN  (SPA-NAI-BANGO)
                                        >   SPA-NAI-SYUTEN2
                                                (SPA-NAI-BANGO))
      *           画像診断はフィルム料が編集
             AND  (SPA-NAI-SRYKBN  (SPA-NAI-BANGO)
                                       NOT =   "70" )
               MOVE    "0021"               TO  SPA-ERRCD
           END-IF
           .
       4201-RIS-ZAIHENKOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤変更画面へ
      *****************************************************************
       420-ZAIHENKOU-SYORI-SEC        SECTION.
      *
      *    診療会計マスター更新
           PERFORM 4901-SRYACCT-UPD-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (FLG-END             =   1     )
               MOVE    5               TO  SPA-J025-SYORI
           ELSE
               PERFORM 4201-ZAIHENKOU-SYORI1-SEC
           END-IF
           .
       420-ZAIHENKOU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤変更画面へ
      *****************************************************************
       4201-ZAIHENKOU-SYORI1-SEC        SECTION.
      *
      *    選択の内容をパラメタに編集
           MOVE    SPA-NAI-SRYYM       TO  SPA-J02-SRYYMD(1:6)
           MOVE    SPA-GMN-SRYYM       TO  SPA-J02-SRYYMDW(1:6)
      *
           IF      SPA-NAI-BANGO       =   ZERO
               MOVE    SPA-GMN-SRYKAMEI    TO  SPA-J01-SRYKAMEI
               MOVE    SPA-GMN-SRYKA       TO  SPA-J01-SRYKA
               IF      SPA-GMN-HHKNCOMBI   =   "0000"
                   MOVE    SPACE               TO  SPA-HKNCOMBINUM
                   MOVE    SPACE               TO  SPA-J02-HKNCOMBIMEI
               ELSE
                   MOVE    SPA-GMN-HHKNCOMBI   TO  SPA-HKNCOMBINUM
                   MOVE    SPA-GMN-HHKNCOMBIMEI
                                               TO  SPA-J02-HKNCOMBIMEI
               END-IF
               INITIALIZE                  SPA-ZAINUMSEL-AREA
               MOVE    ZERO                TO  SPA-ZAINUM
               MOVE    1                   TO  SPA-J04-SYORI
           ELSE
               MOVE    ZERO                TO  SPA-J04-SYORI
      *
           MOVE    SPA-NAI-BANGO       TO  IDX
      *    見出し
           MOVE    SPA-GMN-SRYKAMEI    TO  SPA-J01-SRYKAMEI
      *    診療科
      **** MOVE    SPA-GMN-SRYKA       TO  SPA-J01-SRYKA
           MOVE    SPA-NAI-SRYKA(IDX)  TO  SPA-J01-SRYKA
      *    剤番号
           MOVE    SPA-NAI-SRYKBN(IDX)
                                       TO  SPA-SRYKBN
           MOVE    SPA-NAI-ZAINUM(IDX)
                                       TO  SPA-ZAINUM
      *    枝番
           MOVE    SPA-NAI-RENNUM(IDX) TO  SPA-RENNUM
      *    包括診療区分
           MOVE    SPA-NAI-JIHOKBN (IDX)   TO  SPA-JIHOKBN
      *    剤区分
           MOVE    SPA-NAI-ZAIKBN (IDX)    TO  SPA-ZAIKBN
      *
      *    老人区分
      **** MOVE    SPA-NAI-ROUJIN(IDX)
      ****************                 TO  SPA-ROUJIN
      *    剤点数
           MOVE    SPA-NAI-ZAITEN(IDX)
                                       TO  SPA-ZAITEN
      *    保険組合番号
           MOVE    SPA-NAI-HKNCOMBI(IDX)
                                       TO  SPA-HKNCOMBINUM
      *    保険組合せ内容
           MOVE    SPA-GMN-HKNCOMBIMEI TO  SPA-J02-HKNCOMBIMEI
      *    診療種別
           MOVE    SPA-GMN-SRYKBN      TO  SPA-GMN-SRYKBN-MAE
      *H28.2
      *    後発品変更不可（処方）あり
           MOVE    SPA-NAI-GENEFLG (IDX)   TO  SPA-GENEFUKAFLG
      *
           END-IF
      *
      *    保険算定年齢チェック等
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  AGECHK-PTID
           MOVE    SPA-NAI-SRYYM       TO  AGECHK-SRYYMD(1:6)
           MOVE    "01"                TO  AGECHK-SRYYMD(7:2)
           MOVE    SPA-BIRTHDAY        TO  AGECHK-BIRTHDAY
           MOVE    SPA-HKNCOMBINUM     TO  AGECHK-HKNCOMBINUM
           MOVE    SPA-GMN-NYUGAIKBN   TO  AGECHK-NYUGAIKBN
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
      *    負担率
           MOVE    AGECHK-FTNRATEMEI   TO  SPA-J02-FTNRATEMEI
      *    老人区分
           MOVE    AGECHK-ROUJIN       TO  SPA-ROUJIN
      *    回数テーブル
           MOVE    SPA-GMN-DAY-G       TO  SPA-SEL-DAY-G
      *
      *    受診履歴決定
           IF      SPA-NAI-RRKSELNUM   =   ZERO
               MOVE    SPACE               TO  SPA-SEL-RRKYMD
               MOVE    SPACE               TO  SPA-SEL-RRKYMDG
               MOVE    ZERO                TO  SPA-SEL-RENNUM
               MOVE    ZERO                TO  SPA-SEL-DOUJI-RENNUM
               MOVE    ZERO                TO  SPA-SEL-DENPNUM
           ELSE
               MOVE    SPA-GMN-RRKYMD (SPA-NAI-RRKSELNUM)
                                           TO  SPA-SEL-RRKYMDG
               MOVE    SPA-RRK-RRKYMD (SPA-NAI-RRKSELNUM)
                                           TO  SPA-SEL-RRKYMD
               MOVE    SPA-RRK-RENNUM (SPA-NAI-RRKSELNUM)
                                           TO  SPA-SEL-RENNUM
               MOVE    SPA-RRK-DOUJI-RENNUM (SPA-NAI-RRKSELNUM)
                                           TO  SPA-SEL-DOUJI-RENNUM
               MOVE    SPA-RRK-DENPNUM (SPA-NAI-RRKSELNUM)
                                           TO  SPA-SEL-DENPNUM
               MOVE    ZERO                TO  SPA-SEL-DAY-G
               MOVE    SPA-SEL-RRKYMD(7:2)     TO  IDX-D2
               COMPUTE IDX-D               =   SPA-SEL-RENNUM  + 1
               MOVE    SPA-NAI-DAY (IDX IDX-D  IDX-D2)
                                           TO  SPA-SEL-DAY (IDX-D2)
           END-IF
      *
      *    診療開始年月日
           IF      SPA-NAI-BANGO       =   ZERO
               MOVE    "01"                TO  SPA-J02-SRYYMD(7:2)
           ELSE
               MOVE    ZERO                TO  SPA-J02-SRYYMD(7:2)
               MOVE    "01"                TO  SPA-J02-SRYYMD(7:2)
               PERFORM VARYING     IDX-D   FROM    1   BY  1
                       UNTIL       IDX-D   >   31
                   IF      SPA-GMN-DAY (IDX-D) NOT =   ZERO
                       MOVE    IDX-D           TO  WRK-DD
                       MOVE    WRK-DD          TO  SPA-J02-SRYYMD(7:2)
                       MOVE    31              TO  IDX-D
                   END-IF
               END-PERFORM
           END-IF
      *
           MOVE    SPA-SRYYMD          TO  SPA-MAE-SRYYMD
           MOVE    SPA-PTID            TO  SPA-MAE-PTID
           MOVE    SPA-J02-SRYYMD      TO  SPA-SRYYMD
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
      *
      *    労災
           MOVE    SPA-NAI-HKNKBN      TO  SPA-HKNKBN
           MOVE    SPA-NAI-SISIKBN     TO  SPA-RSI-SISIKBN
      *    労災区分
           MOVE    SPA-NAI-RSI-HKNKBN  TO  SPA-RSI-HKNKBN
      *    健保準拠区分
           MOVE    SPA-NAI-RSI-JUNKYO  TO  SPA-RSI-JUNKYO
      *
      **** 診療会計マスター更新
      *    PERFORM 4901-SRYACCT-UPD-SEC
      *    IF      SPA-ERRCD       NOT =   SPACE
      *        GO      TO       420-ZAIHENKOU-SYORI-EXT
      **** END-IF
      *
      *    診療開始日時点の年齢計算
           MOVE    SPA-SRYYMD          TO  WRK-YMDS2
           PERFORM 30101-NENREI-SET-SEC
      *
      *    保存用パラメタ作成
           PERFORM 4201-SRYACCT-PARA-SEC
      *
           MOVE    "J02"               TO  SPA-MOTOPG
           MOVE    "J04"               TO  SPA-SAKIPG
      *
      *****MOVE    "CHANGE"            TO  MCP-PUTTYPE.
           MOVE    "JOIN"              TO  MCP-PUTTYPE.
           MOVE    "J04"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       420-ZAIHENKOU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診日訂正処理
      *****************************************************************
       440-SRYYMD-TEISEI-SEC           SECTION.
      *
      *    患者選択チェック
           IF      SPA-GMN-MAX         =   ZERO
               GO      TO      440-SRYYMD-TEISEI-EXT
           END-IF
      *    受診日訂正へ
           MOVE    2                   TO  SPA-J02-SYORI
      *
           MOVE    3                   TO  SPA-GMN-CUR
           .
       440-SRYYMD-TEISEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスター  保存用パラメタ作成処理
      *****************************************************************
       4201-SRYACCT-PARA-SEC           SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    パラメタ削除
           INITIALIZE                      PARA-REC
           MOVE    "J02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "SRYACCT"           TO  PARA-FILEMEI
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    診療会計マスターを編集
           INITIALIZE                      SRYACCT-REC 
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  ACCT-PTID
           MOVE    SPA-NAI-SRYYM       TO  ACCT-SRYYM
      *
           IF      SPA-J01-SRYKA       =   ZERO
               MOVE    "key19"             TO  WRK-ORC-DBPATH
               IF      SPA-GMN-SRYKBN      =   "01"
      *            診療区分順
                   MOVE    "key41"         TO  WRK-ORC-DBPATH
               END-IF
           ELSE
               MOVE    SPA-J01-SRYKA       TO  ACCT-SRYKA
               MOVE    "key18"             TO  WRK-ORC-DBPATH
           END-IF
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
               PERFORM 900-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
           MOVE    ZERO                TO  SPA-SRYACCT-MAX
           PERFORM UNTIL      (FLG-SRYACCT     =   1   )
      *        受診履歴の剤に診療種別設定
               PERFORM 42011-JYURRK-SRYKBN-SEC
      *
               MOVE    ZERO                 TO  FLG-ACCT-ERR
               IF     (SPA-GMN-HHKNCOMBI        =   ZERO    )  OR
                     ((SPA-GMN-HHKNCOMBI    NOT =   ZERO    )  AND
                      (SPA-GMN-HHKNCOMBI        =   ACCT-HKNCOMBI))
                  CONTINUE
               ELSE
                  MOVE    1                    TO  FLG-ACCT-ERR
               END-IF
               IF     (SPA-GMN-SRYKBN      =   ZERO
                                           OR  "01"            )  OR
                      (SPA-GMN-SRYKBN  NOT =   ZERO    AND
                       SPA-GMN-SRYKBN(1:1) =   ACCT-SRYKBN(1:1) )
                  CONTINUE
               ELSE
                  MOVE    1                    TO  FLG-ACCT-ERR
               END-IF
      *
               IF      FLG-ACCT-ERR        =   ZERO
                   ADD     1                   TO  SPA-SRYACCT-MAX
      *            パラメタ登録
                   INITIALIZE                      PARA-REC
                   MOVE    "J02"               TO  PARA-GYOUMUID
                   MOVE    SPA-TERMID          TO  PARA-TERMID
                   MOVE    "SRYACCT"           TO  PARA-FILEMEI
                   MOVE    SPA-SRYACCT-MAX     TO  PARA-EDANUM
                   MOVE    SRYACCT-REC         TO  PARA-DATA-REC
      *H30.12
                   MOVE    SMCNDATE-YMD        TO  PARA-UPYMD
                   MOVE    SMCNDATE-HMS        TO  PARA-UPHMS
      *
                   MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
                   MOVE    PARA-REC            TO  MCPDATA-REC
                   MOVE    "DBINSERT"          TO  MCP-FUNC
                   MOVE    "tbl_para"          TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                       DISPLAY "J02 PARA INSERT ERR:"  MCP-RC
                               ",KEY1:" PARA-KEY(1:42)
                       MOVE    "0014"              TO  SPA-ERRCD
                   END-IF
               END-IF
      *
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
               PERFORM 900-SRYACCT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    最大ページ数
           IF      SPA-SRYACCT-MAX     =   ZERO
               MOVE    1               TO  SPA-MAX-PAGE
           ELSE
               COMPUTE SPA-MAX-PAGE    =  (SPA-SRYACCT-MAX   /   400)
                                       +   1
           END-IF
      *****MOVE    1                   TO  SPA-GMN-PAGE
      *****MOVE    1                   TO  SPA-STRNUM
      *
           .
       4201-SRYACCT-PARA-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴の剤に診療種別設定処理
      *****************************************************************
       42011-JYURRK-SRYKBN-SEC        SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-RRK-MAX
               PERFORM VARYING     IDX-RRK     FROM    1   BY  1
                       UNTIL      (IDX-RRK     >=  135 )  OR
                                  (SPA-RRK-ZAINUM (IDX IDX-RRK)
                                               =   ZERO )
                   IF      ACCT-ZAINUM         =   SPA-RRK-ZAINUM
                                                        (IDX IDX-RRK)
                       MOVE    ACCT-SRYKBN     TO  SPA-RRK-SRYKBN
                                                        (IDX IDX-RRK)
                   END-IF
               END-PERFORM
           END-PERFORM
           .
       42011-JYURRK-SRYKBN-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *----(01.00.01) LINE ADD START ----------------------------------
      *    更新前のチェック
           IF      SPA-UPDFLG          =   "1"
               MOVE    "0102"          TO  SPA-JIDCD
               MOVE    99              TO  SPA-GMN-CUR
               GO      TO      210-BACK-EXT
           END-IF
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
      *    ファイル削除
           PERFORM 999-FILE-DEL-SEC
      *
      *
           MOVE    "J02"               TO  SPA-MOTOPG
           MOVE    SPA-MOTOPG2         TO  SPA-SAKIPG
           IF      SPA-J02-MOTOPG      =   "M01"
      *        業務選択へ
               MOVE   "M01"                TO  SPA-SAKIPG
               MOVE    SPACE               TO  LINKAREA
               INITIALIZE                  SPA-KYOTUKEY
               MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
           ELSE
      *        元画面へ
               MOVE    SPACE               TO  LINKAREA
               IF      SPA-J02-MOTOFLG     =   "0"
                   INITIALIZE                  SPA-KYOTUKEY
               END-IF
               MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
               MOVE    SPACE               TO  SPA-MOTOPG2
           END-IF
      *
           MOVE    SPACE               TO  SPA-MOTOPG2
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK-SYORI-SEC           SECTION.
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
      *    ファイル削除
           PERFORM 999-FILE-DEL-SEC
      *
           MOVE    "J02"               TO  SPA-MOTOPG
           MOVE    SPA-J02-MOTOPG      TO  SPA-SAKIPG
           IF      SPA-J02-MOTOFLG     =   "0"
               INITIALIZE                  SPA-KYOTUKEY
           END-IF
           IF      SPA-J02-MOTOPG      =   "M01"
      *        業務選択へ
               MOVE   "M01"                TO  SPA-SAKIPG
               MOVE    SPACE               TO  LINKAREA
               INITIALIZE                  SPA-KYOTUKEY
               MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
           ELSE 
      *        元画面へ
               MOVE    SPACE               TO  LINKAREA
               MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
               MOVE    SPACE               TO  SPA-MOTOPG2
           END-IF
      *
           MOVE    SPACE               TO  SPA-MOTOPG2
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE.
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    前回患者処理
      *****************************************************************
       430-MAEKPTNUM-SEC              SECTION.
      *
           IF      SPA-LAST-PTNUM      =   SPACE
               GO      TO      430-MAEKPTNUM-EXT
           END-IF
      *
           MOVE    SPA-LAST-PTNUM      TO  J02-PTNUM
           PERFORM 41010-PTNUM-SYORI-SEC
      *
           .
       430-MAEKPTNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェック画面へ
      *****************************************************************
       430-CHEKU-SEC              SECTION.
      *
      *----(01.00.01) LINE ADD START ----------------------------------
      *    剤修正のチェック
           IF      SPA-GMN-BANGO   NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
               GO      TO      430-CHEKU-EXT
           END-IF
      *    患者選択チェック
           IF      SPA-GMN-PTID        =   ZERO
               MOVE    "0019"          TO  SPA-ERRCD
               GO      TO      430-CHEKU-EXT
           END-IF
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
      *
           MOVE    SPA-GMN-SRYYM       TO  SPA-J02-SRYYMDW(1:6)
           MOVE    SPA-NAI-SRYYM       TO  SPA-J02-SRYYMD (1:6)
           MOVE    SPA-GMN-SRYKA       TO  SPA-J01-SRYKA
           MOVE    SPA-GMN-SRYKAMEI    TO  SPA-J01-SRYKAMEI
      *    診療種別
           MOVE    SPA-GMN-SRYKBN      TO  SPA-GMN-SRYKBN-MAE
      *
           MOVE    "J02"               TO  SPA-MOTOPG
           MOVE    "J03"               TO  SPA-SAKIPG
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE.
      *****MOVE    "JOIN"              TO  MCP-PUTTYPE.
           MOVE    "J03"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       430-CHEKU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    フリーコメント画面へ
      *****************************************************************
       430-FREEMSG-SEC              SECTION.
      *
      **** 患者選択チェック
      *    IF      SPA-GMN-PTID        =   ZERO
      *        MOVE    "0019"          TO  SPA-ERRCD
      *        GO      TO      430-FREEMSG-EXT
      **** END-IF
      *    剤修正のチェック
           IF      SPA-GMN-BANGO   NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
               GO      TO      430-FREEMSG-EXT
           END-IF
      *
      *    戻り時、画面を元へ戻す
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-J02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "J02"               TO  SPA-MOTOPG
           MOVE    "C50"               TO  SPA-SAKIPG
      *
      *****MOVE    SPACE               TO  SPA-KYOTUKEY
      *    遷移後の初期表示
           MOVE    SPA-GMN-SRYYM       TO  SPA-SRYYMDW(1:6)
           MOVE    SPA-NAI-SRYYM       TO  SPA-SRYYMD (1:6)
           MOVE    SPA-GMN-SRYKA       TO  SPA-SRYKA
           MOVE    SPA-GMN-NYUGAIKBN   TO  SPA-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
           MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
           MOVE    SPA-GMN-NAME        TO  SPA-NAME
           MOVE    SPA-GMN-SEX         TO  SPA-SEX
           MOVE    SPA-GMN-BIRTHDAYW   TO  SPA-BIRTHDAYW
           MOVE    SPA-GMN-BIRTHDAY    TO  SPA-BIRTHDAY
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE.
           MOVE    "C50"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       430-FREEMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴画面へ
      *****************************************************************
       4801-K06-SYORI-SEC              SECTION.
      *
      *    患者選択チェック
           IF      SPA-GMN-PTID        =   ZERO
               MOVE    "0019"          TO  SPA-ERRCD
               GO      TO      4801-K06-SYORI-EXT
           END-IF
      *    剤修正のチェック
           IF      SPA-GMN-BANGO   NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
               GO      TO      4801-K06-SYORI-EXT
           END-IF
      *    診療科選択チェック
           IF      SPA-GMN-SRYKA       =   ZERO
               MOVE    "0036"              TO  SPA-ERRCD
               MOVE    06                  TO  SPA-GMN-CUR
               GO      TO      4801-K06-SYORI-EXT
           END-IF
      *
      *    戻り時、画面を元へ戻す
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-J02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "J02"               TO  SPA-MOTOPG
           MOVE    "K06"               TO  SPA-SAKIPG
      *
      *****MOVE    SPACE               TO  SPA-KYOTUKEY
      *    遷移後の初期表示
           MOVE    SPA-GMN-SRYYM       TO  SPA-SRYYMDW(1:6)
           MOVE    SPA-NAI-SRYYM       TO  SPA-SRYYMD (1:6)
           MOVE    SPA-GMN-SRYKA       TO  SPA-SRYKA
           MOVE    SPA-GMN-NYUGAIKBN   TO  SPA-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
           MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
           MOVE    SPA-GMN-NAME        TO  SPA-NAME
           MOVE    SPA-GMN-KANANAME    TO  SPA-KANANAME
           MOVE    SPA-GMN-SEX         TO  SPA-SEX
           MOVE    SPA-GMN-BIRTHDAYW   TO  SPA-BIRTHDAYW
           MOVE    SPA-GMN-BIRTHDAY    TO  SPA-BIRTHDAY
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE.
           MOVE    "K06"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4801-K06-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    前月選択　処理
      *****************************************************************
       41018-ZENGETU-HEN-SEC             SECTION.
      *
      *----(01.00.01) LINE ADD START ----------------------------------
      *    剤修正のチェック
           IF      SPA-GMN-BANGO   NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
               GO      TO      41018-ZENGETU-HEN-EXT
           END-IF
      *
           IF     (SPA-GMN-PTNUM   NOT =   SPACE )  AND
                  (SPA-GMN-PTID    NOT =   ZERO  )
               MOVE    1                   TO  SPA-J02-SEL
      *
               IF      SPA-UPDFLG          =   "1"
                   MOVE    "0106"              TO  SPA-JIDCD
               ELSE
                   PERFORM 41018-ZENGETU-SYORI-SEC
               END-IF
           END-IF
      *
           .
       41018-ZENGETU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    前月選択　処理
      *****************************************************************
       41018-ZENGETU-SYORI-SEC             SECTION.
      *
      *----(01.00.01) LINE ADD END   ----------------------------------
      *    現在表示されている内容を更新する
      *    診療会計マスター更新
           IF      SPA-UPDFLG          =   "1"
               PERFORM 4901-SRYACCT-UPD-SEC
               IF     (SPA-ERRCD       NOT =   SPACE )  OR
                      (FLG-END             =   1     )
                   MOVE    6               TO  SPA-J025-SYORI
               ELSE
                   PERFORM 410181-ZENGETU-SYORI1-SEC
               END-IF
           ELSE
               PERFORM 410181-ZENGETU-SYORI1-SEC
           END-IF
           .
       41018-ZENGETU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    前月選択　処理
      *****************************************************************
       410181-ZENGETU-SYORI1-SEC       SECTION.
      *
           IF      SPA-BIRTHDAY(1:6)   =   SPA-NAI-SRYMM
               MOVE    "0015"              TO  SPA-ERRCD
               GO      TO       410181-ZENGETU-SYORI1-EXT
            END-IF
      *
           COMPUTE SPA-NAI-SRYMM       =   SPA-NAI-SRYMM
                                       -   1
           IF      SPA-NAI-SRYMM       =   ZERO
               MOVE    12                  TO  SPA-NAI-SRYMM
               COMPUTE SPA-NAI-SRYYY       =   SPA-NAI-SRYYY    -   1
           END-IF
      *
           MOVE    SPA-NAI-SRYYM      TO  WRK-SYMD(1:6)
           MOVE    "01"               TO  WRK-SYMD(7:2)
           PERFORM 41012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:6)    TO  SPA-GMN-SRYYM
      *
      *    診療月変更処理
           PERFORM 410181-SRYYM-CHANGE-SYORI-SEC
      *
           IF      SPA-RRK-MAX         =   ZERO
              AND  SPA-GMN-MAX         =   ZERO
               IF      SPA-SRYKA-MAX       =   1
                   MOVE    07              TO  SPA-GMN-CUR
               ELSE
                   MOVE    06              TO  SPA-GMN-CUR
               END-IF
           ELSE
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
      *
           .
       410181-ZENGETU-SYORI1-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスターより編集　処理
      *****************************************************************
       410181-SRYACCT-HEN-SEC             SECTION.
      *
      *    コラムリスト位置
           MOVE    1                  TO  FLG-GMN-INIT
      *
           MOVE    SPA-NAI-SRYYM      TO  WRK-SYMD(1:6)
           MOVE    "01"               TO  WRK-SYMD(7:2)
           PERFORM 41012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:6)    TO  SPA-GMN-SRYYM
      *
           MOVE    SPA-GMN-SRYYM       TO  SPA-J02-SRYYMDW
           MOVE    SPA-NAI-SRYYM       TO  SPA-J02-SRYYMD
      *    診療年月の末日算定
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    SPA-NAI-SRYYM       TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI(7:2)    TO  SPA-SRYMATU
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDG
           MOVE    ZERO                TO  SPA-GMN-MAX
           INITIALIZE                  SPA-GMN-TBL
           INITIALIZE                  SPA-NAI-TBL
           INITIALIZE                  SPA-GMN-INPUT
           MOVE    ZERO                TO  WRK-RSAI-GAIRAITEN
      *
      *    パラメタより診療会計マスターを編集
           INITIALIZE                      PARA-REC
           MOVE    "J02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "SRYACCT"           TO  PARA-FILEMEI
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
           MOVE    ZERO                TO  SPA-ENDNUM
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDG
           MOVE    ZERO                TO  WRK-BANGO
           MOVE    SPA-STRNUM          TO  SPA-MAE-STRNUM
           PERFORM
               UNTIL   FLG-PARA            =   1
      *        番号
               ADD     1                   TO  WRK-BANGO
      *        開始位置の判定
               IF      SPA-STRNUM          >   PARA-EDANUM
                   CONTINUE
               ELSE
                   MOVE    ZERO                TO  FLG-HEN-OK
                   MOVE    PARA-DATA-REC       TO  SRYACCT-REC
                   ADD     1                   TO  IDX
                   PERFORM 3101-SRYACCT-SPA-SEC
                   IF      FLG-HEN-OK           =   1
                       MOVE    PARA-EDANUM         TO  SPA-ENDNUM
                   END-IF
                   IF      IDG                 >=  400
                       MOVE    1               TO  FLG-PARA
                   END-IF
               END-IF
      *
               IF      FLG-PARA            =   ZERO
                   MOVE    "tbl_para"          TO  MCP-TABLE
                   MOVE    "key5"              TO  MCP-PATHNAME
                   PERFORM 990-PARA-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    IDX                 TO  SPA-NAI-MAX
           MOVE    IDG                 TO  SPA-GMN-MAX
           IF      IDX                 >   400
               MOVE    400             TO  SPA-NAI-MAX
           END-IF
           IF      IDG                 >   400
               MOVE    400             TO  SPA-GMN-MAX
           END-IF
      *
      *    すべての行を選択可能とする
           MOVE    ZERO                TO  WRK-TBANGO
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX             >   SPA-GMN-MAX
               IF      SPA-GMN-TBLREC(IDX)(1:3)    NOT =   "---"
                   IF      SPA-GMN-TBANGO (IDX)    NOT =   ZERO
                       MOVE    SPA-GMN-TBANGO (IDX)    TO  WRK-TBANGO
                   END-IF
                   MOVE    WRK-TBANGO      TO  SPA-NAI-TBANGO2(IDX)
               END-IF
           END-PERFORM
      *
           .
       410181-SRYACCT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次月選択　処理
      *****************************************************************
       41019-JIGETU-HEN-SEC             SECTION.
      *
      *----(01.00.01) LINE ADD START ----------------------------------
      *    剤修正のチェック
           IF      SPA-GMN-BANGO   NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
               GO      TO      41019-JIGETU-HEN-EXT
           END-IF
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
           IF     (SPA-GMN-PTNUM   NOT =   SPACE )  AND
                  (SPA-GMN-PTID    NOT =   ZERO  )
               MOVE    2                   TO  SPA-J02-SEL
               IF      SPA-UPDFLG          =   "1"
                   MOVE    "0106"              TO  SPA-JIDCD
               ELSE
                   PERFORM 41019-JIGETU-SYORI-SEC
               END-IF
           END-IF
      *
           .
       41019-JIGETU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次月選択　処理
      *****************************************************************
       41019-JIGETU-SYORI-SEC             SECTION.
      *
      *    現在表示されている内容を更新する
      *    診療会計マスター更新
           IF      SPA-UPDFLG          =   "1"
               PERFORM 4901-SRYACCT-UPD-SEC
               IF     (SPA-ERRCD      NOT  =   SPACE ) OR
                      (FLG-END             =   1     )
                   MOVE    7               TO  SPA-J025-SYORI
               ELSE
                   PERFORM 410191-JIGETU-SYORI1-SEC
               END-IF
           ELSE
               PERFORM 410191-JIGETU-SYORI1-SEC
           END-IF
           .
       41019-JIGETU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    次月選択　処理
      *****************************************************************
       410191-JIGETU-SYORI1-SEC             SECTION.
      *
           COMPUTE SPA-NAI-SRYMM       =   SPA-NAI-SRYMM
                                       +   1
           IF      SPA-NAI-SRYMM       >   12
               MOVE    1                   TO  SPA-NAI-SRYMM
               COMPUTE SPA-NAI-SRYYY       =   SPA-NAI-SRYYY    +   1
           END-IF
      *
           MOVE    SPA-NAI-SRYYM      TO  WRK-SYMD(1:6)
           MOVE    "01"               TO  WRK-SYMD(7:2)
           PERFORM 41012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:6)    TO  SPA-GMN-SRYYM
      *
      *
      *    診療月変更処理
           PERFORM 410181-SRYYM-CHANGE-SYORI-SEC
      *
           IF      SPA-RRK-MAX         =   ZERO
              AND  SPA-GMN-MAX         =   ZERO
               IF      SPA-SRYKA-MAX       =   1
                   MOVE    07              TO  SPA-GMN-CUR
               ELSE
                   MOVE    06              TO  SPA-GMN-CUR
               END-IF
           ELSE
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
      *
           .
       410191-JIGETU-SYORI1-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       440-MAEGMN-SEC             SECTION.
      *
           IF     (SPA-GMN-PAGE        =   1  )
              OR  (SPA-GMN-PTNUM       =   SPACE)
               GO      TO       440-MAEGMN-EXT
           END-IF
      *
           IF      SPA-UPDFLG          =   "1"
               MOVE    "0113"              TO  SPA-JIDCD
           ELSE
               PERFORM 4401-MAEGMN-SYORI-SEC
           END-IF
           .
       440-MAEGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       4401-MAEGMN-SYORI-SEC             SECTION.
      *
      *    現在表示されている内容を更新する
      *    診療会計マスター更新
           PERFORM 4901-SRYACCT-UPD-SEC
           IF     (SPA-ERRCD      NOT  =   SPACE ) OR
                  (FLG-END             =   1     )
               GO      TO       4401-MAEGMN-SYORI-EXT
           END-IF
      *
           IF      SPA-GMN-PAGE        =   1
               MOVE    1               TO  SPA-GMN-PAGE
           ELSE
               COMPUTE SPA-GMN-PAGE    =   SPA-GMN-PAGE   -   1
           END-IF
      *
           IF      SPA-GMN-PAGE        =   1
               MOVE    1               TO  SPA-STRNUM
           ELSE
               MOVE    SPA-MAE-STRNUM  TO  SPA-STRNUM
           END-IF
      *
      *    受診履歴編集
           PERFORM 3103-JYURRK-HEN-SEC
      *
      *    保存用パラメタ作成
           PERFORM 4201-SRYACCT-PARA-SEC
      *
      *    診療会計マスターより編集をする
           PERFORM 410181-SRYACCT-HEN-SEC
      *
           IF      SPA-RRK-MAX         =   ZERO
              AND  SPA-GMN-MAX         =   ZERO
               IF      SPA-SRYKA-MAX       =   1
                   MOVE    07              TO  SPA-GMN-CUR
               ELSE
                   MOVE    06              TO  SPA-GMN-CUR
               END-IF
           ELSE
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
           .
       4401-MAEGMN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁　処理
      *****************************************************************
       450-ATOGMN-SEC             SECTION.
      *
          IF      SPA-SRYACCT-MAX     <=  SPA-ENDNUM
             OR  (SPA-GMN-PTNUM       =   SPACE)
               GO      TO       450-ATOGMN-EXT
           END-IF
      *
           IF      SPA-UPDFLG          =   "1"
               MOVE    "0114"              TO  SPA-JIDCD
           ELSE
               PERFORM 4501-ATOGMN-SYORI-SEC
           END-IF
           .
       450-ATOGMN-EXT.
           EXIT.
      *****************************************************************
      *    次頁　処理
      *****************************************************************
       4501-ATOGMN-SYORI-SEC             SECTION.
      *
      *    現在表示されている内容を更新する
      *    診療会計マスター更新
           PERFORM 4901-SRYACCT-UPD-SEC
           IF     (SPA-ERRCD      NOT  =   SPACE ) OR
                  (SPA-JIDCD      NOT =   SPACE ) OR
                  (FLG-END             =   1     )
               GO      TO       4501-ATOGMN-SYORI-EXT
           END-IF
      *
           ADD     1                   TO  SPA-GMN-PAGE
           MOVE    SPA-ENDNUM          TO  SPA-STRNUM
           ADD     1                   TO  SPA-STRNUM
      *
      *    受診履歴編集
           PERFORM 3103-JYURRK-HEN-SEC
      *
      *    保存用パラメタ作成
           PERFORM 4201-SRYACCT-PARA-SEC
      *
      *
      *    診療会計マスターより編集をする
           PERFORM 410181-SRYACCT-HEN-SEC
      *
           IF      SPA-RRK-MAX         =   ZERO
              AND  SPA-GMN-MAX         =   ZERO
               IF      SPA-SRYKA-MAX       =   1
                   MOVE    07              TO  SPA-GMN-CUR
               ELSE
                   MOVE    06              TO  SPA-GMN-CUR
               END-IF
           ELSE
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
           .
       4501-ATOGMN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    変更確定　処理
      *****************************************************************
       470-KAKUTEI-SEC             SECTION.
      *
           IF     (SPA-GMN-BANGO       =   ZERO )
      *R01.5
               INITIALIZE                  SPA-GMN-INPUT
               INITIALIZE                  SPA-NAIGMN-AREA
               MOVE    ZERO                TO  SPA-J02-SYORI
      *        番号選択へカーソル移動
               MOVE    1                   TO  SPA-GMN-CUR
               GO      TO      470-KAKUTEI-EXT
           END-IF
      *R01.5
           IF     (SPA-NAI-BANGO       =   ZERO )
               MOVE    1                   TO  SPA-GMN-CUR
               MOVE    "0013"              TO  SPA-ERRCD
               GO      TO      470-KAKUTEI-EXT
           END-IF
      *
      *    剤回数
           MOVE    ZERO                TO  SPA-DAY-IDX
           PERFORM 41012-DAY-CHK-SEC
      *
           MOVE    ZERO                TO  WRK-CNT-TUKI
      *
           MOVE    SPA-NAI-BANGO       TO  IDX-C
      *    受診歴有無のチェック
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL    (IDX         >   31  )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-GMN-DAY (IDX)   >   ZERO
      *            受診歴有無のチェック
                   PERFORM 410121-RRK-DAYCHK-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       MOVE    IDX             TO  WRK-IDX
                       MOVE    WRK-IDX         TO  SPA-DAY-IDX
                   END-IF
               END-IF
      *        上限回数のチェック
               IF     (SPA-GMN-DAY (IDX)   >   ZERO  )  AND
                      (SPA-ERRCD           =   SPACE )  AND
                      (SPA-NAI-SANTEIRRKKBN(IDX-C) NOT =   ZERO)
                   PERFORM 4702-SANTEIRRK-CHK-SEC
               END-IF
           END-PERFORM
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    WRK-IDX         TO  SPA-DAY-IDX
               GO      TO      470-KAKUTEI-EXT
           END-IF
      *    受診履歴への反映
           MOVE    SPACE               TO  WRK-ERRCD
           PERFORM 4701-RRK-ZAINUM-SEC
      *
      *    点滴エラークリア
           MOVE    ZERO                TO  SPA-J02-TENTEKI
      *
           MOVE    SPA-NAI-BANGO       TO  IDX
      *
           MOVE    ZERO                TO  IDG
           PERFORM VARYING     IDXG    FROM    1   BY  1
                   UNTIL       IDXG    >   SPA-GMN-MAX
               IF      SPA-GMN-TBLREC(IDXG)(1:3)   NOT =   "---"
                   IF      SPA-GMN-BANGO       =   SPA-GMN-TBANGO
                                                            (IDXG)
                       MOVE    IDXG                TO  IDG
                       MOVE    SPA-GMN-MAX         TO  IDXG
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE   SPA-GNAI-DAY-G       TO  SPA-NAI-DAY-AREA (IDX)
      *
           MOVE   SPA-GMN-HKNCOMBI     TO  SPA-NAI-HKNCOMBI  (IDX)
      *!   MOVE   SPA-GMN-ROUJIN       TO  SPA-NAI-ROUJIN    (IDX)
      *    組合せ
           MOVE   SPA-GMN-HKNCOMBI     TO  SPA-GMN-THKNCOMBI (IDG)
      *    回数
           MOVE    ZERO                TO  WRK-ZAIKAISU
           PERFORM VARYING    IDK      FROM    1   BY  1
                   UNTIL      IDK      >   31
               ADD     SPA-NAI-DAY  (IDX 1 IDK)
                                       TO  WRK-ZAIKAISU
               MOVE    SPA-NAI-DAY  (IDX 1 IDK)
                                       TO  WRK-ZZ
               MOVE    WRK-ZZ-X        TO  SPA-GMN-TDAY    (IDG IDK)
               IF      SPA-NAI-DAY  (IDX 1 IDK)    >=  100
                   MOVE    SPA-NAI-DAY  (IDX 1 IDK)
                                           TO  SPA-GMN-TDAY (IDG IDK)
               END-IF
           END-PERFORM
           MOVE    WRK-ZAIKAISU        TO  SPA-NAI-ZAIKAISU (IDX)
           IF      SPA-NAI-ZAIKAISU (IDX)  >=   100
               MOVE    SPA-NAI-ZAIKAISU (IDX)  TO  WRK-ZZ9
               MOVE    WRK-ZZ9-X               TO  SPA-GMN-TDAYGOK(IDG)
           ELSE
               MOVE    SPA-NAI-ZAIKAISU (IDX)   TO  WRK-Z9
               MOVE    WRK-Z9-X                 TO  SPA-GMN-TDAYGOK(IDG)
           END-IF
      *
      *    合計回数の再表示
           IF      SPA-NAIGMN-SRYKBN   =   "95"  OR  "96"
               CONTINUE
           ELSE
               PERFORM 31013-INPUTCD-HEN-SEC
               MOVE    SPA-NAI-ENDBANGO (IDX)
                                           TO  IDG 
               MOVE    WRK-TENSU-X         TO  SPA-GMN-TTENSU  (IDG)
           END-IF
      *
      *    変更項目クリア
           INITIALIZE                  SPA-GMN-INPUT
           INITIALIZE                  SPA-NAIGMN-AREA
           MOVE    ZERO                TO  SPA-J02-SYORI
      *
      *    番号選択へカーソル移動
           MOVE    1                   TO  SPA-GMN-CUR
      *
           IF      WRK-ERRCD       NOT =   SPACE
               MOVE    "K100"          TO  SPA-ERRCD
           END-IF
      *----(01.00.01) LINE ADD START ----------------------------------
      *    剤確定あり
           MOVE    "1"                 TO  SPA-UPDFLG
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
           .
       470-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴への反映処理
      *****************************************************************
       4701-RRK-ZAINUM-SEC             SECTION.
      *
           MOVE    SPA-NAI-BANGO       TO  IDX
      *
           PERFORM   VARYING   IDX-IN2         FROM    1   BY  1
                     UNTIL    (IDX-IN2         >   31  )  OR
                              (SPA-ERRCD   NOT =   SPACE)
      *ver.4.7
               MOVE    IDX-IN2             TO  SPA-DAY-IDX
               IF     (SPA-NYUGAIKBN           =   "1"   )  AND
                      (SPA-NAI-NYUIN (IDX-IN2) =   3     )
      *            同日再入院日
                   IF      SPA-GNAI-DAY (2 IDX-IN2)  >  ZERO
      *                退院日
                       MOVE    1               TO  WRK-D-RENNUM
      *                受診履歴の剤変更
                       PERFORM 410122-RRK-ZAINUM-SEC
                   END-IF
                   IF      SPA-GNAI-DAY (3 IDX-IN2)  >  ZERO
      *                入院
                       MOVE    2               TO  WRK-D-RENNUM
      *                受診履歴の剤変更
                       PERFORM 410122-RRK-ZAINUM-SEC
                   END-IF
                   IF      SPA-GNAI-DAY (1 IDX-IN2)  =  ZERO
      *                削除
                       MOVE    ZERO            TO  WRK-D-RENNUM
      *                受診履歴の剤変更
                       PERFORM 410122-RRK-ZAINUM-SEC
                   END-IF
               ELSE
      *        受診履歴の剤変更
                   MOVE    ZERO            TO  WRK-D-RENNUM
                   PERFORM 410122-RRK-ZAINUM-SEC
               END-IF
           END-PERFORM
      *
           .
       4701-RRK-ZAINUM-EXT.
           EXIT.
      *****************************************************************
      *    上限回数のチェック処理
      *****************************************************************
       4702-SANTEIRRK-CHK-SEC           SECTION.
      *
      *    月上限
           ADD     SPA-GMN-DAY (IDX)   TO  WRK-CNT-TUKI
           IF    ((SPA-NAI-JGNCNT (IDX-C)  >   ZERO  )  AND
                  (SPA-NAI-JGNCNTERR (IDX-C)   NOT =   9 )  AND
                  (SPA-NAI-JGNCNT (IDX-C)  <   WRK-CNT-TUKI))  OR
                 ((SPA-NAI-JGNCNTETC (IDX-C)   >   ZERO  )  AND
                  (SPA-NAI-JGNCNTERR (IDX-C)   NOT =   9 )  AND
                  (SPA-NAI-JGNCNTETC (IDX-C)   <   WRK-CNT-TUKI))
      *        内分泌負荷試験はエラー
               IF      SPA-NAI-HOUKNSKBN (IDX-C)  =   08
                   MOVE    "1007"              TO  SPA-ERRCD
               ELSE
                   IF      SPA-NAI-KEIFLG      =   ZERO
                       MOVE    "K003"              TO  SPA-ERRCD
                       MOVE    1                   TO  SPA-NAI-KEIFLG
                   END-IF
               END-IF
           END-IF
      *
      *    日上限
           IF     (SPA-NAI-JGNCNT1D (IDX-C)    >   ZERO  )  AND
                  (SPA-NAI-JGNCNTERR (IDX-C)   NOT =   9 )
               COMPUTE WRK-DAYCNT          =   SPA-NAI-TSURYO (IDX-C)
                                           *   SPA-GMN-DAY (IDX)
               IF      WRK-DAYCNT          >   SPA-NAI-JGNCNT1D (IDX-C)
                   IF      SPA-NAI-KEIFLG      =   ZERO
                       MOVE    "K004"              TO  SPA-ERRCD
                       MOVE    1                   TO  SPA-NAI-KEIFLG
                   END-IF
               END-IF
           END-IF
      *
      *    週上限
           IF     (SPA-NAI-JGNCNT1W (IDX-C)    >   ZERO  )  AND
                  (SPA-NAI-JGNCNTERR (IDX-C)   NOT =   9 )
            AND   (SPA-ERRCD                   =   SPACE )
            AND   (SPA-NAI-KEIFLG              =   ZERO  )
               MOVE    SPA-NAI-SRYYM       TO  WRK-CHK-SRYYMD(1:6)
               MOVE    IDX                 TO  WRK-CHK-SRYDD
               PERFORM 47021-WEEKCNT-CHEK-SEC
      *
               MOVE    ZERO                TO  WRK-WEEKCNT
               PERFORM VARYING     IDD     FROM    IDX-ST  BY  1
                       UNTIL       IDD     >   IDX-ED
                   ADD     SPA-GMN-DAY (IDD)   TO  WRK-WEEKCNT
               END-PERFORM
               IF      WRK-WEEKCNT         >   SPA-NAI-JGNCNT1W (IDX-C)
                   MOVE    "K006"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-NAI-KEIFLG
               END-IF
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    IDX             TO  WRK-IDX
               MOVE    WRK-IDX         TO  SPA-DAY-IDX
           END-IF
      *
           .
       4702-SANTEIRRK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    週上限回数チェック処理
      *****************************************************************
       47021-WEEKCNT-CHEK-SEC           SECTION.
      *
      *    曜日指定
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY3-AREA
           MOVE    "31"                TO  LNK-DAY3-IRAI
           MOVE    WRK-CHK-SRYYMD      TO  LNK-DAY3-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY3-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY3-YOBI       TO  WRK-YOBI
           ELSE
               MOVE    1                   TO  WRK-YOBI
           END-IF
      *
           IF      WRK-YOBI            =   7
      *        日曜
               MOVE    WRK-CHK-SRYYMD      TO  WRK-STR-YMD
           ELSE
      *        診療日の日曜日
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    WRK-CHK-SRYYMD      TO  LNK-DAY6-KIJUN
               MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
               COMPUTE LNK-DAY6-ZOGEN      =   WRK-YOBI   *   -1
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN     TO  WRK-STR-YMD
           END-IF
      *    土曜日
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-STR-YMD         TO  LNK-DAY6-KIJUN
           MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    6                   TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-END-YMD
      *今月分のみチェック
      *    日曜＝診療月
           IF      WRK-STR-YMD(1:6)    =   WRK-CHK-SRYYMD(1:6)
               MOVE    WRK-STR-DD          TO  IDX-ST
           ELSE
               MOVE    1                   TO  IDX-ST
           END-IF
      *    土曜＝診療月
           IF      WRK-END-YMD(1:6)    =   WRK-CHK-SRYYMD(1:6)
               MOVE    WRK-END-DD          TO  IDX-ED
           ELSE
               MOVE    31                  TO  IDX-ED
           END-IF
      *
      *
           .
       47021-WEEKCNT-CHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴の剤変更処理
      *****************************************************************
       410122-RRK-ZAINUM-SEC           SECTION.
      *
      *    対象履歴判定
           MOVE    ZERO                TO  WRK-D-IDX
           MOVE    ZERO                TO  IDX-RRK
           MOVE    ZERO                TO  WRK-RRK-RENNUM
           MOVE    ZERO                TO  WRK-RRK-DOUJI-RENNUM
           MOVE    SPACE               TO  WRK-RRK-HKNKBN
           MOVE    SPACE               TO  WRK-RRK-DRCD
           PERFORM VARYING     IDR     FROM    1   BY   1
                   UNTIL      (IDR     >   SPA-RRK-MAX  )  OR
                              (IDX-RRK     >   ZERO     )
               MOVE    SPA-RRK-RRKYMD(IDR)(7:2)    TO  IDR2
               IF     (IDR2                =   SPA-DAY-IDX    ) AND
                      (SPA-RRK-SRYKA (IDR) =   SPA-NAIGMN-SRYKA)
      *
                   MOVE    SPA-RRK-RENNUM(IDR) TO  WRK-RRK-RENNUM
      *            入院の場合、最大同時連番を取得する
                   IF     (SPA-NYUGAIKBN       =   "1" )
                       IF     (WRK-D-RENNUM        =   ZERO   )
                         AND  (SPA-RRK-DOUJI-RENNUM(IDR)
                                               >= WRK-RRK-DOUJI-RENNUM)
                           MOVE    SPA-RRK-DOUJI-RENNUM(IDR)
                                              TO  WRK-RRK-DOUJI-RENNUM
                           MOVE    IDR        TO  WRK-D-IDX
                       END-IF
                       IF    ((WRK-D-RENNUM   NOT  =   ZERO   )  AND
                              (WRK-D-RENNUM        =   SPA-RRK-RENNUM
                                                             (IDR)))
                       AND   ( SPA-RRK-DOUJI-RENNUM(IDR)
                                              >=  WRK-RRK-DOUJI-RENNUM )
                           MOVE    SPA-RRK-DOUJI-RENNUM(IDR)
                                              TO  WRK-RRK-DOUJI-RENNUM
                           MOVE    IDR        TO  WRK-D-IDX
                       END-IF
                   END-IF
               END-IF
               IF     (SPA-RRK-HKNCOMBI(IDR)   =   SPA-GMN-HKNCOMBI)
                   MOVE    SPA-RRK-HKNKBN(IDR) TO  WRK-RRK-HKNKBN
               END-IF
               IF     (SPA-RRK-SRYKA (IDR) =   SPA-NAIGMN-SRYKA) AND
                      (SPA-RRK-HKNCOMBI(IDR)   =  SPA-GMN-HKNCOMBI)
                   MOVE    SPA-RRK-DRCD (IDR)  TO  WRK-RRK-DRCD
               END-IF
      *
               IF     (IDR2                =   SPA-DAY-IDX    ) AND
                      (SPA-RRK-SRYKA (IDR) =   SPA-NAIGMN-SRYKA) AND
                      (SPA-RRK-HKNCOMBI(IDR)   =  SPA-GMN-HKNCOMBI)
                   IF     (WRK-D-RENNUM        =   ZERO   )  OR
                          (WRK-D-RENNUM        =   SPA-RRK-RENNUM
                                                             (IDR))
                       MOVE    IDR                 TO  IDX-RRK
                       PERFORM 4101220-ZAINUM-HEN-SEC
                   END-IF
               END-IF
           END-PERFORM
      *    対象がなかった時追加
           IF      IDX-RRK             =   ZERO
               IF      SPA-GNAI-DAY (1 SPA-DAY-IDX)    >   ZERO
      *            入院調剤料の自動発生分は追加対象外
                   IF     (SPA-NAI-NYUAUTO (IDX)   =   1    )
                   AND   ((SPA-GNAI-DAY (1 SPA-DAY-IDX)
                                                   =   SPA-NAI-DAY
                                                    (IDX 1 SPA-DAY-IDX))
                     OR   (SPA-NAI-DAY (IDX 1 SPA-DAY-IDX)
                                                   =   ZERO  ))
                       CONTINUE
                   ELSE
                       PERFORM 41001221-RRK-ADD-SEC
                   END-IF
               END-IF
           ELSE
               IF     (SPA-RRK-ADD (IDX-RRK)   =   "1" )  AND
                      (SPA-RRK-ZAINUM-G(IDX-RRK)   =   ZERO )
      *            剤がなくなった場合
                   PERFORM 41001222-RRK-DEL-SEC
               END-IF
           END-IF
      *
           .
       410122-RRK-ZAINUM-EXT.
           EXIT.
      *            
      *****************************************************************
      *    回数より受診履歴編集処理
      *****************************************************************
       4101220-ZAINUM-HEN-SEC             SECTION.
      *
      **** PERFORM VARYING     IDR4    FROM    2   BY   1
      *            UNTIL       IDR4    >   4
      ****     IF     (SPA-GNAI-DAY (IDR4 SPA-DAY-IDX)
      *                                NOT =
      ****             SPA-NAI-DAY (IDX IDR4 SPA-DAY-IDX) )  AND
           PERFORM VARYING     IDR4    FROM    2   BY   1
      ************ UNTIL       IDR4    >   4
                   UNTIL       IDR4    >   10
               IF     (SPA-GNAI-DAY (IDR4 SPA-DAY-IDX)
                                           >   ZERO  )  OR
                      (SPA-NAI-DAY (IDX IDR4 SPA-DAY-IDX)
                                           >   ZERO  )
                   MOVE    ZERO            TO  IDX-RRK2
                   MOVE    ZERO            TO  IDX-RRK3
                   MOVE    ZERO            TO  IDR6
                   PERFORM VARYING IDR5    FROM    IDX-RRK   BY   1
                           UNTIL  (IDR5        >   SPA-RRK-MAX )  OR
                                  (IDX-RRK2    >   ZERO        )
                       IF     (SPA-RRK-RRKYMD(IDX-RRK)
                                           =   SPA-RRK-RRKYMD(IDR5)) AND
                              (SPA-RRK-SRYKA   (IDR5)
                                           =   SPA-NAIGMN-SRYKA) AND
                              (SPA-RRK-HKNCOMBI(IDR5)
                                           =   SPA-GMN-HKNCOMBI )
                           MOVE    SPA-RRK-RENNUM (IDR5)
                                                       TO  WRK-REN
                           ADD     1               TO  WRK-REN
      *                    剤番号存在判定
                           IF      WRK-REN             =   IDR4
                               MOVE    IDR5            TO  IDX-RRK2
                               PERFORM 41012202-JYURRK-ZAINUM-SEC
                           ELSE
                               IF      IDR6            =   ZERO
                                   MOVE    IDR5            TO  IDR6
                               END-IF
                           END-IF
                       END-IF
      *
                       IF      SPA-RRK-RRKYMD(IDX-RRK)
                                           NOT =   SPA-RRK-RRKYMD(IDR5)
                           MOVE    SPA-RRK-MAX     TO  IDR5
                       END-IF
                   END-PERFORM
      *ver.4.7
      *            入院対象の受診履歴がない時
                   IF     (IDX-RRK2            =   ZERO )
                      AND (WRK-D-RENNUM    NOT =   ZERO )
                       CONTINUE
                   ELSE
      *
                   IF      IDX-RRK2            =   ZERO
                       MOVE    IDR6            TO  IDX-RRK2
                   END-IF
                   MOVE    SPA-RRK-RENNUM (IDX-RRK2)
                                               TO  WRK-REN
                   ADD     1                   TO  WRK-REN
      *
                   MOVE    SPA-GNAI-DAY (IDR4 SPA-DAY-IDX)
                                               TO  WRK-DAY-S
                   IF      IDR4            NOT =   WRK-REN
                       ADD     WRK-DAY-S       TO  SPA-GNAI-DAY
                                                 (WRK-REN SPA-DAY-IDX)
                       MOVE    ZERO            TO  SPA-GNAI-DAY
                                                 (IDR4 SPA-DAY-IDX)
                       MOVE    WRK-REN         TO  IDR4
                   END-IF
                   MOVE    SPA-NAI-DAY (IDX IDR4 SPA-DAY-IDX)
                                               TO  WRK-DAY-MAE
      *            対象剤判定
                   PERFORM 41012202-JYURRK-ZAINUM-SEC
      *
                   IF      WRK-IDR             =   ZERO
                       IF      WRK-DAY-S           >   ZERO
      *                   入院調剤料の自動発生分は追加対象外
                       AND    (SPA-NAI-NYUAUTO (IDX)   =   ZERO )
      *                    剤追加
                           ADD     1               TO  WRK-IDR-MAX
                           IF      WRK-IDR-MAX     <=  135
                               MOVE    SPA-GMN-ZAINUM
                                                   TO  SPA-RRK-ZAINUM
                                                 (IDX-RRK2 WRK-IDR-MAX)
                               MOVE    SPA-NAIGMN-SRYKBN
                                                   TO  SPA-RRK-SRYKBN
                                                 (IDX-RRK2 WRK-IDR-MAX)
                           ELSE
                               MOVE    "K100"      TO  WRK-ERRCD
                           END-IF
                       END-IF
                   ELSE
                       IF      WRK-DAY-S           =   ZERO
      *                    剤削除
                           MOVE    ZERO            TO  SPA-RRK-ZAINUM
                                                 (IDX-RRK2 WRK-IDR)
                           MOVE    SPACE           TO  SPA-RRK-SRYKBN
                                                 (IDX-RRK2 WRK-IDR)
                       ELSE
      *                    剤変更
                           IF      WRK-DAY-S   NOT =   WRK-DAY-MAE
                               MOVE    "1"         TO  SPA-RRK-UPD
                                                             (IDX-RRK2)
                           END-IF
                       END-IF
                   END-IF
      *ver.4.7
                   END-IF
      *ver.4.7
      *
      *H26.11
               ELSE
      *            回数ゼロで受診履歴に存在する時、削除
                   IF     (SPA-GNAI-DAY (IDR4 SPA-DAY-IDX)
                                           =   ZERO  )  AND
                          (SPA-NAI-DAY (IDX IDR4 SPA-DAY-IDX)
                                           =   ZERO  )
                       PERFORM 41012203-RRKZAINUM-DEL-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       4101220-ZAINUM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴テーブル判定処理
      *****************************************************************
       41012202-JYURRK-ZAINUM-SEC             SECTION.
      *
      *    対象剤判定
           MOVE    ZERO                TO  WRK-IDR
           MOVE    ZERO                TO  WRK-IDR-MAX
           PERFORM VARYING     IDR3    FROM    1   BY   1
                   UNTIL       IDR3    >   135
               IF      SPA-RRK-ZAINUM (IDX-RRK2 IDR3)
                                           =   SPA-GMN-ZAINUM
                    MOVE    IDR3           TO  WRK-IDR
               END-IF
               IF      SPA-RRK-ZAINUM (IDX-RRK2 IDR3)
                                       NOT =  ZERO
                   MOVE    IDR3            TO  WRK-IDR-MAX
               END-IF
           END-PERFORM
           .
       41012202-JYURRK-ZAINUM-EXT.
           EXIT.
      *
      *H26.11
      *****************************************************************
      *    受診履歴剤番号削除処理
      *****************************************************************
       41012203-RRKZAINUM-DEL-SEC             SECTION.
      *
           MOVE    ZERO            TO  IDX-RRK2
           MOVE    ZERO            TO  IDX-RRK3
           MOVE    ZERO            TO  IDR6
           PERFORM VARYING IDR5    FROM    IDX-RRK   BY   1
                   UNTIL  (IDR5        >   SPA-RRK-MAX )  OR
                          (IDX-RRK2    >   ZERO        )
               IF     (SPA-RRK-RRKYMD(IDX-RRK)
                                           =   SPA-RRK-RRKYMD(IDR5)) AND
                      (SPA-RRK-SRYKA   (IDR5)
                                           =   SPA-NAIGMN-SRYKA  ) AND
                      (SPA-RRK-HKNCOMBI(IDR5)
                                           =   SPA-GMN-HKNCOMBI )
                   MOVE    SPA-RRK-RENNUM (IDR5)   TO  WRK-REN
                   ADD     1                       TO  WRK-REN
                   IF      WRK-REN             =   IDR4
                       MOVE    IDR5            TO  IDX-RRK2
                       PERFORM 41012202-JYURRK-ZAINUM-SEC
      *                剤番号存在判定
                       IF      WRK-IDR         >   ZERO
      *                    剤削除
                           MOVE    ZERO            TO  SPA-RRK-ZAINUM
                                                 (IDX-RRK2 WRK-IDR)
                           MOVE    SPACE           TO  SPA-RRK-SRYKBN
                                                 (IDX-RRK2 WRK-IDR)
                       END-IF
                   END-IF
               END-IF
      *
               IF      SPA-RRK-RRKYMD(IDX-RRK)
                                           NOT =   SPA-RRK-RRKYMD(IDR5)
                   MOVE    SPA-RRK-MAX     TO  IDR5
               END-IF
           END-PERFORM
           .
       41012203-RRKZAINUM-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴テーブル追加処理
      *****************************************************************
       41001221-RRK-ADD-SEC             SECTION.
      *
           IF      SPA-RRK-MAX         >=  100
               MOVE    "0024"          TO  SPA-ERRCD
               GO      TO      41001221-RRK-ADD-EXT
           END-IF
      *    保険区分設定
           IF      WRK-RRK-HKNKBN      =   SPACE
               PERFORM 410012211-RRK-HKNKBN-SEC
           END-IF
      *
           ADD     1                   TO  SPA-RRK-MAX
           MOVE    SPA-RRK-MAX         TO  IDR
           MOVE    SPA-NAI-SRYYM       TO  SPA-RRK-RRKYMD(IDR)(1:6)
           MOVE    SPA-DAY-IDX         TO  SPA-RRK-RRKYMD(IDR)(7:2)
           MOVE    SPA-NAIGMN-SRYKA    TO  SPA-RRK-SRYKA (IDR)
           MOVE    SPA-GMN-HKNCOMBI    TO  SPA-RRK-HKNCOMBI (IDR)
           MOVE    WRK-RRK-HKNKBN      TO  SPA-RRK-HKNKBN   (IDR)
           IF      WRK-D-RENNUM        =   ZERO
               ADD     1                   TO  WRK-RRK-RENNUM
               IF     (SPA-NYUGAIKBN       =   "1"  )
                   MOVE    1                   TO  WRK-RRK-RENNUM
               END-IF
           ELSE
               MOVE    WRK-D-RENNUM        TO  WRK-RRK-RENNUM
           END-IF
           MOVE    WRK-RRK-RENNUM      TO  SPA-RRK-RENNUM   (IDR)
           MOVE    ZERO                TO  SPA-RRK-DOUJI-RENNUM(IDR)
           MOVE    WRK-RRK-DRCD        TO  SPA-RRK-DRCD  (IDR)
      *
      *    入院の場合、同時連番を連番とする
           IF     (SPA-NYUGAIKBN       =   "1"  )
               IF      WRK-D-IDX           >   ZERO
                   MOVE    SPA-RRK-DOUJI-RENNUM(WRK-D-IDX)
                                           TO  WRK-RRK-DOUJI-RENNUM
                   ADD     1               TO  WRK-RRK-DOUJI-RENNUM
               ELSE
                   MOVE    ZERO            TO  WRK-RRK-DOUJI-RENNUM
               END-IF
               MOVE    WRK-RRK-DOUJI-RENNUM
                                           TO  SPA-RRK-DOUJI-RENNUM(IDR)
           END-IF
      *
      *    連番が１以上の時、回数テーブルの判定
           IF     (SPA-RRK-RENNUM   (IDR)  >   1   )
             AND  (SPA-NAI-NYUIN (SPA-DAY-IDX) NOT =   3 )
               MOVE    SPA-RRK-RENNUM   (IDR)  TO  IDD3
               ADD     1                       TO  IDD3
      *v4.8    IF      IDD3                    >   4
      *            MOVE    4                   TO  IDD3
      *        END-IF
      *        MOVE    ZERO                TO  SPA-GNAI-DAY
      *                                              (2 SPA-DAY-IDX)
      *        MOVE    ZERO                TO  SPA-GNAI-DAY
      *                                              (3 SPA-DAY-IDX)
      *        MOVE    ZERO                TO  SPA-GNAI-DAY
      ********                                       (4 SPA-DAY-IDX)
               PERFORM VARYING     IDXD    FROM    2   BY   1
                       UNTIL      (IDXD    >   10  )
                   MOVE    ZERO            TO  SPA-GNAI-DAY
                                                  (IDXD SPA-DAY-IDX)
               END-PERFORM
               MOVE    SPA-GNAI-DAY (1 SPA-DAY-IDX)
                                           TO  SPA-GNAI-DAY
                                                     (IDD3 SPA-DAY-IDX)
           END-IF
      *
           MOVE    1                   TO  SPA-RRK-KAIKEI-RENNUM(IDR)
           MOVE    ZERO                TO  SPA-RRK-DENPNUM (IDR)
           MOVE    SPA-GMN-ZAINUM      TO  SPA-RRK-ZAINUM  (IDR 1)
           MOVE    SPA-NAIGMN-SRYKBN   TO  SPA-RRK-SRYKBN  (IDR 1)
           MOVE    "1"                 TO  SPA-RRK-ADD     (IDR)
      *    画面表示用
           MOVE    IDR                 TO  SPA-GMN-RRKNO (IDR)
           MOVE    SPA-RRK-RRKYMD(IDR) TO  WRK-SYMD
           PERFORM 41012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-RRKYMD (IDR)
           IF     (SPA-RRK-RENNUM (IDR)    >   1 ) OR
                  (SPA-RRK-DOUJI-RENNUM (IDR)  >  ZERO )
               MOVE    1                   TO  FLG-HEN
           END-IF
           IF      FLG-HEN             =   1
               MOVE    "("                 TO  SPA-GMN-RRKYMD (IDR)
                                                               (10:1)
               MOVE    SPA-RRK-RENNUM (IDR)    TO  SPA-GMN-RRKYMD (IDR)
                                                              (11:1)
               MOVE    ")"                 TO  SPA-GMN-RRKYMD (IDR)
                                                              (12:1)
               IF      SPA-RRK-DOUJI-RENNUM (IDR)  NOT =   ZERO
                   MOVE    SPA-RRK-DOUJI-RENNUM (IDR) 
                                               TO  SPA-GMN-RRKYMD (IDR)
                                                              (13:1)
               END-IF
           END-IF
           MOVE    SPA-GMN-BSRYKAMEI   TO  SPA-GMN-RRKSRYKA (IDR)
           MOVE    SPA-GMN-HKNCOMBI    TO  SPA-GMN-RRKHKNCOMBI (IDR)
      *
           .
       41001221-RRK-ADD-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴保険区分設定処理
      *****************************************************************
       410012211-RRK-HKNKBN-SEC             SECTION.
      *
           PERFORM VARYING     IDH     FROM    1   BY  1
                   UNTIL       IDH     >   SPA-HKN-MAX
               IF      SPA-GMN-THKNCOMBINUM (IDH)
                                               =   SPA-GMN-HKNCOMBI
                   MOVE    SPA-NAI-THKNKBN (IDH)   TO  WRK-RRK-HKNKBN
                   IF     (SPA-NAI-THKNKBN (IDH)   =   "1" )
      *                自賠責
                       IF     (SPA-NAI-TRSI-HKNKBN (IDH)   =   "4")
                           MOVE    "2"             TO  WRK-RRK-HKNKBN
                           IF      SPA-NAI-TRSI-JUNKYO(IDH)   =   "1"
      *                        自賠責(健保準拠）
                               MOVE    "4"         TO  WRK-RRK-HKNKBN
                           END-IF
                       END-IF
      *                公務災害
                       IF     (SPA-NAI-TRSI-HKNKBN (IDH)   =   "5")
                           MOVE    "1"             TO  WRK-RRK-HKNKBN
                           IF      SPA-NAI-TRSI-JUNKYO(IDH)   =   "1"
      *                        公務災害(健保準拠）
                               MOVE    "5"         TO  WRK-RRK-HKNKBN
                           END-IF
                       END-IF
      *H25.7
      *                第三者行為
                       IF     (SPA-NAI-TRSI-HKNKBN (IDH)   =   "6")
                           MOVE    "7"             TO  WRK-RRK-HKNKBN
                       END-IF
                   END-IF
                   MOVE    SPA-HKN-MAX             TO  IDH
               END-IF
           END-PERFORM
           .
       410012211-RRK-HKNKBN-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴テーブル削除処理
      *****************************************************************
       41001222-RRK-DEL-SEC             SECTION.
      *
           PERFORM VARYING     IDR     FROM    IDX-RRK BY   1
                   UNTIL       IDR     >   100
               IF      IDR             =   100
                   INITIALIZE              SPA-NAI-RRK-LIST (IDR)
                   INITIALIZE              SPA-GMN-RRK-LIST (IDR)
               ELSE
                   MOVE    SPA-GMN-RRK-LIST (IDR + 1)
                                       TO  SPA-GMN-RRK-LIST (IDR)
                   MOVE    SPA-NAI-RRK-LIST (IDR + 1)
                                       TO  SPA-NAI-RRK-LIST (IDR)
               END-IF
           END-PERFORM
           COMPUTE SPA-RRK-MAX         =   SPA-RRK-MAX   -   1
      *
           .
       41001222-RRK-DEL-EXT.
           EXIT.
      *            
      *****************************************************************
      *    氏名検索へ
      *****************************************************************
       470-SIMEIKEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-J02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "J02"               TO  SPA-MOTOPG
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
      *
      *****************************************************************
      *    予約登録へ
      *****************************************************************
       480-YOYAKU-SEC             SECTION.
      *
      *----(01.00.01) LINE ADD START ----------------------------------
      *    剤修正のチェック
           IF      SPA-GMN-BANGO   NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
               GO      TO      480-YOYAKU-EXT
           END-IF
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
      *
      *    戻り時、画面を元へ戻す
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-J02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "J02"               TO  SPA-MOTOPG
           MOVE    "J02"               TO  SPA-MOTOPG3
           MOVE    "Y01"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
      **** MOVE    "JOIN"              TO  MCP-PUTTYPE.
           MOVE   "Y01"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       480-YOYAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付一覧へ
      *****************************************************************
       490-UKEICHI-SEC             SECTION.
      *
      *    保存用パラメタ作成
           PERFORM 4201-SRYACCT-PARA-SEC
      *
      *    戻り時、画面を元へ戻す
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-J02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "J02"               TO  SPA-MOTOPG
           MOVE    "U01"               TO  SPA-SAKIPG
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
      **** MOVE    "JOIN"              TO  MCP-PUTTYPE.
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
      *   画面移行用のパラメタを作成
      *****************************************************************
       4430-SPA-OUT-SEC                SECTION.
      *
           .
       4430-SPA-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録　処理
      *****************************************************************
       490-TOROKU-SEC              SECTION.
      *
      *    患者選択チェック
           IF      SPA-GMN-PTID        =   ZERO
               MOVE    "0019"          TO  SPA-ERRCD
               GO      TO      490-TOROKU-EXT
           END-IF
      *
      *    更新前のチェック
           IF      SPA-GMN-BANGO   NOT =   ZERO
      *----(01.00.01) LINE UPD START ----------------------------------
      ******** MOVE    "0101"          TO  SPA-JIDCD
               MOVE    "0009"          TO  SPA-ERRCD
      *----(01.00.01) LINE UPD END   ----------------------------------
               GO      TO      490-TOROKU-EXT
           END-IF
      *
      *    診療会計マスター更新
           PERFORM 4901-SRYACCT-UPD-SEC
           IF     (SPA-ERRCD       NOT =   SPACE ) OR
                  (SPA-JIDCD       NOT =   SPACE ) OR
                  (FLG-END             =   1     )
               GO      TO       490-TOROKU-EXT
           END-IF
      *
      *    患者が選択されてきた時は、元の画面へ
           IF      SPA-J02-MOTOFLG     =   "0"
      *        排他制御解除処理
               PERFORM 990-LOCK-OUT-SEC
      *
               MOVE    ZERO            TO  SPA-GMN-CUR
               INITIALIZE                  SPA-J02-AREA
               INITIALIZE                  SPA-SCR-REC
               INITIALIZE                  SPA-J02-SET
               INITIALIZE                  SPA-GMN-NYUIN-AREA
               MOVE    SPA-J02-SRYYMDW(1:6)    TO  SPA-GMN-SRYYM
               MOVE    SPA-J02-SRYYMD (1:6)    TO  SPA-NAI-SRYYM
               MOVE    SPACE               TO  SPA-GMN-SRYKA-G
           ELSE
      *        更新後、戻るへ
               PERFORM 210-BACK
           END-IF
           .
       490-TOROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスター更新処理
      *****************************************************************
       4901-SRYACCT-UPD-SEC              SECTION.
      *
      *    変更がない時、処理なし
           IF      SPA-UPDFLG          =   "1"
               CONTINUE
           ELSE
               GO      TO      4901-SRYACCT-UPD-EXT
           END-IF
      *
      *----(01.00.01) LINE ADD START ----------------------------------
           MOVE    SPACE               TO  SPA-UPDFLG
      *----(01.00.01) LINE ADD END   ----------------------------------
      *    対象がない時、処理なし
           IF      SPA-NAI-MAX         =   ZERO
               GO      TO      4901-SRYACCT-UPD-EXT
           END-IF
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           IF      SPA-NAI-NAIBUNCHK   =   ZERO
               PERFORM 49010-KOUSIN-SYORI-SEC
           ELSE
               MOVE    SPACE           TO  SPA-NAI-NAIBUNERR
           END-IF
      *    収納変更時、再計算
           IF     (SPA-NAI-NAIBUNERR   =   SPACE )  AND
                  (SPA-ERRCD           =   SPACE )
               IF      SPA-SYUNOU-KOUSIN   =   1
                   PERFORM 49012-SYUNOU-SAIKEISAN-SEC
               END-IF
           END-IF
      *
           .
       4901-SRYACCT-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスター  変更処理
      *****************************************************************
       49010-KOUSIN-SYORI-SEC           SECTION.
      *
      *    内分泌負荷試験
           MOVE    ZERO                TO  FLG-NAIBUN
           MOVE    SPACE               TO  SPA-NAI-NAIBUNERR
           MOVE    ZERO                TO  SPA-NAI-NAIBUNCHK
      *
      *    診療会計マスターを編集
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL     (IDX      >   SPA-NAI-MAX )  OR
                             (SPA-ERRCD    NOT =   SPACE )
               INITIALIZE                      SRYACCT-REC
               MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
               MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
               MOVE    SPA-GMN-PTID        TO  ACCT-PTID
      *********MOVE    SPA-J01-SRYKA       TO  ACCT-SRYKA
               MOVE    SPA-NAI-SRYKA (IDX) TO  ACCT-SRYKA
               MOVE    SPA-NAI-SRYYM       TO  ACCT-SRYYM
               MOVE    SPA-NAI-SRYKBN(IDX) TO  ACCT-SRYKBN
               MOVE    SPA-NAI-ZAINUM(IDX) TO  ACCT-ZAINUM
      *
               MOVE    SRYACCT-REC         TO  MCPDATA-REC
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 900-SRYACCT-READ-SEC
               ELSE
                   INITIALIZE                  SRYACCT-REC
                   MOVE    1               TO  FLG-SRYACCT
               END-IF
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
               IF      FLG-SRYACCT          =   ZERO
      *            診療会計マスターを変更する
                   PERFORM 4901-SRYACCT-KOUSIN-SEC
      *
                   MOVE    SMCNDATE-YMD        TO  ACCT-UPYMD
                   MOVE    SMCNDATE-HMS        TO  ACCT-UPHMS
                   MOVE    SPA-OPID            TO  ACCT-OPID
      *
                   MOVE    SRYACCT-REC         TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                        DISPLAY "J02 SRYACCT UPD ERR:" MCP-RC
                        MOVE    "2000"          TO  SPA-ERRCD
                   END-IF
               ELSE
                   DISPLAY "J02 SRYACCT READ ERR:" ACCT-KEY
               END-IF
      *
           END-PERFORM
      *
           IF     (FLG-NAIBUN          =   1   ) AND
                  (SPA-ERRCD           =   SPACE)
      *        内分泌負荷試験包括点数チェック
               INITIALIZE                  ORCSSANFUKAAREA
               INITIALIZE                  LNK-K02SCR-REC
               MOVE    3                   TO  LNK-SANFUKA-SYORI
               MOVE    "J02"               TO  LNK-SANFUKA-PG
               MOVE    SPA-NAI-SRYYM       TO  LNK-SANFUKA-SRYYMD(1:6)
               MOVE    "01"                TO  LNK-SANFUKA-SRYYMD(7:2)
               CALL    "ORCSSANFUKA"       USING
                                           ORCSSANFUKAAREA
                                           SPA-AREA
                                           LNK-K02SCR-REC
               IF      LNK-SANFUKA-ERRMSG  NOT =   SPACE
                   MOVE    LNK-SANFUKA-ERRMSG  TO  SPA-NAI-NAIBUNERR
               END-IF
           END-IF
      *
      *
      *    受診履歴の更新
           IF      SPA-ERRCD           =   SPACE
               PERFORM 49011-JYURRK-UPD-SEC
           END-IF
      *R01.5
      *    エラーの時、ＤＢ異常終了とする（診療会計が残る為）
           IF     (SPA-ERRCD       NOT =   SPACE)
               MOVE    SPACE               TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "abort"             TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               GO  TO         49010-KOUSIN-SYORI-EXT
           END-IF
      *
      *    診療科履歴の初診算定日修正処理
           IF      SPA-ERRCD           =   SPACE
      *    全科指定の時、対象の科すべて
             IF      SPA-J01-SRYKA       =   ZERO
               MOVE    SPA-SRYKA           TO  WRK-SRYKA
               PERFORM VARYING    IDX      FROM    1   BY  1
                       UNTIL      IDX      >   SPA-NAI-MAX
                   IF    ( IDX             =   1  )  OR
                         ((SPA-NAI-SRYKA (IDX)
                                       NOT =   SPACE  AND ZERO ) AND
                          (SPA-NAI-SRYKA (IDX)
                                       NOT =   SPA-NAI-SRYKA (IDX - 1)))
                       INITIALIZE                  ORCSKARRKAREA
                       MOVE    SPA-GMN-PTID        TO  SPA-PTID
                       MOVE    SPA-NAI-SRYYM       TO  SKARRK-SRYYM
                       MOVE    SPA-NAI-SRYKA (IDX)
                                                   TO  SKARRK-SRYKA
                      IF     (SPA-NYUGAIKBN       =   "1"  )  AND
                             (TBL-SRYKA (IDX)     =   SPA-NAI-NYUINKA)
                      AND    (SPA-NAI-TAIINYMD    NOT =   "99999999"
                                                  AND  SPACE    )
                           MOVE    SPA-NAI-TAIINYMD
                                                  TO  SKARRK-LASTYMD
                       END-IF
                       CALL    "ORCSKARRK"         USING
                                                   ORCSKARRKAREA
                                                   SPA-AREA
                       IF      SKARRK-RC       NOT =   ZERO
                           MOVE    "0010"              TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-PERFORM
               MOVE    WRK-SRYKA           TO  SPA-SRYKA
               IF      SKARRK-ERRMSG   NOT =   SPACE
                   MOVE    "0010"              TO  SPA-ERRCD
               END-IF
           ELSE
               INITIALIZE                  ORCSKARRKAREA
               MOVE    SPA-GMN-PTID        TO  SPA-PTID
               MOVE    SPA-NAI-SRYYM       TO  SKARRK-SRYYM
               MOVE    SPA-J01-SRYKA       TO  SKARRK-SRYKA
               IF     (SPA-NYUGAIKBN       =   "1"  )  AND
                      (SPA-J01-SRYKA       =   SPA-NAI-NYUINKA) AND
                      (SPA-NAI-TAIINYMD    NOT =   "99999999"
                                               AND  SPACE    )
                   MOVE    SPA-NAI-TAIINYMD    TO  SKARRK-LASTYMD
               END-IF
               CALL    "ORCSKARRK"         USING
                                           ORCSKARRKAREA
                                           SPA-AREA
      *
               IF      SKARRK-ERRMSG   NOT =   SPACE
                   MOVE    "0010"              TO  SPA-ERRCD
               END-IF
             END-IF
      *
           END-IF
      *
      *    点滴チェック
           IF      SPA-GMN-NYUGAIKBN   =   "1"
      *        点滴一括変更処理
               IF      SPA-ERRCD       =   SPACE
                   PERFORM 42001-TENTEKI-SYORI-SEC
               END-IF
           END-IF
      *!
           IF      SPA-ERRCD           =   SPACE
      *        主科設定
               PERFORM 410029-ORCSSYUKAMAIN-SEC
           END-IF
      *
      *    内分泌負荷試験エラー判定
           IF      SPA-ERRCD           =   SPACE
               IF      (SPA-NAI-NAIBUNERR  NOT =   SPACE )  AND
                       (SPA-NAI-NAIBUNCHK      =   ZERO )
                   MOVE    1               TO  SPA-NAI-NAIBUNCHK
                   MOVE    "0120"          TO  SPA-JIDCD
               END-IF
           END-IF
      *
           .
       49010-KOUSIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスター  変更処理
      *****************************************************************
       4901-SRYACCT-KOUSIN-SEC           SECTION.
      *
      *    回数変更時、算定履歴を変更する
           IF      SPA-NAI-DAY-AREA (IDX)  NOT =   ACCT-DAY-AREA
               PERFORM 4902-SANTEI-KOUSIN-SEC
           END-IF
      *
      *    保険組合せ
           MOVE    SPA-NAI-HKNCOMBI(IDX)   TO  ACCT-HKNCOMBI
      *    剤回数
           MOVE    SPA-NAI-ZAIKAISU(IDX)   TO  ACCT-ZAIKAISU
      *    回数テーブル（１日から３１日）
           MOVE    SPA-NAI-DAY-AREA (IDX)  TO  ACCT-DAY-AREA
           .
       4901-SRYACCT-KOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴の更新処理
      *****************************************************************
       49011-JYURRK-UPD-SEC           SECTION.
      *
           MOVE    ZERO                TO  SPA-SYUNOU-KOUSIN
           MOVE    ZERO                TO  IDG
      *
           MOVE    ZERO                TO  WRK-PARA-EDANUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-RRK-MAX )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               PERFORM 49011-JYURRK-PARAUPD-SEC
           END-PERFORM
      *D   収納変更時、再計算
      *    IF      SPA-SYUNOU-KOUSIN   =   1
      *        PERFORM 49012-SYUNOU-SAIKEISAN-SEC
      *D   END-IF
      *
           .
       49011-JYURRK-UPD-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴の更新処理
      *****************************************************************
       49011-JYURRK-PARAUPD-SEC           SECTION.
      *
           MOVE    1                   TO  IDR
           MOVE    ZERO                TO  FLG-JYURRK-ARI
      *
      *    受診歴を検索する  医療機関、患者ＩＤ、伝票番号
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-RRK-SRYKA (IDX) TO  JYURRK-SRYKA
      **** IF      SPA-NYUGAIKBN       =   "2"
      *        外来の時、伝票番号
      *        MOVE    SPA-RRK-DENPNUM(IDX)
      *                                    TO  JYURRK-DENPNUM
      *
      *        MOVE    "key6"              TO  WRK-ORC-DBPATH
      **** ELSE
      *H24.5   伝票番号重複対応（伝票番号検索廃止）
      *        入院の時、診療年月日、保険組合せ
               MOVE    SPA-RRK-RRKYMD-MAE(IDX)
                                           TO  JYURRK-SRYYMD
               MOVE    SPA-RRK-RENNUM(IDX)
                                           TO  JYURRK-RENNUM
               MOVE    SPA-RRK-HKNCOMBI(IDX)
                                           TO  JYURRK-HKNCOMBINUM
               MOVE    "key19"             TO  WRK-ORC-DBPATH
      **** END-IF
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           MOVE    ZERO                TO  IDX3
           MOVE    ZERO                TO  IDX-T
           INITIALIZE                  TBL-JYURRK-AREA
           INITIALIZE                  TBL-MAE-ZAINUM-AREA
           PERFORM UNTIL  (FLG-JYURRK      =   1  )
                     OR   (IDX-T           >=  100)
      *        外来の時、伝票番号の一致のチェック
           IF      (JYURRK-NYUGAIKBN     =  "2"  )  AND
                   (SPA-RRK-DENPNUM(IDX) NOT =  ZERO )  AND
                   (SPA-RRK-DENPNUM(IDX) NOT =  JYURRK-DENPNUM )
               CONTINUE
           ELSE
      *
               ADD     1                   TO  IDX-T
               MOVE    JYURRK-REC          TO  TBL-JYURRK-REC(IDX-T)
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2    >   15  )  OR
                                  (IDX3    >=  135 )
                   IF      JYURRK-ZAINUM (IDX2)    NOT =   ZERO
                       ADD     1               TO  IDX3
                       MOVE    JYURRK-ZAINUM (IDX2)
                                               TO  TBL-MAE-ZAINUM (IDX3)
                   END-IF
               END-PERFORM
      *
           END-IF
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    剤変更チェック
           MOVE    ZERO                TO  FLG-ERR
           PERFORM VARYING     IDR     FROM    1   BY  1
                   UNTIL      (IDR     >   135  )  OR
                              (FLG-ERR =   1    )
               IF      SPA-RRK-ZAINUM (IDX IDR)    NOT =   ZERO
                   MOVE    ZERO                TO  FLG-OK
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL      (IDX2    >   135  )  OR
                                      (FLG-OK      =   1 )
                       IF      SPA-RRK-ZAINUM (IDX IDR)
                                               =   TBL-MAE-ZAINUM (IDX2)
                           MOVE    ZERO        TO  TBL-MAE-ZAINUM (IDX2)
                           MOVE    1           TO  FLG-OK
                       END-IF
                   END-PERFORM
                   IF      FLG-OK              =   ZERO
                       MOVE    1               TO  FLG-ERR
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      FLG-ERR             =   ZERO
               IF     (TBL-MAE-ZAINUM-AREA =   ZERO ) AND
                      (SPA-RRK-UPD (IDX)   =   SPACE)
      *            剤がすべて同じ時、変更なし
                   GO      TO      49011-JYURRK-PARAUPD-EXT
               END-IF
           END-IF
           MOVE    1                   TO  IDR
           PERFORM VARYING     IDX3    FROM    1   BY  1
                   UNTIL      (IDX3    >   IDX-T   )   OR
                              (SPA-ERRCD   NOT =   SPACE )
               MOVE    TBL-JYURRK-REC(IDX3)    TO  JYURRK-REC
      *        剤番号変更
               INITIALIZE                  JYURRK-ZAINUM-G
               INITIALIZE                  JYURRK-SRYKBN-G
               MOVE    ZERO                TO  IDX2
               PERFORM
                       UNTIL      (IDX2    >=  15  )   OR
                                  (IDR     >   135 )
                   MOVE    ZERO                TO  FLG-OK
                   PERFORM
                           UNTIL  (FLG-OK      =   1   )  OR
                                  (IDR         >   135 )
                       IF      SPA-RRK-ZAINUM (IDX IDR)    =   ZERO
                           CONTINUE
                       ELSE
                           ADD     1               TO  IDX2
                           MOVE    SPA-RRK-ZAINUM (IDX IDR)
                                                   TO  JYURRK-ZAINUM
                                                              (IDX2)
                           MOVE    ZERO            TO  SPA-RRK-ZAINUM
                                                            (IDX IDR)
      *                    診療区分領域の編集
                           PERFORM 4901122-JYURRK-SRYKBN-SEC
                           MOVE    1               TO  FLG-OK
                       END-IF
                       ADD     1                   TO  IDR
                   END-PERFORM
               END-PERFORM
               IF     (IDX2                =   ZERO ) AND
                      (JYURRK-EDANUM       >   1    )
                   PERFORM 490112-JYURRK-DEL-SEC
               ELSE
                   PERFORM 490113-JYURRK-UPD-SEC
               END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      49011-JYURRK-PARAUPD-EXT
           END-IF
      *
      *    受診履歴の追加判定
           IF     (SPA-RRK-ADD (IDX)   =   "1" )  OR
                  (IDX-T               =   ZERO )
               PERFORM 4901121-JYURRK-ADDHEN-SEC
               GO      TO      49011-JYURRK-PARAUPD-EXT
           END-IF
      *
           MOVE    IDR                 TO  IDX2
           MOVE    ZERO                TO  IDR3
           MOVE    TBL-JYURRK-REC(IDX-T)
                                       TO  JYURRK-REC
           INITIALIZE                  JYURRK-ZAINUM-G
           INITIALIZE                  JYURRK-SRYKBN-G
           PERFORM VARYING     IDR     FROM    IDX2   BY  1
                   UNTIL      (IDR     >   135           )
                         OR   (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-RRK-ZAINUM (IDX IDR) NOT=   ZERO
      *            受診履歴の追加
                   ADD     1               TO  IDR3
                   IF      IDR3            >   15
                       PERFORM 490111-JYURRK-INS-SEC
                       MOVE    1               TO  IDR3
                   END-IF
      *
                   MOVE    SPA-RRK-ZAINUM (IDX IDR)
                                           TO  JYURRK-ZAINUM (IDR3)
      *            診療区分領域の編集
                   PERFORM 4901122-JYURRK-SRYKBN-SEC
               END-IF
           END-PERFORM
           IF     (IDR3                >   ZERO  )
             AND  (SPA-ERRCD           =   SPACE )
               PERFORM 490111-JYURRK-INS-SEC
           END-IF
      *    収納マスタの更新
           IF     (SPA-NYUGAIKBN       =   "2"   )
             AND  (SPA-ERRCD           =   SPACE )
               PERFORM 490113-SYUNOU-UPD-SEC
           END-IF
           .
       49011-JYURRK-PARAUPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴の更新処理
      *****************************************************************
       490113-JYURRK-UPD-SEC           SECTION.
      *
      *    受診履歴変更
           MOVE    JYURRK-KEY          TO  JYURRK-UPKEY
           MOVE    SMCNDATE-YMD        TO  JYURRK-UPYMD
           MOVE    SMCNDATE-HMS        TO  JYURRK-UPHMS
           MOVE    SPA-OPID            TO  JYURRK-OPID
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "upd1"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "J02 JYURRK UPD ERR:" MCP-RC
                               ",KEY:" JYURRK-KEY
               MOVE    "2001"              TO  SPA-ERRCD
           END-IF
           MOVE    1                   TO  FLG-JYURRK-ARI
      *
           .
       490113-JYURRK-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴の追加処理
      *****************************************************************
       490111-JYURRK-INS-SEC           SECTION.
      *
      *    枝番
           ADD     1                   TO  JYURRK-EDANUM
      *
           MOVE    SMCNDATE-YMD        TO  JYURRK-CREYMD
           MOVE    SMCNDATE-YMD        TO  JYURRK-UPYMD
           MOVE    SMCNDATE-HMS        TO  JYURRK-UPHMS
           MOVE    SPA-OPID            TO  JYURRK-OPID
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "J02 JYURRK INS ERR:" MCP-RC
                               ",KEY:" JYURRK-KEY
               MOVE    "2001"              TO  SPA-ERRCD
           END-IF
           INITIALIZE                  JYURRK-ZAINUM-G
           MOVE    1                   TO  FLG-JYURRK-ARI
           .
       490111-JYURRK-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴の削除処理
      *****************************************************************
       490112-JYURRK-DEL-SEC           SECTION.
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "J02 JYURRK DEL ERR:" MCP-RC
                               ",KEY:" JYURRK-KEY
               MOVE    "2001"              TO  SPA-ERRCD
           END-IF
      *
           .
       490112-JYURRK-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴の追加出力処理
      *****************************************************************
       4901121-JYURRK-ADDHEN-SEC           SECTION.
      *
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPNUM          TO  JYURRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-RRK-SRYKA (IDX) TO  JYURRK-SRYKA
           MOVE    SPA-RRK-RRKYMD(IDX) TO  JYURRK-SRYYMD
           MOVE    SPA-RRK-RENNUM(IDX) TO  JYURRK-RENNUM
           MOVE    SPA-RRK-DOUJI-RENNUM(IDX)
                                       TO  JYURRK-DOUJI-RENNUM
           MOVE    ZERO                TO  JYURRK-EDANUM
      *    会計連番
           MOVE    SPA-RRK-KAIKEI-RENNUM(IDX)
                                       TO  JYURRK-KAIKEI-RENNUM
           IF      JYURRK-KAIKEI-RENNUM    =   ZERO
               MOVE    1               TO  JYURRK-KAIKEI-RENNUM
           END-IF
      *
      *****MOVE    "1"                 TO  JYURRK-LASTKBN
      *
           MOVE    SPA-RRK-DRCD(IDX)   TO  JYURRK-DRCD
           MOVE    SPA-RRK-HKNCOMBI(IDX)
                                       TO  JYURRK-HKNCOMBINUM
           MOVE    SPA-RRK-HKNKBN(IDX) TO  JYURRK-HKNKBN
           MOVE    ZERO                TO  JYURRK-DENPNUM
      *
      *H30.11
           MOVE    ZERO            TO  FLG-INSARI
           PERFORM VARYING     IDR     FROM    1   BY   1
                   UNTIL      (IDR         >   135  )
                          OR  (FLG-INSARI  =   1    )
               IF      SPA-RRK-ZAINUM (IDX IDR)    NOT =   ZERO
                   MOVE    1                   TO  FLG-INSARI
               END-IF
           END-PERFORM
      *    剤がない時は処理なし
           IF      FLG-INSARI          =   ZERO
               GO      TO      4901121-JYURRK-ADDHEN-EXT
           END-IF
      *
           IF      SPA-NYUGAIKBN       =   "2"
               PERFORM 49011212-DENPNUM-SET-SEC
      *        伝票番号の編集
               MOVE    JYURRK-DENPNUM  TO  SPA-RRK-DENPNUM
                                                           (IDX)
           END-IF
      *
      *    剤番号変更
           MOVE    ZERO                TO  IDX2
           PERFORM VARYING     IDR     FROM    1   BY   1
                   UNTIL       IDR         >   135
               IF      SPA-RRK-ZAINUM (IDX IDR)    =   ZERO
                   CONTINUE
               ELSE
                   ADD     1               TO  IDX2
                   IF      IDX2            >   15
      *                出力処理
                       PERFORM 490111-JYURRK-INS-SEC
                       MOVE    1               TO  IDX2
                   END-IF
                   MOVE    SPA-RRK-ZAINUM (IDX IDR)
                                           TO  JYURRK-ZAINUM (IDX2)
                   MOVE    ZERO            TO  SPA-RRK-ZAINUM
                                                             (IDX IDR)
      *            診療区分領域の編集
                   PERFORM 4901122-JYURRK-SRYKBN-SEC
               END-IF
           END-PERFORM
      *    パラメタ出力処理
           IF     (IDX2                >   ZERO  )
            AND   (SPA-ERRCD           =   SPACE )
               PERFORM 490111-JYURRK-INS-SEC
           END-IF
      *
      *    外来の時、収納作成
           IF     (SPA-NYUGAIKBN       =   "2" ) AND
                  (SPA-RRK-HKNCOMBI(IDX)   NOT =   "9999" )
            AND   (SPA-ERRCD           =   SPACE )
               PERFORM 49011212-SYUNOU-INS-SEC
           END-IF
      *
           .
       4901121-JYURRK-ADDHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスタ登録処理
      *****************************************************************
       49011212-SYUNOU-INS-SEC           SECTION.
      *
           MOVE    SPACE               TO  SYUNOU-REC
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM          TO  SYU-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
           MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-RRK-SRYKA (IDX) TO  SYU-SRYKA
           MOVE    SPA-RRK-RRKYMD(IDX) TO  SYU-SRYYMD
           MOVE    SPA-RRK-RRKYMD(IDX) TO  SYU-SKYSTYMD
           MOVE    ALL "9"             TO  SYU-SKYEDYMD
           MOVE    JYURRK-DENPNUM      TO  SYU-DENPNUM
           MOVE    SPA-RRK-HKNCOMBI(IDX)
                                       TO  SYU-HKNCOMBINUM
           MOVE    SPA-RRK-DRCD  (IDX) TO  SYU-DRCD-G
      *    伝票番号発行日
           MOVE    SPA-SYSYMD          TO  SYU-DENPPRTYMD
      *    院外処方区分
           MOVE    "0"                 TO  SYU-INGAISHOHOKBN
      *    請求書作成区分
           MOVE    "0"                 TO  SYU-SKYKUKBN
      *
           PERFORM VARYING     IDH     FROM   1   BY  1
                   UNTIL       IDH     >   SPA-HKN-MAX
               IF      SPA-RRK-HKNCOMBI(IDX)
                                       =   SPA-GMN-THKNCOMBINUM (IDH)
                   MOVE    SPA-NAI-TFTNRATE (IDH)  TO  SYU-PTFTNRATE
                   MOVE    SPA-NAI-THKNNUM  (IDH)  TO  SYU-SYUHKNNUM
                   MOVE    SPA-NAI-TKOH1HKNNUM(IDH)
                                                   TO  SYU-KOH1HKNNUM
                   MOVE    SPA-NAI-TKOH2HKNNUM(IDH)
                                                   TO  SYU-KOH2HKNNUM
                   MOVE    SPA-NAI-TKOH3HKNNUM(IDH)
                                                   TO  SYU-KOH3HKNNUM
                   MOVE    SPA-NAI-TKOH4HKNNUM(IDH)
                                                   TO  SYU-KOH4HKNNUM
                   MOVE    SPA-HKN-MAX             TO  IDH
               END-IF
           END-PERFORM
      *    会計変更区分
           MOVE    "2"                 TO  SYU-ACCT-UPDKBN
           MOVE    1                   TO  SPA-SYUNOU-KOUSIN
      *
           MOVE    SMCNDATE-YMD        TO  SYU-CREYMD
           MOVE    SMCNDATE-YMD        TO  SYU-UPYMD
           MOVE    SMCNDATE-HMS        TO  SYU-UPHMS
           MOVE    SPA-OPID            TO  SYU-OPID
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "J02 SYUNOU INS ERR:" SYU-KEY
                                ",MCP-RC:" MCP-RC 
               MOVE    "2002"              TO  SPA-ERRCD
           END-IF
      *
      *
           .
       49011212-SYUNOU-INS-EXT.
           EXIT.
      *****************************************************************
      *    システム管理より、伝票番号取得処理
      *****************************************************************
       49011212-DENPNUM-SET-SEC        SECTION.
      *
           INITIALIZE                  SYS-0041-REC.
           MOVE    "0041"              TO  SYS-0041-KANRICD
           MOVE    "*"                 TO  SYS-0041-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-0041-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-0041-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-0041-HOSPNUM
      *    システム管理検索
           MOVE    SYS-0041-REC        TO  SYSKANRI-REC
           PERFORM 9001-SYSKANRI-READ-SEC
      *
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-0041-REC
               ADD     1                   TO  SYS-0041-DENPNUMMAX
      *!!!
               IF      SYS-0041-DENPNUMMAX     =   0
                   MOVE    1               TO  SYS-0041-DENPNUMMAX
               END-IF
               MOVE    SYS-0041-DENPNUMMAX
                                           TO  JYURRK-DENPNUM
      *
               MOVE    SMCNDATE-YMD        TO  SYS-0041-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-0041-UPHMS
      *
               MOVE    SYS-0041-REC        TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC         NOT  =   ZERO
                   MOVE    "2002"              TO  SPA-ERRCD
                   DISPLAY "J02 SYSKANRI UPDATE:" MCP-RC
               END-IF
           ELSE
               MOVE    "2002"              TO  SPA-ERRCD
               DISPLAY "J02 DENPNUM-SET ERR:" MCP-RC
           END-IF
      *
           .
       49011212-DENPNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴種別編集処理
      *****************************************************************
       4901122-JYURRK-SRYKBN-SEC           SECTION.
      *
           EVALUATE    SPA-RRK-SRYKBN (IDX IDR)
               WHEN   "11"
               WHEN   "12"
               WHEN   "13"
               WHEN   "14"
                   MOVE    "01"        TO  JYURRK-SRYKBN1
               WHEN   "21"
                   MOVE    "01"        TO  JYURRK-SRYKBN2
               WHEN   "22"
                   MOVE    "01"        TO  JYURRK-SRYKBN3
               WHEN   "23"
                   MOVE    "01"        TO  JYURRK-SRYKBN4
               WHEN   "31"
               WHEN   "32"
               WHEN   "33"
                   MOVE    "01"        TO  JYURRK-SRYKBN5
               WHEN   "40"
                   MOVE    "01"        TO  JYURRK-SRYKBN6
               WHEN   "50"
                   MOVE    "01"        TO  JYURRK-SRYKBN7
               WHEN   "54"
                   MOVE    "01"        TO  JYURRK-SRYKBN8
               WHEN   "60"
               WHEN   "64"
                   MOVE    "01"        TO  JYURRK-SRYKBN9
               WHEN   "70"
                   MOVE    "01"        TO  JYURRK-SRYKBN10
               WHEN   "80"
                   MOVE    "01"        TO  JYURRK-SRYKBN11
      ****     WHEN   "93"
      *        WHEN   "95"
      *        WHEN   "96"
      *        WHEN   "99"
      ****         MOVE    "01"        TO  JYURRK-SRYKBN11
           END-EVALUATE
           .
       4901122-JYURRK-SRYKBNN-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤番号から診療区分の編集
      *****************************************************************
       4901123-ZAINUM-SRYKBN-SEC           SECTION.
      *
           .
       4901123-ZAINUM-SRYKBN-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスタの更新処理
      *****************************************************************
       490113-SYUNOU-UPD-SEC           SECTION.
      *
      *    包括入力の時対象外
           IF      SPA-RRK-HKNCOMBI(IDX)   =   "9999"
               GO      TO      490113-SYUNOU-UPD-EXT
           END-IF
      *    伝票番号ゼロをエラーとする
           IF      SPA-RRK-DENPNUM(IDX)
                                       =   ZERO
               MOVE    "1006"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
               GO      TO      490113-SYUNOU-UPD-EXT
           END-IF
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
           MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-RRK-DENPNUM(IDX)
                                       TO  SYU-DENPNUM
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
      ******   GO      TO      490113-SYUNOU-UPD-EXT
           END-IF
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SYUNOU          =   ZERO
      *        会計変更区分
               MOVE    "1"                 TO  SYU-ACCT-UPDKBN
               MOVE    1                   TO  SPA-SYUNOU-KOUSIN
      *
               MOVE    SMCNDATE-YMD        TO  SYU-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYU-UPHMS
               MOVE    SPA-OPID            TO  SYU-OPID
      *
               MOVE    SYUNOU-REC          TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC         NOT  =   ZERO
                   MOVE    "2001"              TO  SPA-ERRCD
                   DISPLAY "J02 SYUNOU UPDATE:" MCP-RC
                              ",KEY:" SYU-KEY
               END-IF
           ELSE
      *        収納なし
               DISPLAY "J02 SYUNOU UPDATE ERR:"
                        ",DENPNUM:" SPA-RRK-DENPNUM(IDX)
           END-IF
      *
           .
       490113-SYUNOU-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納変更時、再計算処理
      *****************************************************************
       49012-SYUNOU-SAIKEISAN-SEC           SECTION.
      *
           INITIALIZE                  SPA-J025-AREA
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM          TO  SYU-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
           MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-NAI-SRYYM       TO  SYU-SRYYMD(1:6)
           MOVE    "%"                 TO  SYU-SRYYMD(7:1)
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key28"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key28"             TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYUNOU-REC
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           MOVE    ZERO                TO  WRK-GRP-DENPNUM
           PERFORM
                   UNTIL  (FLG-SYUNOU      =   1     )
                       OR (SPA-ERRCD   NOT =   SPACE )
               IF     (SYU-GRP-DENPNUM     =   ZERO )  OR
                      (SYU-GRP-RENNUM      =   1    )
                   MOVE    ZERO                TO  WRK-GRP-DENPNUM
               END-IF
               IF      SYU-ACCT-UPDKBN     NOT =   SPACE
                   IF      SYU-GRP-DENPNUM NOT =   ZERO
                       PERFORM 490123-SYUNOU-TOTAL-SEC
                   ELSE
                       PERFORM 490121-SYUNOU-SAIKEI-SEC
                   END-IF
               END-IF
      *
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key28"             TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           END-PERFORM
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key28"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *HAORI
      *H29.2
           IF      FLG-HAORI           =   1
               GO    TO  49012-SYUNOU-SAIKEISAN-EXT
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE) AND
                  (SPA-J025-MAX        >   ZERO )
               PERFORM 490129-J025-SYRI-SEC
               IF      SPA-J025-ERRMSG     =   SPACE
                   MOVE    SPACE               TO  SPA-GMN-SYUMSG
               END-IF
           ELSE
               IF      SPA-ERRCD           =   SPACE
                   MOVE    "0064"          TO  SPA-ERRCD
                END-IF
           END-IF
           .
       49012-SYUNOU-SAIKEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納まとめ再計算用処理
      *****************************************************************
       490123-SYUNOU-TOTAL-SEC           SECTION.
      *
           MOVE    SYUNOU-REC          TO  WRK-SYUNOU-REC
           INITIALIZE                  TBL-SYUNOU-AREA
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key27"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key27"             TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYUNOU-REC
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           MOVE    ZERO                TO  IDX-SYU
           PERFORM
                   UNTIL   FLG-SYUNOU      =   1
               IF      SYU-ACCT-UPDKBN     NOT =   SPACE
                   ADD     1                   TO  IDX-SYU
                   MOVE    SYUNOU-REC          TO  TBL-SYUNOU-REC
                                                    (IDX-SYU)
               END-IF
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key27"             TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           END-PERFORM
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key27"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      IDX-SYU             =   ZERO
                MOVE    WRK-SYUNOU-REC     TO  SYUNOU-REC
           ELSE
                MOVE    TBL-SYUNOU-REC(1)  TO  SYUNOU-REC
                PERFORM 490121-SYUNOU-SAIKEI-SEC
           END-IF
      *
           PERFORM VARYING     IDX     FROM    2   BY  1
                   UNTIL      (IDX     >   IDX-SYU )
                           OR (SPA-ERRCD   NOT =   SPACE )
      *
               MOVE    TBL-SYUNOU-REC(IDX) TO  SYUNOU-REC
      *
               ADD     1                   TO  SPA-J025-MAX
               IF      SPA-J025-MAX        <=  100
                   PERFORM 490122-J025-HENSYU-SEC
               END-IF
      *
      *        追加分
      *H29.6   修正
      *****    IF      TBL-SYU-ACCT-UPDKBN(IDX)    =   "2"
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
      *
               IF      TBL-SYU-ACCT-UPDKBN(IDX)    =   "2"
      *            新規分追加
                   PERFORM 4901219-SYUNOU-MEI-OUT-SEC
               END-IF
      *
               IF      FLG-ERR         =   1
                   IF      IDX-J025        <=  100
                       MOVE    "エラ−"    TO  SPA-J025-KBNMEI
                                                 (IDX-J025)
                   END-IF
               END-IF
               IF     (FLG-SYUNOU          =   ZERO )  AND
                      (FLG-ERR             =   ZERO )
                   IF      IDX-J025        <=  100
                       MOVE    SYU-SKYMONEY    TO  SPA-J025-ATO-SKYMONEY
                                                    (IDX-J025)
                   END-IF
                END-IF
           END-PERFORM
      *
           .
       490123-SYUNOU-TOTAL-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納負担金再計算用処理
      *****************************************************************
       490121-SYUNOU-SAIKEI-SEC           SECTION.
      *
           ADD     1                   TO  SPA-J025-MAX
           IF      SPA-J025-MAX        <=  100
               PERFORM 490122-J025-HENSYU-SEC
           END-IF
           MOVE    SYU-ACCT-UPDKBN     TO  WRK-ACCT-UPDKBN
      *
           MOVE    SPACE               TO  SPA-ERRCD
           INITIALIZE                  LNK-ORCSSAIKEISAN-REC
           MOVE    SYU-HOSPNUM         TO  LNK-ORCSSKS-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO  LNK-ORCSSKS-NYUGAIKBN
           MOVE    SYU-PTID            TO  LNK-ORCSSKS-PTID
           MOVE    SYU-DENPNUM         TO  LNK-ORCSSKS-DENPNUM
           MOVE    SYU-SRYKA           TO  LNK-ORCSSKS-SRYKA
           CALL    "ORCSSAIKEISAN"     USING
                                       SPA-AREA
                                       LNK-ORCSSAIKEISAN-REC
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    0                   TO  FLG-ERR
           ELSE
      *H25.1
               IF      SPA-ERRCD           =   "0070"
      *            ＤＢ更新エラー
                   MOVE    "2009"              TO  SPA-ERRCD
                   GO      TO      490121-SYUNOU-SAIKEI-EXT
               ELSE
      *            再計算エラー
                   MOVE    1                   TO  FLG-ERR
               END-IF
           END-IF
      *
      *    受診履歴更新
           PERFORM 4901211-JYURRK-UPDKBN-SEC
      *
           MOVE    SPACE               TO  SPA-ERRCD
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
      *
           IF      FLG-ERR         =   1
               IF      IDX-J025        <=  100
                   MOVE    "エラ−"    TO  SPA-J025-KBNMEI
                                                 (IDX-J025)
                   STRING  "収納がまとめ入力分の為、"
                           "再計算できませんので、"
                           "診療行為で訂正して下さい"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-J025-ERRMSG
                   END-STRING
               END-IF
           END-IF
           IF     (FLG-SYUNOU          =   ZERO ) AND
                  (WRK-ACCT-UPDKBN     =   "2"  )
      *        新規分追加
               PERFORM 4901219-SYUNOU-MEI-OUT-SEC
           END-IF
           IF      FLG-SYUNOU          =   ZERO
               IF      IDX-J025        <=  100
                   MOVE    SYU-SKYMONEY    TO  SPA-J025-ATO-SKYMONEY
                                                    (IDX-J025)
               END-IF
      *
               IF      WRK-ACCT-UPDKBN     NOT =   "2"
                   IF      FLG-ERR             =   ZERO
      *                会計変更区分
                       MOVE    SPACE           TO  SYU-ACCT-UPDKBN
                   END-IF
      *
                   MOVE    SMCNDATE-YMD        TO  SYU-UPYMD
                   MOVE    SMCNDATE-HMS        TO  SYU-UPHMS
                   MOVE    SPA-OPID            TO  SYU-OPID
      *
                   MOVE    SYUNOU-REC          TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "tbl_syunou"        TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC         NOT  =   ZERO
                       MOVE    "2001"              TO  SPA-ERRCD
                       DISPLAY "J02 SYUNOU UPDATE:" MCP-RC
                          ",KEY:" SYU-KEY
                   END-IF
               END-IF
           END-IF
      *
           .
       490121-SYUNOU-SAIKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納新規追加分処理
      *****************************************************************
       4901219-SYUNOU-MEI-OUT-SEC           SECTION.
      *
           IF      FLG-ERR         =   1
               MOVE    ZERO            TO  SYU-SKYMONEY
           END-IF
      *    入金状態が未入金の時、入金額ゼロ
           IF     (SPA-NYUKIN-JYOTAI   =   "2" ) 
               MOVE    ZERO                TO  SYU-NYUKIN-TOTAL
           ELSE
      *    入返金額
               MOVE    SYU-SKYMONEY        TO  SYU-NYUKIN-TOTAL
           END-IF
      *    入金回数
           MOVE    1                   TO  SYU-NYUKIN-KAISU
      *    伝票状態区分
           IF      SYU-SKYMONEY        =   ZERO
               MOVE    SPACE               TO  SYU-DENPJTIKBN
           ELSE
      ******   MOVE    "1"                 TO  SYU-DENPJTIKBN
      *H29.6   未収判定
               IF      SYU-SKYMONEY        <=  SYU-NYUKIN-TOTAL
                   MOVE    "1"                 TO  SYU-DENPJTIKBN
               ELSE
                   MOVE    "2"                 TO  SYU-DENPJTIKBN
               END-IF
           END-IF
           IF      FLG-ERR             =   ZERO
      *        会計変更区分
               MOVE    SPACE           TO  SYU-ACCT-UPDKBN
           END-IF
      *
           MOVE    SMCNDATE-YMD        TO  SYU-UPYMD
           MOVE    SMCNDATE-HMS        TO  SYU-UPHMS
           MOVE    SPA-OPID            TO  SYU-OPID
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC         NOT  =   ZERO
               MOVE    "2001"              TO  SPA-ERRCD
               DISPLAY "J02 SYUNOU UPDATE:" MCP-RC
                            ",KEY:" SYU-KEY
           END-IF
      *    収納明細
           MOVE    SPACE               TO  SYUMEI-REC
           INITIALIZE                      SYUMEI-REC
           MOVE    SYU-HOSPNUM         TO  SMEI-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID            TO  SMEI-PTID
      *    伝票番号
           MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
      *    伝票番号枝番
           MOVE    01                  TO  SMEI-DENPEDANUM
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syumei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 982-SYUMEI-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYUMEI-REC
               INITIALIZE                  SYUMEI-REC
               MOVE    1               TO  FLG-SYUMEI
           END-IF
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SYUMEI          =   1
               PERFORM 4901211-SYUMEI-OUT-SEC
           END-IF
      *    収納合計の請求額クリア
           IF      FLG-ERR         =   1
               IF      (SYU-GRP-DENPNUM    >   ZERO )  AND
                       (SYU-FUKU-DENPNUM   >   ZERO )  AND
                       (SYU-FUKU-KBN       =   "1"  )
                   PERFORM 4901291-SYUTOTAL-ERRUPD-SEC
               END-IF
           END-IF
      *
           .
       4901219-SYUNOU-MEI-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納合計更新処理
      *****************************************************************
        4901291-SYUTOTAL-ERRUPD-SEC      SECTION.
      *
           INITIALIZE                  SYUTTL-REC
           MOVE    SYU-HOSPNUM         TO  SYUTTL-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO  SYUTTL-NYUGAIKBN
           MOVE    SYU-PTID            TO  SYUTTL-PTID
           MOVE    SYU-FUKU-DENPNUM    TO  SYUTTL-DENPNUM
      *
           MOVE    SYUTTL-REC          TO  MCPDATA-REC
           MOVE    "tbl_syutotal"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syutotal"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 981-SYUTTL-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYUTTL
               INITIALIZE              SYUTTL-REC
           END-IF
           MOVE    "tbl_syutotal"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SYUTTL          =   ZERO
               MOVE    ZERO                TO  SYUTTL-SKYMONEY
               MOVE    ZERO                TO  SYUTTL-NYUKIN-TOTAL
               MOVE    1                   TO  SYUTTL-NYUKIN-KAISU
      *
               MOVE    SMCNDATE-YMD        TO  SYUTTL-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYUTTL-UPHMS
               MOVE    SPA-OPID            TO  SYUTTL-OPID
      *
               MOVE    SYUTTL-REC          TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_syutotal"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "J02 SYUTTL UPD ERR:" SYUTTL-KEY
                       ",MCP-RC:"  MCP-RC
                   MOVE    "2003"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
        4901291-SYUTOTAL-ERRUPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細マスター作成処理
      *****************************************************************
       4901211-SYUMEI-OUT-SEC           SECTION.
      *
           MOVE    SPACE               TO  SYUMEI-REC
           INITIALIZE                      SYUMEI-REC
           MOVE    SYU-HOSPNUM          TO  SMEI-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID            TO  SMEI-PTID
      *伝票番号
           MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
      *伝票番号枝番
           MOVE    01                  TO  SMEI-DENPEDANUM
      *伝票状態区分
           MOVE    SPACE               TO  SMEI-MEISAIJOUTAIKBN
      *請求発行日（発行日の西暦）
           MOVE    SPA-SYSYMD          TO  SMEI-SKYPRINTYMD
      *請求再発行日
           MOVE    SPACE               TO  SMEI-SKYREPRINTYMD
      *領収書発行日
           MOVE    SMEI-SKYPRINTYMD    TO  SMEI-RYOSYUPRINTYMD
      *領収書再発行日
           MOVE    SPACE               TO  SMEI-RYOSYUREPRINTYMD
      *請求金額
           MOVE    SYU-SKYMONEY        TO  SMEI-SKYMONEY
      *入返金日
           MOVE    SMEI-SKYPRINTYMD    TO  SMEI-NYUHEN-YMD
      *入金連番
           MOVE    01                  TO  SMEI-NYUKINRENNUM
           MOVE    SYU-SRYKA           TO  SMEI-SRYKA
      *
      *    請求額がゼロの時、入金力なし
      *****IF      SYU-NYUKIN-TOTAL    =   ZERO
           IF      SYU-SKYMONEY        =   ZERO
      *入金方法
               MOVE    ZERO                TO  SMEI-NYUKIN-HOHO
           ELSE
      *入返金区分
               MOVE    "1"                 TO  SMEI-NYUHEN-KBN
      *入返金額
               MOVE    SYU-NYUKIN-TOTAL    TO  SMEI-NYUHEN-MONEY
      *入金方法
      ******** MOVE    "01"                TO  SMEI-NYUKIN-HOHO
               MOVE    SPA-NYUKIN-HOHO     TO  SMEI-NYUKIN-HOHO
           END-IF
      *状態区分
           IF      SYU-SKYMONEY        =   ZERO
               MOVE    SPACE               TO  SMEI-JOUTAIKBN
           ELSE
               MOVE    "1"                 TO  SMEI-JOUTAIKBN
           END-IF
      *    収納履歴編集処理
           PERFORM 49012111-SRYURRK-HEN-SEC
      *
           MOVE    SMCNDATE-YMD        TO  SMEI-CREYMD
           MOVE    SMCNDATE-YMD        TO  SMEI-UPYMD
           MOVE    SMCNDATE-HMS        TO  SMEI-UPHMS
           MOVE    SMCNDATE-HMS        TO  SMEI-CREHMS
           MOVE    SPA-OPID            TO  SMEI-OPID
      *
           MOVE    SMEI-CREYMD         TO  SDAILYKEY-CREYMD
           MOVE    SMEI-CREHMS         TO  SDAILYKEY-CREHMS
           MOVE    SMEI-NYUHEN-YMD     TO  SDAILYKEY-BASEYMD
           PERFORM 800-DAILYKEY-SEC
           MOVE    SDAILYKEY-DAILYKEY  TO  SMEI-DAILYKEY
      *
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "2002"          TO  SPA-ERRCD
               DISPLAY "J02 SINMEI-SYORI MCP-RC:" MCP-RC
                        ",KEY:" SMEI-KEY
           END-IF
      *
           .
       4901211-SYUMEI-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納履歴編集処理
      *****************************************************************
       49012111-SRYURRK-HEN-SEC           SECTION.
      *
           INITIALIZE                  SS002-AREA
           MOVE    "3"                 TO  SS002-UPDKBN
           CALL    "ORCSS002"          USING    SS002-AREA
                                                SYUNOU-REC
                                                SPA-AREA
      *
           IF      SMEI-JOUTAIKBN       =   "1"
                                        OR  "3"
                                        OR  "7"
                                        OR  "8"
                                        OR  SPACE
      *        収納履歴番号
               MOVE    SS002-SYURRKNUM     TO  SMEI-SYURRKNUM
      *        収納履歴枝番号
               MOVE    SS002-SYUEDANUM     TO  SMEI-SYUEDANUM
      *        収納履歴更新区分
               MOVE    SS002-SYURRKUPDKBN  TO  SMEI-SYURRKUPDKBN
           END-IF
      *
           .
       49012111-SRYURRK-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴  変更処理
      *****************************************************************
       490122-J025-HENSYU-SEC           SECTION.
      *
           MOVE    ZERO                TO  IDJ2
           PERFORM VARYING     IDJ3     FROM    1   BY  1
                   UNTIL       IDJ3     >   SPA-J025-MAX
               IF      SYU-DENPNUM     =   SPA-J025-DENPNUM(IDJ3)
                   MOVE    IDJ3            TO  IDJ2
                   MOVE    SPA-J025-MAX    TO  IDJ3
               END-IF
           END-PERFORM
           IF      IDJ2                >   ZERO
               COMPUTE SPA-J025-MAX    =   SPA-J025-MAX   -   1
               MOVE    IDJ2            TO  IDX-J025
           ELSE
               MOVE    SPA-J025-MAX    TO  IDX-J025
           END-IF
      *
           MOVE    SYU-DENPNUM         TO  SPA-J025-DENPNUM
                                                    (IDX-J025)
           PERFORM VARYING     IDH     FROM    1   BY  1
                   UNTIL       IDH     >   SPA-HKN-MAX
               IF      SPA-GMN-THKNCOMBINUM (IDH)
                                           =   SYU-HKNCOMBINUM
                   MOVE    SPA-GMN-THKNCOMBIMEI (IDH)
                                           TO  SPA-J025-HKNCOMBI
                                                    (IDX-J025)
                   MOVE    SPA-HKN-MAX     TO  IDH
               END-IF
           END-PERFORM
           PERFORM VARYING     IDH     FROM    1   BY  1
                   UNTIL       IDH     >   SPA-SRYKA-MAX
               IF      SYU-SRYKA       =   SPA-GMN-SRYKAL (IDH)
                   MOVE    SPA-GMN-SRYKAMEIL (IDH)
                                           TO  SPA-J025-SRYKA
                                                    (IDX-J025)
                   MOVE    SPA-SRYKA-MAX   TO  IDH
               END-IF
           END-PERFORM
           MOVE    SYU-PTFTNRATE       TO  SPA-J025-FTNRATE
                                                    (IDX-J025)
           MOVE    SYU-SRYYMD          TO  WRK-SYMD
           PERFORM 41012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-J025-SRYYMD
                                                    (IDX-J025)
           MOVE    SYU-DENPPRTYMD      TO  WRK-SYMD
           PERFORM 41012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-J025-DENPPRTYMD
                                                    (IDX-J025)
           MOVE    SYU-SKYMONEY        TO  SPA-J025-MAE-SKYMONEY
                                                    (IDX-J025)
           IF      SYU-ACCT-UPDKBN     =   "2"
               MOVE    "新規"          TO  SPA-J025-KBNMEI
                                                    (IDX-J025)
           END-IF
      *
           .
       490122-J025-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴更新処理
      *****************************************************************
       4901211-JYURRK-UPDKBN-SEC           SECTION.
      *
           INITIALIZE                  JYURRK-REC
           MOVE    SYU-HOSPNUM          TO  JYURRK-HOSPNUM
           MOVE    SYU-PTID            TO  JYURRK-PTID
           MOVE    SYU-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SYU-SRYKA           TO  JYURRK-SRYKA
           MOVE    SYU-DENPNUM         TO  JYURRK-DENPNUM
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *H24.5
      *    伝票番号重複対応（診療年月日・保険組合せ一致判定）
           MOVE    ZERO                TO  FLG-JYURRK-OK
           PERFORM UNTIL   (FLG-JYURRK-OK  =   1   )
                       OR  (FLG-JYURRK     =   1   )
               IF     (SYU-SRYYMD          =   JYURRK-SRYYMD     )
                 AND  (SYU-HKNCOMBINUM     =   JYURRK-HKNCOMBINUM)
                   MOVE    1                   TO  FLG-JYURRK-OK
               ELSE
                   MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                   MOVE    "key6"              TO  MCP-PATHNAME
                   PERFORM 970-JYURRK-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-JYURRK          =   ZERO
      *        収納更新区分
               IF      FLG-ERR             =   1
                   MOVE    "1"                 TO  JYURRK-SYU-UPDKBN
               ELSE
                   MOVE    SPACE               TO  JYURRK-SYU-UPDKBN
               END-IF
      *
               MOVE    SMCNDATE-YMD        TO  JYURRK-UPYMD
               MOVE    SMCNDATE-HMS        TO  JYURRK-UPHMS
               MOVE    SPA-OPID            TO  JYURRK-OPID
      *
               MOVE    JYURRK-REC          TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "J02 JYURRK UPD ERR:" MCP-RC
                                   ",KEY:" JYURRK-KEY
                   MOVE    "2001"              TO  SPA-ERRCD
               END-IF
           END-IF
           .
       4901211-JYURRK-UPDKBN-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴  変更処理
      *****************************************************************
       4902-SANTEI-KOUSIN-SEC           SECTION.
      *
      *    包括入力の時対象外
           IF      ACCT-HKNCOMBI       =   "9999"
               GO      TO      4902-SANTEI-KOUSIN-EXT
           END-IF
      *
      *    自費の時対象外
           IF      ACCT-SRYKBN         =   "95"  OR  "96"
               GO      TO      4902-SANTEI-KOUSIN-EXT
           END-IF
      ***  包括診療対象外
      *    IF      ACCT-JIHOKBN        =   "1"
      *        GO      TO      4902-SANTEI-KOUSIN-EXT
      **** END-IF
      *
      *    診療行為マスター読み込み
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *    点数検索用日付編集
           MOVE    ACCT-SRYYM          TO  WRK-SRYYMD(1:6)
           MOVE    01                  TO  WRK-SRYDD
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               IF      ACCT-DAY (1 IDX-D)  NOT =   ZERO
                   MOVE    IDX-D           TO  WRK-SRYDD
                   MOVE    31              TO  IDX-D
               END-IF
           END-PERFORM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-RAED-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
           PERFORM UNTIL       FLG-SRYACT      =   1
      *        剤明細編集
               PERFORM VARYING     IDZ     FROM    1   BY  1
                       UNTIL      (IDZ         >   5        )  OR
                                  (SRY-SRYCD (IDZ) =   SPACE)
      *            手技料の時、算定歴を判定する
                   IF     (SRY-SRYCD (IDZ)(1:1)    =   "1" )
      *                システム予約コード
                       OR (SRY-SRYCD (IDZ)(1:3)    =   "099" )
      *
                       MOVE    SRY-SRYCD (IDZ)     TO  TNS-SRYCD
                       MOVE    SRY-SRYCD (IDZ)     TO  WRK-SRYCD
      *                算定用の診療コードへ変更
                       PERFORM 910-TENSU-READ-SEC
                       PERFORM 45020-SANTEI-SRYCD-SEC
                       IF      SRY-SRYCD (IDZ) NOT =   WRK-SRYCD
                           MOVE    WRK-SRYCD       TO  TNS-SRYCD
                           PERFORM 910-TENSU-READ-SEC
                       END-IF
                       IF      TNS-SANTEIRRKKBN    NOT =   ZERO
                           PERFORM 49021-SANTEI-UPD-SEC
                       END-IF
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-RAED-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       4902-SANTEI-KOUSIN-EXT.
           EXIT.
      *****************************************************************
      *    算定用の診療コードへ変更処理
      *****************************************************************
       45020-SANTEI-SRYCD-SEC              SECTION.
      *
      *    包括逓減区分判定
           EVALUATE    TNS-HOUKATUTEIGENKBN
      *        頭部
               WHEN    201
                   MOVE    "099700101"         TO  WRK-SRYCD
      *        躯幹
               WHEN    202
                   MOVE    "099700102"         TO  WRK-SRYCD
      *        四肢
               WHEN    203
                   MOVE    "099700103"         TO  WRK-SRYCD
           END-EVALUATE
           .
       45020-SANTEI-SRYCD-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴更新処理
      *****************************************************************
       49021-SANTEI-UPD-SEC           SECTION.
      *
      *    今回分の登録
           INITIALIZE                  ORCSSANTEIAREA
           MOVE    "1"                 TO  SSANTEI-SYORI
           MOVE    "J02"               TO  SSANTEI-PG
      **** MOVE    SRY-SRYCD (IDZ)     TO  SSANTEI-SRYCD
           MOVE    WRK-SRYCD           TO  SSANTEI-SRYCD
           MOVE    ACCT-SRYYM          TO  SSANTEI-SRYYMD(1:6)
           MOVE    "01"                TO  SSANTEI-SRYYMD(7:2)
           MOVE    ACCT-PTID           TO  SSANTEI-PTID
           MOVE    ACCT-NYUGAIKBN      TO  SSANTEI-NYUGAIKBN
           MOVE    ACCT-SRYKA          TO  SSANTEI-SRYKA
           MOVE    ZERO                TO  SSANTEI-MSANTEID
           MOVE    ZERO                TO  SSANTEI-MSANTEICNT
      *    保険組合せ
           MOVE    ACCT-HKNCOMBI       TO  SPA-HKNCOMBINUM
      *    数量
      *    １日上限で単位が分以外の時、数量編集
           IF     (SRY-SRYSURYO(IDZ)   >   ZERO  )
               AND (TNS-JGNCNT1D       >   ZERO )
               AND (TNS-TANICD     NOT =   "001")
               MOVE    SRY-SRYSURYO(IDZ)   TO  SSANTEI-SRYSURYO
           END-IF
      *!   保険内容編集
      *!   MOVE    SPA-HKNCOMBINUM     TO  WRK-HKNCOMBI
      *!   PERFORM 510-HKNCOMBI-MEI-SEC
      *!   MOVE    SPA-NAI-HKNKBN      TO  SPA-HKNKBN
      *    保険内容編集
           PERFORM VARYING     IDH     FROM    1   BY  1
                   UNTIL       IDH     >   SPA-HKN-MAX
               IF      SPA-GMN-THKNCOMBINUM (IDH)
                                           =   SPA-HKNCOMBINUM
      *            労災判定
                   MOVE    SPA-NAI-THKNKBN(IDH)    TO  SPA-HKNKBN
                   MOVE    SPA-NAI-TRSI-HKNKBN (IDH)
                                                   TO  SPA-RSI-HKNKBN
                   MOVE    SPA-NAI-TRSI-JUNKYO (IDH)
                                                   TO  SPA-RSI-JUNKYO
               END-IF
           END-PERFORM
      *
      *    今回分
           PERFORM VARYING IDX-D       FROM    1   BY  1
                   UNTIL   IDX-D       >   31
               IF      SPA-NAI-DAY (IDX 1 IDX-D)  >   ZERO
                   IF      SSANTEI-SRYYMD(7:2)    =  "01"
                       MOVE    IDX-D       TO  SSANTEI-SRYYMD(7:2)
                   END-IF
                   MOVE    SPA-NAI-DAY (IDX 1 IDX-D)
                                           TO  SSANTEI-DAY (IDX-D)
               END-IF
           END-PERFORM
      *    前回分
           PERFORM VARYING IDX-D       FROM    1   BY  1
                   UNTIL   IDX-D       >   31
               IF      ACCT-DAY (1 IDX-D)  >   ZERO
                   MOVE    ACCT-DAY (1 IDX-D)
                                           TO  SSANTEI-MAE-DAY (IDX-D)
               END-IF
           END-PERFORM
      *    内分泌負荷試験削除フラグ
           IF     (TNS-HOUKNSKBN       =   08  )  AND
                  (ACCT-SRYKBN         =   "60")
               MOVE    1               TO  FLG-NAIBUN
           END-IF
      *
           CALL    "ORCSSANTEI"        USING
                                       ORCSSANTEIAREA
                                       SPA-AREA
      *
      *    算定履歴付加情報処理
           IF      (WRK-SRYCD (1:7)    =   "0998001")
             AND   (SPA-ERRCD          =   SPACE    )
               IF      FLG-SYORI           =   1
      *            受診日の変更
                   PERFORM 490212-SANTEIPLUS-SYORI02-SEC
               ELSE
                   PERFORM 490211-SANTEIPLUS-SYORI-SEC
               END-IF
           END-IF
           .
       49021-SANTEI-UPD-EXT.
           EXIT.
      *****************************************************************
      *    リハビリ開始日算定履歴付加情報処理
      *****************************************************************
       490211-SANTEIPLUS-SYORI-SEC            SECTION.
      *
      *    算定履歴検索
           MOVE    SPACE               TO  SANTEI-REC
           INITIALIZE                      SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    ACCT-SRYYM          TO  SANTEI-SRYYM
      *
           IF     (SPA-HKNKBN      NOT =   ZERO)
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
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    算定履歴付加情報検索
           INITIALIZE                  SANTEIPLUS-REC
           MOVE    SANTEI-HOSPNUM      TO  SANTEIPLUS-HOSPNUM
           MOVE    SANTEI-PTID         TO  SANTEIPLUS-PTID
           MOVE    SANTEI-SRYCD        TO  SANTEIPLUS-SRYCD
           MOVE    SANTEI-SRYYM        TO  SANTEIPLUS-SRYYM
           MOVE    SANTEI-SRYKA        TO  SANTEIPLUS-SRYKA
           MOVE    SANTEI-NYUGAIKBN    TO  SANTEIPLUS-NYUGAIKBN
           MOVE    SANTEI-HKNCOMBINUM  TO  SANTEIPLUS-HKNCOMBINUM
      *    MOVE    IDX-Y2              TO  SANTEIPLUS-DAYKEY
      *
           MOVE    SANTEIPLUS-REC      TO  MCPDATA-REC
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santeiplus"    TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 920-SANTEIPLUS-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEIPLUS
           END-IF
           PERFORM
                   UNTIL   FLG-SANTEIPLUS  =   1
               IF      SANTEI-DAY (SANTEIPLUS-DAYKEY)  =   ZERO
      *            算定のない日は、削除
                   PERFORM 4902111-SANTEIPLUS-DELETE-SEC
               END-IF
               MOVE    "tbl_santeiplus"    TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 920-SANTEIPLUS-READ-SEC
           END-PERFORM
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       490211-SANTEIPLUS-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    算定履歴付加情報削除処理
      *****************************************************************
       4902111-SANTEIPLUS-DELETE-SEC            SECTION.
      *
           MOVE    SANTEIPLUS-REC      TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "J02 SANTEIPLUS DEL ERR:" SANTEIPLUS-KEY
                                      ","   MCP-RC
           END-IF
           .
       4902111-SANTEIPLUS-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    リハビリ開始日算定履歴付加情報処理
      *****************************************************************
       490212-SANTEIPLUS-SYORI02-SEC  SECTION.
      *
      *    算定履歴付加情報検索
           INITIALIZE                  SANTEIPLUS-REC
           MOVE    SPA-HOSPNUM         TO  SANTEIPLUS-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEIPLUS-PTID
           MOVE    WRK-SRYCD           TO  SANTEIPLUS-SRYCD
           MOVE    ACCT-SRYYM          TO  SANTEIPLUS-SRYYM
           MOVE    ZERO                TO  SANTEIPLUS-SRYKA
           MOVE    ZERO                TO  SANTEIPLUS-NYUGAIKBN
      *
           IF     (SPA-HKNKBN      NOT =   ZERO)
               MOVE    SPA-HKNCOMBINUM     TO  SANTEIPLUS-HKNCOMBINUM
           ELSE
               MOVE    ZERO                TO  SANTEIPLUS-HKNCOMBINUM
           END-IF
           MOVE    IDX-D2              TO  SANTEIPLUS-DAYKEY
      *
           MOVE    SANTEIPLUS-REC      TO  MCPDATA-REC
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santeiplus"    TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SANTEIPLUS-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEIPLUS
           END-IF
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    変更前に存在する時、削除
           IF      FLG-SANTEIPLUS      =   ZERO
               MOVE    IDX-D1              TO  SANTEIPLUS-DAYKEY
      *
               MOVE    SANTEIPLUS-REC      TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_santeiplus"    TO  MCP-TABLE
               MOVE    "del3"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               GO      TO   490212-SANTEIPLUS-SYORI02-EXT
           END-IF
      *    算定履歴付加情報検索
           INITIALIZE                  SANTEIPLUS-REC
           MOVE    SPA-HOSPNUM         TO  SANTEIPLUS-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEIPLUS-PTID
           MOVE    WRK-SRYCD           TO  SANTEIPLUS-SRYCD
           MOVE    ACCT-SRYYM          TO  SANTEIPLUS-SRYYM
           MOVE    ZERO                TO  SANTEIPLUS-SRYKA
           MOVE    ZERO                TO  SANTEIPLUS-NYUGAIKBN
      *
           IF     (SPA-HKNKBN      NOT =   ZERO)
               MOVE    SPA-HKNCOMBINUM     TO  SANTEIPLUS-HKNCOMBINUM
           ELSE
               MOVE    ZERO                TO  SANTEIPLUS-HKNCOMBINUM
           END-IF
           MOVE    IDX-D1              TO  SANTEIPLUS-DAYKEY
      *
           MOVE    SANTEIPLUS-REC      TO  MCPDATA-REC
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santeiplus"    TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SANTEIPLUS-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEIPLUS
           END-IF
           PERFORM
                   UNTIL   FLG-SANTEIPLUS  =   1
      *        削除
               PERFORM 4902111-SANTEIPLUS-DELETE-SEC
      *        算定日変更
               MOVE    IDX-D2              TO  SANTEIPLUS-DAYKEY
               MOVE    SMCNDATE-YMD        TO  SANTEI-UPYMD
               MOVE    SMCNDATE-HMS        TO  SANTEI-UPHMS
      *
               MOVE    SANTEIPLUS-REC      TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_santeiplus"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "J02 SANTEIPLUS INS ERR:" SANTEIPLUS-KEY
                                          ","   MCP-RC
                   MOVE    "2000"          TO  SPA-ERRCD
               END-IF
      *
               MOVE    "tbl_santeiplus"    TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SANTEIPLUS-READ-SEC
           END-PERFORM
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       490212-SANTEIPLUS-SYORI02-EXT.
           EXIT.
      *****************************************************************
      *    収納変更確認遷移処理
      *****************************************************************
       490129-J025-SYRI-SEC            SECTION.
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-J01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-J02             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGJ025"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-J02
           MOVE    SPA-WORK        TO  SPA-J01KYOUTU
      *
      *    画面カーソルセット処理
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    SPA-J01KYOUTU       TO  SPA-WORK
      *
           MOVE    "J025"              TO  SPA-SAKIPG
           MOVE    "J02"               TO  SPA-MOTOPG
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "J025"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
      *
           .
       490129-J025-SYRI-EXT.
           EXIT.
      *****************************************************************
      *    収納変更確認画面より
      *****************************************************************
       3002-J025-BACK-SEC            SECTION.
      *
           EVALUATE    SPA-J025-SYORI
               WHEN    1
      *            診療科変更
                   PERFORM 41013-SRYKA-CHANGE-SYORI-SEC
               WHEN    2
      *            診療日変更
                   PERFORM 410181-SRYYM-CHANGE-SYORI-SEC
               WHEN    3
      *            変更後診療日
                   PERFORM 410161-HENSRYYMD-SYORI-SEC
               WHEN    4
      *            その他変更
                   PERFORM 410171-NYUGAIKBN-CHG-SYORI-SEC
               WHEN    5
      *            剤変更へ
                   PERFORM 4201-ZAIHENKOU-SYORI1-SEC
               WHEN    6
      *            前月選択
                   PERFORM 410181-ZENGETU-SYORI1-SEC
               WHEN    7
      *            次月選択
                   PERFORM 410191-JIGETU-SYORI1-SEC
               WHEN    9
      *            保険一括変更へ
                   PERFORM 410011-HKNHEN-SYORI1-SEC
               WHEN    10
      *            収納チェック
                   CONTINUE
               WHEN    11
      *            診療日追加
                   PERFORM 4101621-HEN-INSSYORI-SEC
               WHEN    12
      *            診療日追加後
                   PERFORM 41016219-INSSYORI-END-SEC
               WHEN    13
      *            カルテ３号紙印刷開始へ
                   PERFORM 41201-PRINT-SYORI-SEC
               WHEN    14
      *            受診履歴編集
      **           PERFORM 3103-JYURRK-HEN-SEC
      **           MOVE    1                   TO  SPA-GMN-PAGE
      *            プレビューへ
      *            プレビュー確認
                   IF      SPA-XC01-CHK        =   "1"
                       MOVE    "0125"          TO  SPA-JIDCD
                   ELSE
                       PERFORM 4130-PREVIEW-SEC
                   END-IF
               WHEN    OTHER
      *            登録
      *            患者が選択されてきた時は、元の画面へ
                   IF      SPA-J02-MOTOFLG     =   "0"
      *                排他制御解除処理
                       PERFORM 990-LOCK-OUT-SEC
      *
                       MOVE    ZERO            TO  SPA-GMN-CUR
                       INITIALIZE                  SPA-J02-AREA
                       INITIALIZE                  SPA-SCR-REC
                       INITIALIZE                  SPA-J02-SET
                       INITIALIZE                  SPA-GMN-NYUIN-AREA
                       MOVE    SPA-J02-SRYYMDW(1:6)    TO  SPA-GMN-SRYYM
                       MOVE    SPA-J02-SRYYMD (1:6)    TO  SPA-NAI-SRYYM
                       MOVE    SPACE               TO  SPA-GMN-SRYKA-G
                   ELSE
      *               更新後、戻るへ
                       PERFORM 210-BACK-SYORI-SEC
                   END-IF
           END-EVALUATE
           MOVE    ZERO                TO  SPA-J025-SYORI
           .
       3002-J025-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納更新チェック編集処理
      *****************************************************************
       3104-SYUNOU-CHECK-SEC          SECTION.
      *
           MOVE    SPACE             TO  SPA-GMN-SYUMSG
      *
           IF     (SPA-NYUGAIKBN       =  "2"   )
      *        外来のみチェック
               INITIALIZE                  SYUNOU-REC
               MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
               MOVE    SPA-GMN-PTID        TO  SYU-PTID
               MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
               MOVE    SPA-NAI-SRYYM       TO  SYU-SRYYMD(1:6)
               MOVE    "%"                 TO  SYU-SRYYMD(7:1)
      *
               MOVE    SYUNOU-REC          TO  MCPDATA-REC
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key28"             TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_syunou"        TO  MCP-TABLE
                   MOVE    "key28"             TO  MCP-PATHNAME
                   PERFORM 980-SYUNOU-READ-SEC
               ELSE
                   INITIALIZE                  SYUNOU-REC
                   MOVE    1               TO  FLG-SYUNOU
               END-IF
               PERFORM
                       UNTIL   FLG-SYUNOU      =   1
                   IF      SYU-ACCT-UPDKBN     NOT =   SPACE
                       MOVE    "【収納更新】"  TO  SPA-GMN-SYUMSG
                       MOVE    1               TO  FLG-SYUNOU
                   END-IF
      *
                   IF      FLG-SYUNOU          =   ZERO
                       MOVE    "tbl_syunou"        TO  MCP-TABLE
                       MOVE    "key28"             TO  MCP-PATHNAME
                       PERFORM 980-SYUNOU-READ-SEC
                   END-IF
               END-PERFORM
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key28"             TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-IF
           .
       3104-SYUNOU-CHECK-EXT.
           EXIT.
      *
      *!!!未使用
      *****************************************************************
      *    保険組合せ名称編集処理
      *****************************************************************
       510-HKNCOMBI-MEI-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNCOMBIMEI
      *
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPNUM          TO  COMB-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  COMB-PTID
           MOVE    WRK-HKNCOMBI        TO  COMB-HKNCOMBINUM
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
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
           IF      FLG-HKNCOMBI        =   ZERO
               PERFORM 5101-HKNCOMBI-SET-SEC
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       510-HKNCOMBI-MEI-EXT.
           EXIT.
      *!!!未使用
      *
      *****************************************************************
      *    保険組合せ名称編集処理
      *****************************************************************
       5101-HKNCOMBI-SET-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNCOMBIMEI
      *
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
           MOVE    ZERO                TO  SPA-NAI-SISIKBN
      *    保険区分編集
           EVALUATE    COMB-HKNNUM
      *        労災
               WHEN    "971"
      *        自賠責
               WHEN    "973"
                   MOVE    1                   TO  SPA-NAI-HKNKBN
               WHEN    OTHER
      *        健保
                   MOVE    ZERO                TO  SPA-NAI-HKNKBN
           END-EVALUATE
      *
      *    労災の時、コメントの追加
           IF      COMB-HKNNUM         =   "971"  OR  "973"
      *        患者保険情報検索
               INITIALIZE                      PTRSIINF-REC
               MOVE    SPA-HOSPNUM          TO  PTRSI-HOSPNUM
               MOVE    SPA-GMN-PTID        TO  PTRSI-PTID
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
      *
      *        四肢区分
               MOVE    PTRSI-SISIKBN       TO  SPA-NAI-SISIKBN
      *        労災区分
               MOVE    PTRSI-HKNKBN        TO  SPA-NAI-RSI-HKNKBN
      *        傷病年月日
               IF      PTRSI-HKNKBN        =   "1"  OR  "2"  OR  "4"
                                           OR  "5"
                   MOVE    PTRSI-SHOBYOYMD     TO  SPA-RSI-SHOBYOYMD
               ELSE
                   MOVE    PTRSI-SINSATUYMD    TO  SPA-RSI-SHOBYOYMD
               END-IF
      *        発生年月日＋３月
      *        発生日を求める
      *        救急医療管理加算のチェック
      *        　(2004.4から使用なし　労災コード変更対応なし）
               MOVE    SPACE               TO  WRK-HASSYOYMD
               MOVE    "101110030"         TO  WRK-SRYCD
               PERFORM 51011-SANTEI-CHK-SEC
               IF      WRK-HASSYOYMD       =   SPACE
                  MOVE    SPA-RSI-SHOBYOYMD    TO  WRK-HASSYOYMD
               END-IF
      *        発生日から３ヶ月の計算
               COMPUTE WRK-HAS-MM          =   WRK-HAS-MM
                                           +   3
               IF      WRK-HAS-MM          >   12
                   COMPUTE WRK-HAS-MM      =   WRK-HAS-MM    -   12
                   COMPUTE WRK-HAS-YY      =   WRK-HAS-YY    +   1
               END-IF
               MOVE    WRK-HASSYOYMD       TO  SPA-RSI-HASSYOYMD3
           END-IF
      *
           .
       5101-HKNCOMBI-SET-EXT.
           EXIT.
      *!!!未使用　まで
      *
      *
      *****************************************************************
      *    算定履歴チェック処理
      *****************************************************************
       51011-SANTEI-CHK-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-HASSYOYMD
      *
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU           =   1
               GO      TO      51011-SANTEI-CHK-EXT
           END-IF
      *    保険をキーにするかのチェック
           SET     TBL-SAN             TO  1
           SEARCH  TBL-SANTEI-OCC      VARYING   TBL-SAN
               AT  END
                   MOVE    ZERO            TO  FLG-HKNSAN
               WHEN    WRK-SRYCD       =   TBL-SANTEI-SRYCD
                                                          (TBL-SAN)
                   MOVE    1               TO  FLG-HKNSAN
      *            Ｈ１８／４月から対応
                   IF      TBL-RIGAKU-RYO(TBL-SAN) =   "H"
      *************    IF      WRK-SRYYMD      >=  "20060401"
                       IF      WRK-SRYYMD      <   "20060401"
                           MOVE    ZERO            TO  FLG-HKNSAN
                       END-IF
                   END-IF
          END-SEARCH
      *
      *    算定履歴検索
           MOVE    SPACE               TO  SANTEI-REC
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM          TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           EVALUATE    TNS-SANTEIRRKKBN
               WHEN    1
                   MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                   MOVE    ZERO                TO  SANTEI-SRYKA
               WHEN    2
                   MOVE    SPA-NYUGAIKBN       TO  SANTEI-NYUGAIKBN
                   MOVE    ZERO                TO  SANTEI-SRYKA
               WHEN    3
                   MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                   MOVE    SPA-SRYKA           TO  SANTEI-SRYKA
               WHEN    4
                   MOVE    SPA-NYUGAIKBN       TO  SANTEI-NYUGAIKBN
                   MOVE    SPA-SRYKA           TO  SANTEI-SRYKA
           END-EVALUATE
      *
           IF     (FLG-HKNSAN          =   1   )  AND
                  (SPA-NAI-HKNKBN  NOT =   ZERO)
               MOVE     WRK-HKNCOMBI       TO  SANTEI-HKNCOMBINUM
           ELSE
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           END-IF
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           PERFORM
               UNTIL       FLG-SANTEI      =   1
      *
               IF      SANTEI-MSANTEICNT   >   ZERO
      *            初回算定日
                   MOVE    SANTEI-FSANTEIYMD   TO  WRK-HASSYOYMD
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
      *
           .
       51011-SANTEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険一括変更処理
      *****************************************************************
       4100-HKNHEN-SEC            SECTION.
      *
           IF      SPA-GMN-BANGO   NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
           END-IF
      *    患者選択チェック
           IF      SPA-GMN-PTID        =   ZERO
               MOVE    "0019"          TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-J01-SRYKA       =   ZERO
                   MOVE    "0022"          TO  SPA-ERRCD
                   MOVE    06              TO  SPA-GMN-CUR
               ELSE
                   MOVE    "0109"          TO  SPA-JIDCD
               END-IF
           END-IF
      *
           .
       4100-HKNHEN-EXT.
           EXIT.
      *****************************************************************
      *    保険一括変更処理
      *****************************************************************
       41001-HKNHEN-SYORI-SEC            SECTION.
      *
           MOVE    SPACE               TO  SPA-JIDCD
      *    現在表示されている内容を更新する
      *    診療会計マスター更新
           PERFORM 4901-SRYACCT-UPD-SEC
           IF     (SPA-ERRCD       NOT =   SPACE ) OR
                  (SPA-JIDCD       NOT =   SPACE ) OR
                  (FLG-END             =   1     )
               MOVE    9               TO  SPA-J025-SYORI
           ELSE
               PERFORM 410011-HKNHEN-SYORI1-SEC
           END-IF
      *
           .
       41001-HKNHEN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    保険一括変更処理
      *****************************************************************
       410011-HKNHEN-SYORI1-SEC            SECTION.
      *
      *
           INITIALIZE                  SPA-J021-AREA
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-J01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-J02             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGJ021"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-J02
           MOVE    SPA-WORK        TO  SPA-J01KYOUTU
      *
      *    画面カーソルセット処理
           MOVE    "MAE_HKNCOMBI"      TO  MCP-WIDGET
      *
           MOVE    SPA-J01KYOUTU       TO  SPA-WORK
      *
           MOVE    "J021"              TO  SPA-SAKIPG
           MOVE    "J02"               TO  SPA-MOTOPG
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "J021"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
      *
       410011-HKNHEN-SYORI1-EXT.
           EXIT.
      *****************************************************************
      *    点滴一括変更処理
      *****************************************************************
       42001-TENTEKI-SYORI-SEC            SECTION.
      *
      *    点滴エラー後、変更のない時、エラーとしない
           IF      SPA-J02-TENTEKI     =   1
               GO      TO      42001-TENTEKI-SYORI-EXT
           END-IF
      *
      *    入院の時、点滴液量の再計算
           INITIALIZE                  ORCSTENTEKAREA
           MOVE    "1"                 TO  ORCSTEN-KBN
           MOVE    SPA-GMN-PTID        TO  ORCSTEN-PTID
           MOVE    SPA-NAI-SRYYM       TO  ORCSTEN-SRYYM
           CALL    "ORCSTENTEK"        USING
                                       SPA-AREA
                                       ORCSTENTEKAREA
      *
           IF      ORCSTEN-ERRCD   NOT =   ZERO
               MOVE    "2000"              TO  SPA-ERRCD
               GO      TO      42001-TENTEKI-SYORI-EXT
           END-IF
           MOVE    SPACE               TO  SPA-GMN-NYUIN-AREA
           MOVE    ZERO                TO  IDX-D
      *    点滴手技料重複エラー編集
           IF      ORCSTEN-FUKUDAY-AREA    NOT =   ZERO
               PERFORM 420011-FUKUERR-SYORI-SEC
           END-IF
      *    中心静脈エラー編集
           IF     (IDX-D               <   2         )  AND
                  (ORCSTEN-MIIVHDAY-AREA   NOT =   ZERO  )
               PERFORM 420013-MIIVHDAY-HEN-SEC
           END-IF
      *    点滴手技料エラー編集
           IF     (IDX-D               <   2         )  AND
                  (ORCSTEN-MIDAY-AREA  NOT =   ZERO  )
               PERFORM 420012-MIDAY-HEN-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      ORCSTEN-HENFLG      =   1
                    MOVE    "0053"              TO  SPA-ERRCD
               END-IF
      *        点滴エラーあり
           ELSE
               MOVE    1                   TO  SPA-J02-TENTEKI
           END-IF
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (ORCSTEN-HENFLG      =   ZERO  )
               GO      TO    42001-TENTEKI-SYORI-EXT
           END-IF
      *    変更があった時、点滴のみ再表示する
      *    全科
           MOVE    SPA-GMN-SRYKA-LIST(01)
                                       TO  SPA-GMN-SRYKA-G
           MOVE    SPA-GMN-SRYKA       TO  SPA-J01-SRYKA
      *    点滴
           MOVE    SPA-GMN-SRYKBN-LST(05)
                                       TO  SPA-GMN-SRYKBN-G
           MOVE    SPA-GMN-SRYKAMEI    TO  SPA-J01-SRYKAMEI
      *
      *    受診履歴編集
           PERFORM 3103-JYURRK-HEN-SEC
      *
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
      *    保存用パラメタ作成
           PERFORM 4201-SRYACCT-PARA-SEC
      *
      *    診療会計マスターより編集をする
           PERFORM 410181-SRYACCT-HEN-SEC
      *
           MOVE    1                   TO  SPA-GMN-CUR
           .
       42001-TENTEKI-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    点滴手技料重複エラー編集処理
      *****************************************************************
       420011-FUKUERR-SYORI-SEC          SECTION.
      *
      *    点滴手技
           IF      ORCSTEN-FUKU-AREA     NOT =   ZERO
               MOVE    ORCSTEN-FUKU-AREA   TO  WRK-ERR-TBL
               PERFORM 4200111-NYUIN-ERR-HEN-SEC
               ADD     1                   TO  IDX-D
               MOVE    "点滴同時算定："    TO  SPA-GMN-NYUINMSG(IDX-D)
               MOVE    WRK-NYUIN-ERR(1:36) TO  SPA-GMN-NYUINMSG(IDX-D)
                                                               (15:)
               IF      WRK-NYUIN-ERR(37:)  NOT =   SPACE
                   ADD     1                   TO  IDX-D
                   MOVE    WRK-NYUIN-ERR(37:)  TO  SPA-GMN-NYUINMSG
                                                              (IDX-D)
               END-IF
               MOVE    "0051"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
      *    動脈
           IF      IDX-D               =   2
               GO      TO      420011-FUKUERR-SYORI-EXT
           END-IF
           IF      ORCSTEN-DOU-AREA    NOT =   ZERO
               MOVE    ORCSTEN-DOU-AREA    TO  WRK-ERR-TBL
               PERFORM 4200111-NYUIN-ERR-HEN-SEC
               ADD     1                   TO  IDX-D
               MOVE    "動脈同時算定："    TO  SPA-GMN-NYUINMSG (IDX-D)
               MOVE    WRK-NYUIN-ERR(1:36) TO  SPA-GMN-NYUINMSG (IDX-D)
                                                               (15:)
               IF     (WRK-NYUIN-ERR(37:)  NOT =   SPACE ) AND
                      (IDX-D                   <   2     )
                   ADD    1                    TO  IDX-D
                   MOVE    WRK-NYUIN-ERR(37:)  TO  SPA-GMN-NYUINMSG
                                                              (IDX-D)
               END-IF
               MOVE    "0054"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
      *    中心静脈
           IF      IDX-D               =   2
               GO      TO      420011-FUKUERR-SYORI-EXT
           END-IF
           IF      ORCSTEN-CHU-AREA    NOT =   ZERO
               MOVE    ORCSTEN-CHU-AREA    TO  WRK-ERR-TBL
               PERFORM 4200111-NYUIN-ERR-HEN-SEC
               ADD     1                   TO  IDX-D
               MOVE    "中静同時算定："    TO  SPA-GMN-NYUINMSG(IDX-D)
               MOVE    WRK-NYUIN-ERR(1:36) TO  SPA-GMN-NYUINMSG(IDX-D)
                                                           (15:)
               IF     (WRK-NYUIN-ERR(37:)  NOT =   SPACE ) AND
                      (IDX-D                   <   2     )
                   ADD    1                    TO  IDX-D
                   MOVE    WRK-NYUIN-ERR(37:)  TO  SPA-GMN-NYUINMSG
                                                              (IDX-D)
               END-IF
               MOVE    "0055"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       420011-FUKUERR-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    中心静脈加算算定エラー編集処理
      *****************************************************************
       420013-MIIVHDAY-HEN-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-NYUIN-ERR
           MOVE    1                   TO  IDX-N
      *H28.6
           MOVE    SPACE               TO  WRK-NYUIN-ERR2
           MOVE    1                   TO  IDX-N22
           PERFORM VARYING     IDX-N2  FROM    1   BY  1
                   UNTIL       IDX-N2  >   31
               IF      ORCSTEN-MIIVH-DAY (IDX-N2) =   1
                   IF      IDX-N       >   1
                       MOVE    ","         TO  WRK-NYUIN-ERR
                                                            (IDX-N:1)
                       ADD     1           TO  IDX-N
                   END-IF
                   MOVE    IDX-N2          TO  WRK-NYUIN-ERR
                                                           (IDX-N:2)
                   ADD     2               TO  IDX-N
               END-IF
      *        重複
               IF      ORCSTEN-MIIVH-DAY (IDX-N2) =   9
                   IF      IDX-N22      >   1
                       MOVE    ","         TO  WRK-NYUIN-ERR2
                                                            (IDX-N22:1)
                       ADD     1           TO  IDX-N22
                   END-IF
                   MOVE    IDX-N2          TO  WRK-NYUIN-ERR2
                                                           (IDX-N22:2)
                   ADD     2               TO  IDX-N22
               END-IF
           END-PERFORM
           IF     (WRK-NYUIN-ERR       =   SPACE )
             AND  (WRK-NYUIN-ERR2      =   SPACE )
               GO      TO     420013-MIIVHDAY-HEN-EXT 
           END-IF
      *
           IF     (WRK-NYUIN-ERR   NOT =   SPACE )
               ADD     1                   TO  IDX-D
               STRING  "中静未算定："
                       WRK-NYUIN-ERR(1:38)  DELIMITED  BY  SIZE
                       INTO                 SPA-GMN-NYUINMSG(IDX-D)
               END-STRING
               IF     (WRK-NYUIN-ERR(39:)  NOT =   SPACE )  AND
                      (IDX-D               <   2         )
                   ADD     1                   TO  IDX-D
                   MOVE    WRK-NYUIN-ERR(39:)  TO  SPA-GMN-NYUINMSG
                                                           (IDX-D)
               END-IF
      *
               MOVE    "0056"              TO  SPA-ERRCD
           END-IF
      *
           IF     (WRK-NYUIN-ERR2  NOT =   SPACE )
              AND (IDX-D               <   2     )
               ADD     1                    TO  IDX-D
               STRING  "中静重算定："
                       WRK-NYUIN-ERR2(1:38)  DELIMITED  BY  SIZE
                       INTO                 SPA-GMN-NYUINMSG(IDX-D)
               END-STRING
               IF     (WRK-NYUIN-ERR2(39:)  NOT =   SPACE )  AND
                      (IDX-D               <   2         )
                   ADD     1                   TO  IDX-D
                   MOVE    WRK-NYUIN-ERR2(39:) TO  SPA-GMN-NYUINMSG
                                                           (IDX-D)
               END-IF
               MOVE    "0066"              TO  SPA-ERRCD
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       420013-MIIVHDAY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    加算算定エラー編集処理
      *****************************************************************
       420012-MIDAY-HEN-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-NYUIN-ERR
           MOVE    1                   TO  IDX-N
           PERFORM VARYING     IDX-N2  FROM    1   BY  1
                   UNTIL       IDX-N2  >   31
               IF      ORCSTEN-MI-DAY (IDX-N2) =   1
                   IF      IDX-N       >   1
                       MOVE    ","         TO  WRK-NYUIN-ERR (IDX-N:1)
                       ADD     1           TO  IDX-N
                   END-IF
                   MOVE    IDX-N2          TO  WRK-NYUIN-ERR (IDX-N:2)
                   ADD     2               TO  IDX-N
               END-IF
           END-PERFORM
           IF      WRK-NYUIN-ERR       =   SPACE
               PERFORM 420011-KASAN-ERR-SEC
           ELSE
               ADD     1                   TO  IDX-D
               MOVE    "点滴未算定："      TO  SPA-GMN-NYUINMSG(IDX-D)
               MOVE    WRK-NYUIN-ERR(1:38) TO  SPA-GMN-NYUINMSG(IDX-D)
                                                                   (13:)
               IF      (WRK-NYUIN-ERR(39:) NOT =   SPACE)  AND
                       (IDX-D              <   2   )
                   ADD     1                   TO  IDX-D
                   MOVE    WRK-NYUIN-ERR(39:)  TO  SPA-GMN-NYUINMSG
                                                                 (IDX-D)
               END-IF
               MOVE    "0052"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       420012-MIDAY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    加算算定エラー編集処理
      *****************************************************************
       4200111-NYUIN-ERR-HEN-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-NYUIN-ERR
           MOVE    1                   TO  IDX-N
           PERFORM VARYING     IDX-N2  FROM    1   BY  1
                   UNTIL       IDX-N2  >   31
               IF      WRK-ERR-DAY (IDX-N2)    =   1
                   IF      IDX-N       >   1
                       MOVE    ","         TO  WRK-NYUIN-ERR(IDX-N:1)
                       ADD     1           TO  IDX-N
                   END-IF
                   MOVE    IDX-N2          TO  WRK-NYUIN-ERR
                                                            (IDX-N:2)
                   ADD     2               TO  IDX-N
               END-IF
           END-PERFORM
           .
       4200111-NYUIN-ERR-HEN-EXT.
           EXIT.
      *****************************************************************
      *    加算算定エラー編集処理
      *****************************************************************
       420011-KASAN-ERR-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-NYUIN-ERR
           MOVE    1                   TO  IDX-N
           PERFORM VARYING     IDX-N2  FROM    1   BY  1
                   UNTIL       IDX-N2  >   31
               IF      ORCSTEN-MI-DAY (IDX-N2) =   3
                   IF      IDX-N       >   1
                       MOVE    ","         TO  WRK-NYUIN-ERR(IDX-N:1)
                       ADD     1           TO  IDX-N
                   END-IF
                   MOVE    IDX-N2          TO  WRK-NYUIN-ERR
                                                            (IDX-N:2)
                   ADD     2               TO  IDX-N
               END-IF
           END-PERFORM
      *
           IF      WRK-NYUIN-ERR   NOT=   SPACE
               ADD     1                   TO  IDX-D
               MOVE    "加算算定日："      TO  SPA-GMN-NYUINMSG(IDX-D)
               MOVE    WRK-NYUIN-ERR(1:38) TO  SPA-GMN-NYUINMSG(IDX-D)
                                                               (13:)
               IF     (WRK-NYUIN-ERR(39:)  NOT =   SPACE )  AND
                      (IDX-D               <   2         )
                   ADD     1                   TO  IDX-D
                   MOVE    WRK-NYUIN-ERR(39:)  TO  SPA-GMN-NYUINMSG
                                                              (IDX-D)
               END-IF
               MOVE    "0058"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
           IF      IDX-D               >=  2
               GO      TO      420011-KASAN-ERR-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-NYUIN-ERR
           MOVE    1                   TO  IDX-N
           PERFORM VARYING     IDX-N2  FROM    1   BY  1
                   UNTIL       IDX-N2  >   31
               IF      ORCSTEN-MI-DAY (IDX-N2) =   2
                   IF      IDX-N       >   1
                       MOVE    ","         TO  WRK-NYUIN-ERR(IDX-N:1)
                       ADD     1           TO  IDX-N
                   END-IF
                   MOVE    IDX-N2          TO  WRK-NYUIN-ERR
                                                            (IDX-N:2)
                   ADD     2               TO  IDX-N
               END-IF
           END-PERFORM
           IF      WRK-NYUIN-ERR       =   SPACE
               GO      TO      420011-KASAN-ERR-EXT
           END-IF
      *
           ADD     1                   TO  IDX-D
           MOVE    "加算未算定："      TO  SPA-GMN-NYUINMSG(IDX-D)
           MOVE    WRK-NYUIN-ERR(1:38) TO  SPA-GMN-NYUINMSG(IDX-D)(13:)
           IF     (WRK-NYUIN-ERR(39:)  NOT =   SPACE )  AND
                  (IDX-D               <   2         )
               ADD     1                   TO  IDX-D
               MOVE    WRK-NYUIN-ERR(39:)  TO  SPA-GMN-NYUINMSG(IDX-D)
           END-IF
           MOVE    "0057"              TO  SPA-ERRCD
           MOVE    1                   TO  SPA-GMN-CUR
           .
       420011-KASAN-ERR-EXT.
           EXIT.
      *****************************************************************
      *    入院調剤料変更処理
      *****************************************************************
       4110-CHOZAI-SEC            SECTION.
      *
      *    点滴エラー後、変更のない時、エラーとしない
           IF      SPA-GMN-NYUGAIKBN   =   "2"
               GO      TO      4110-CHOZAI-EXT
           END-IF
      *
      *    入院の時、点滴液量の再計算
           INITIALIZE                  ORCSCHOZAIAREA
           MOVE    "1"                 TO  ORCSCHO-KBN
           MOVE    SPA-GMN-PTID        TO  ORCSCHO-PTID
           MOVE    SPA-NAI-SRYYM       TO  ORCSCHO-SRYYM
           CALL    "ORCSCHOZAI"        USING
                                       SPA-AREA
                                       ORCSCHOZAIAREA
      *
           IF      ORCSCHO-ERRCD   NOT =   ZERO
               MOVE    "3333"              TO  SPA-ERRCD
               GO      TO      4110-CHOZAI-EXT
           END-IF
      *    主科設定
           PERFORM 410029-ORCSSYUKAMAIN-SEC
      *
      *    受診履歴編集
           PERFORM 3103-JYURRK-HEN-SEC
      *
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
      *    保存用パラメタ作成
           PERFORM 4201-SRYACCT-PARA-SEC
      *
      *    診療会計マスターより編集をする
           PERFORM 410181-SRYACCT-HEN-SEC
      *    保険組合せ期間外あり
           IF      ORCSCHO-HKNERR      =   1
               MOVE    "K005"              TO  SPA-ERRCD
           END-IF
           MOVE    1                   TO  SPA-GMN-CUR
           .
       4110-CHOZAI-EXT.
           EXIT.
      *
      *H28.5
      *****************************************************************
      *    処方名変換処理
      *****************************************************************
       440-GENERIC-HEN-SEC             SECTION.
      *
      *    患者選択チェック
           IF      SPA-GMN-PTID        =   ZERO
               GO      TO       440-GENERIC-HEN-EXT
           END-IF
      *
           IF      SPA-NAI-SRYYM       <   "201604"
               MOVE    "0"             TO  SPA-GENERICFLG
           ELSE
               IF      SPA-GENERICFLG      =   "1"
                   MOVE    "0"             TO  SPA-GENERICFLG
               ELSE
                   MOVE    "1"             TO  SPA-GENERICFLG
               END-IF
           END-IF
           .
       440-GENERIC-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷処理
      *****************************************************************
       4120-PRINT-SEC            SECTION.
      *
           IF     SPA-GMN-PTID        =   ZERO
               MOVE    "0019"             TO  SPA-ERRCD
           END-IF
      *    番号選択中でないこと
           IF      SPA-GMN-BANGO   NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-UPDFLG          =   "1"
                   MOVE    "0116"              TO  SPA-JIDCD
               ELSE
      ******** MOVE    "0117"              TO  SPA-JIDCD
                   PERFORM 41201-PRINT-SYORI-SEC
               END-IF
           END-IF
      *
           .
       4120-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    カルテ３号印刷処理
      *****************************************************************
       41201-PRINT-SYORI-SEC            SECTION.
      *
           MOVE    ZERO                TO  SPA-J025-SYORI
           IF      SPA-UPDFLG          =   "1"
      *        診療会計マスター更新
               PERFORM 4901-SRYACCT-UPD-SEC
               IF     (SPA-ERRCD           =   SPACE )
      *            更新後再編集
                   PERFORM 41016219-INSSYORI-END-SEC
               END-IF
               IF     (SPA-ERRCD       NOT =   SPACE ) OR
                      (FLG-END             =   1     )
                   MOVE    13              TO  SPA-J025-SYORI
                   GO      TO       41201-PRINT-SYORI-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-J01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-J02             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGJ026"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-J02
           MOVE    SPA-WORK        TO  SPA-J01KYOUTU
      *
      *    画面カーソルセット処理
           MOVE    "STRDD"             TO  MCP-WIDGET
      *
           MOVE    SPA-J01KYOUTU       TO  SPA-WORK
      *
           MOVE    "J026"              TO  SPA-SAKIPG
           MOVE    "J02"               TO  SPA-MOTOPG
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "J026"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       41201-PRINT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    収納更新処理
      *****************************************************************
       490-SYUNOU-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-PTNUM       =   SPACE
               CONTINUE
           ELSE
      *
      *        更新日取得
               INITIALIZE                  ORCSMCNDATEAREA
               CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
               IF      SPA-NYUGAIKBN       =   "2"
                   PERFORM 49012-SYUNOU-SAIKEISAN-SEC
                   IF      FLG-END         =   1
                       MOVE    10              TO  SPA-J025-SYORI
                   END-IF
               END-IF
           END-IF
      *
           .
       490-SYUNOU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    プレビュー処理
      *****************************************************************
       4010-PREVIEW-SYORI-SEC          SECTION.
      *
           IF      SPA-GMN-BANGO   NOT =   ZERO
               MOVE    "0009"          TO  SPA-ERRCD
           END-IF
      *@!!
      **** ４月のレセプト適用までエラー
      *    IF      SPA-ERRCD           =   SPACE
      *        IF      SPA-NAI-SRYYM       >=  "200804"
      *            MOVE    "9009"          TO  SPA-ERRCD
      *        END-IF
      **** END-IF
      *@
      *
           MOVE    SPACE               TO  SPA-XC01-HKNKBN
           MOVE    SPACE               TO  SPA-XC01-RSI-HKNKBN
           MOVE    SPACE               TO  SPA-XC01-CHK
      *    保険判定
           MOVE    ZERO                TO  FLG-HKN-OK
           MOVE    ZERO                TO  FLG-RSIHKN-OK
           MOVE    ZERO                TO  FLG-KOGHKN-OK
           MOVE    ZERO                TO  FLG-RSIHKN-OK2
           MOVE    ZERO                TO  FLG-DAISAN-OK
           IF     (SPA-GMN-PTNUM   NOT =   SPACE )  AND
                  (SPA-GMN-PTID    NOT =   ZERO  )  AND
                  (SPA-ERRCD           =   SPACE )  AND
                  (SPA-GMN-HHKNCOMBI   NOT =    "9999" )
               IF      SPA-GMN-HHKNCOMBI   =   "0000"
                   PERFORM 40102-ALL-HKNCOMBI-CHK-SEC
               ELSE
                   PERFORM 40103-SEL-HKNCOMBI-CHK-SEC
               END-IF
           END-IF
      *****IF      SPA-GMN-HHKNCOMBI   =   "971"
      *                                OR  "973"
      *        MOVE    "0072"          TO  SPA-ERRCD
      **** END-IF
           IF     (SPA-GMN-PTNUM   NOT =   SPACE )  AND
                  (SPA-GMN-PTID    NOT =   ZERO  )  AND
                  (SPA-ERRCD           =   SPACE )
               IF      FLG-HKN-OK      =   ZERO
                   MOVE    "0071"          TO  SPA-ERRCD
               END-IF
           END-IF
           IF     (SPA-GMN-PTNUM   NOT =   SPACE )  AND
                  (SPA-GMN-PTID    NOT =   ZERO  )  AND
                  (SPA-ERRCD           =   SPACE )  AND
                  (FLG-HKN-OK          =   1     )
               IF      SPA-UPDFLG          =   "1"
                   MOVE    "0121"              TO  SPA-JIDCD
               ELSE
                   IF      SPA-XC01-CHK        =   "1"
                       MOVE    "0125"          TO  SPA-JIDCD
                   ELSE
                       PERFORM 4130-PREVIEW-SEC
                   END-IF
               END-IF
           END-IF
           .
       4010-PREVIEW-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    プレビュー対象の全保険内容チェック処理
      *****************************************************************
       40102-ALL-HKNCOMBI-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  WRK-RSI-CNT
           MOVE    ZERO                TO  WRK-KOG-CNT
           MOVE    ZERO                TO  WRK-JIB-CNT
           MOVE    ZERO                TO  WRK-NCNT-R
           MOVE    ZERO                TO  WRK-NCNT-G
           MOVE    ZERO                TO  WRK-NCNT-J
           MOVE    ZERO                TO  WRK-NCNT
           PERFORM VARYING     IDH     FROM   1   BY  1
                   UNTIL      (IDH     >   SPA-HKN-MAX   )
               IF      SPA-GMN-THKNCOMBINUM(IDH)  =   "9999"
                                                  OR  "0000"
                   CONTINUE
               ELSE
                   PERFORM 40102-HKNCOMBI-CHK-SEC
               END-IF
           END-PERFORM
      *    医保の対象あり
           IF      FLG-HKN-OK      NOT =   ZERO
               MOVE    "0"                 TO  SPA-XC01-HKNKBN
               MOVE    SPACE               TO  SPA-XC01-RSI-HKNKBN
               IF     (FLG-RSIHKN-OK   NOT =   ZERO ) OR
                      (FLG-KOGHKN-OK   NOT =   ZERO )
                 OR   (FLG-DAISAN-OK   NOT =   ZERO )
                   MOVE    "1"             TO  SPA-XC01-CHK
               END-IF
           ELSE
               IF     (WRK-RSI-CNT    NOT =   ZERO )
                  AND (WRK-JIB-CNT        =   ZERO )
                  AND (WRK-KOG-CNT        =   ZERO )
      *            労災のみ
                   MOVE    1               TO  FLG-HKN-OK
                   MOVE    "1"             TO  SPA-XC01-HKNKBN
                   MOVE    FLG-RSIHKN-OK   TO  SPA-XC01-RSI-HKNKBN
               END-IF
               IF     (WRK-RSI-CNT        =   ZERO )
                  AND (WRK-JIB-CNT    NOT =   ZERO )
                  AND (WRK-KOG-CNT        =   ZERO )
      *            自賠責
                   MOVE    1               TO  FLG-HKN-OK
                   MOVE    "2"             TO  SPA-XC01-HKNKBN
                   MOVE    FLG-RSIHKN-OK   TO  SPA-XC01-RSI-HKNKBN
               END-IF
               IF     (WRK-RSI-CNT        =   ZERO )
                  AND (WRK-JIB-CNT        =   ZERO )
                  AND (WRK-KOG-CNT    NOT =   ZERO )
      *            公害
                   MOVE    1               TO  FLG-HKN-OK
                   MOVE    "3"             TO  SPA-XC01-HKNKBN
                   MOVE    SPACE           TO  SPA-XC01-RSI-HKNKBN
               END-IF
               IF    ((WRK-RSI-CNT    NOT =   ZERO )
                  OR  (WRK-JIB-CNT    NOT =   ZERO )
                  OR  (WRK-KOG-CNT    NOT =   ZERO ))
                  AND (FLG-HKN-OK         =   ZERO )
                   MOVE    "0073"          TO  SPA-ERRCD
               END-IF
           END-IF
      *    入院中で受診履歴がない時
           IF     (FLG-HKN-OK          =   ZERO )  AND
                  (SPA-ERRCD           =   SPACE)  AND
                  (SPA-NYUGAIKBN       =  "1"   )  AND
                  (SPA-GMN-NYUINYMD    NOT =   SPACE )
      *        健保のみ
               IF     (WRK-NCNT-R          =   ZERO )   AND
                      (WRK-NCNT-J          =   ZERO )   AND
                      (WRK-NCNT-G          =   ZERO )   AND
                      (WRK-NCNT            >   ZERO )
                   MOVE    1                   TO  FLG-HKN-OK
                   MOVE    "0"                 TO  SPA-XC01-HKNKBN
                   MOVE    SPACE               TO  SPA-XC01-RSI-HKNKBN
               END-IF
      *        労災のみ
               IF     (WRK-NCNT-R          >   ZERO )   AND
                      (WRK-NCNT-J          =   ZERO )   AND
                      (WRK-NCNT-G          =   ZERO )   AND
                      (WRK-NCNT            =   ZERO )
                   MOVE    1                   TO  FLG-HKN-OK
                   MOVE    "1"                 TO  SPA-XC01-HKNKBN
                   MOVE    FLG-RSIHKN-OK2      TO  SPA-XC01-RSI-HKNKBN
               END-IF
      *        自賠責のみ
               IF     (WRK-NCNT-R          =   ZERO )   AND
                      (WRK-NCNT-J          >   ZERO )   AND
                      (WRK-NCNT-G          =   ZERO )   AND
                      (WRK-NCNT            =   ZERO )
                   MOVE    1                   TO  FLG-HKN-OK
                   MOVE    "2"                 TO  SPA-XC01-HKNKBN
                   MOVE    FLG-RSIHKN-OK2      TO  SPA-XC01-RSI-HKNKBN
               END-IF
      *        公害のみ
               IF     (WRK-NCNT-R          =   ZERO )   AND
                      (WRK-NCNT-J          =   ZERO )   AND
                      (WRK-NCNT-G          >   ZERO )   AND
                      (WRK-NCNT            =   ZERO )
                   MOVE    1                   TO  FLG-HKN-OK
                   MOVE    "3"                 TO  SPA-XC01-HKNKBN
                   MOVE    SPACE               TO  SPA-XC01-RSI-HKNKBN
               END-IF
      *
               IF     (FLG-HKN-OK          =   ZERO )
                   MOVE    "0074"          TO  SPA-ERRCD
               END-IF
           END-IF
           .
       40102-ALL-HKNCOMBI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    プレビュー対象の全保険内容チェック処理
      *****************************************************************
       40102-HKNCOMBI-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-THKNCOMBINUM(IDH)
                                       TO  WRK-HKNCOMBI
           PERFORM 40101-HKNNUM-CHK-SEC
           IF      FLG-JYURRK          =   ZERO
               IF      SPA-NAI-THKNNUM(IDH)    =   "971"
                                               OR  "973"
                   IF      SPA-NAI-THKNNUM(IDH)    =   "971"
                       ADD     1               TO  WRK-RSI-CNT
                   ELSE
                       ADD     1               TO  WRK-JIB-CNT
                   END-IF
                   IF      FLG-RSIHKN-OK       =   ZERO
                        MOVE    SPA-NAI-TRSI-HKNKBN (IDH)
                                               TO  FLG-RSIHKN-OK
                   END-IF
               ELSE
      *            公害
                   IF      SPA-NAI-THKNNUM(IDH)    =   "975"
                       ADD     1               TO  WRK-KOG-CNT
                       MOVE    1               TO  FLG-KOGHKN-OK
                   ELSE
                       MOVE    1               TO  FLG-HKN-OK
                   END-IF
      *!!!
      *            第三者行為
                   IF     (SPA-NAI-TRSI-HKNKBN (IDH)   =   "6")
                       MOVE    1               TO  FLG-DAISAN-OK
                   END-IF
               END-IF
           ELSE
               IF      SPA-NAI-THKNNUM(IDH)    =   "971"
                                               OR  "973"
                   IF      SPA-NAI-THKNNUM(IDH)    =   "971"
                       ADD     1               TO  WRK-NCNT-R
                   ELSE
                       ADD     1               TO  WRK-NCNT-J
                   END-IF
                   IF      FLG-RSIHKN-OK2  =   ZERO
                        MOVE    SPA-NAI-TRSI-HKNKBN (IDH)
                                               TO  FLG-RSIHKN-OK2
                   END-IF
               ELSE
      *            公害
                   IF      SPA-NAI-THKNNUM(IDH)    =   "975"
                       ADD     1               TO  WRK-NCNT-G
                   ELSE
                       ADD     1               TO  WRK-NCNT
                   END-IF
               END-IF
           END-IF
           .
       40102-HKNCOMBI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    プレビュー対象の選択保険処理
      *****************************************************************
       40103-SEL-HKNCOMBI-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-HHKNCOMBI   TO  WRK-HKNCOMBI
           PERFORM 40101-HKNNUM-CHK-SEC
           IF      FLG-JYURRK      =   ZERO
               MOVE    1               TO  FLG-HKN-OK
           END-IF
           IF     (FLG-HKN-OK          =   ZERO )  AND
                  (SPA-NYUGAIKBN       =  "1"   )  AND
                  (SPA-GMN-NYUINYMD    NOT =   SPACE )
               MOVE    1               TO  FLG-HKN-OK
           END-IF
      *
           IF      FLG-HKN-OK          =   1
               PERFORM VARYING     IDH     FROM    1   BY  1
                       UNTIL       IDH     >   SPA-HKN-MAX
                   IF      SPA-GMN-THKNCOMBINUM (IDH)
                                               =   SPA-GMN-HHKNCOMBI
      *              入院でアフターケアはエラー
                     IF     (SPA-NAI-THKNNUM(IDH)      =   "971")
                       AND  (SPA-NAI-TRSI-HKNKBN (IDH) =   "3"  )
                       AND  (SPA-NYUGAIKBN             =   "1"  )
                       MOVE    "0071"          TO  SPA-ERRCD
                     ELSE
      *
                       IF      SPA-NAI-THKNKBN (IDH)   =   "0"
                           MOVE    "0"         TO  SPA-XC01-HKNKBN
                           MOVE    SPACE       TO  SPA-XC01-RSI-HKNKBN
                       ELSE
                           IF      SPA-NAI-THKNNUM(IDH)    =   "975"
      *                    公害
                               MOVE    "3"         TO  SPA-XC01-HKNKBN
                               MOVE    SPACE       TO
                                                   SPA-XC01-RSI-HKNKBN
                           ELSE
      *                    労災・自賠責
                               MOVE    "1"         TO  SPA-XC01-HKNKBN
                               MOVE    SPA-NAI-TRSI-HKNKBN (IDH)
                                               TO  SPA-XC01-RSI-HKNKBN
                           END-IF
                       END-IF
                     END-IF
                       MOVE    SPA-HKN-MAX     TO  IDH
                   END-IF
               END-PERFORM
           END-IF
           .
       40103-SEL-HKNCOMBI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    プレビュー対象の保険処理
      *****************************************************************
       40101-HKNNUM-CHK-SEC          SECTION.
      *
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM          TO  JYURRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    WRK-HKNCOMBI        TO  JYURRK-HKNCOMBINUM
           MOVE    SPA-NAI-SRYYM       TO  JYURRK-SRYYMD(1:6)
           MOVE    "01"                TO  JYURRK-SRYYMD(7:2)
           MOVE    SPA-NAI-SRYYM       TO  JYURRK-UPSRYYMD(1:6)
           MOVE    "31"                TO  JYURRK-UPSRYYMD(7:2)
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       40101-HKNNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    プレビュー処理
      *****************************************************************
       4130-PREVIEW-CHK-SEC          SECTION.
      *
           IF      SPA-UPDFLG          =   "1"
               PERFORM 4901-SRYACCT-UPD-SEC
               IF     (SPA-ERRCD           =   SPACE )
      *            更新後再編集
                   PERFORM 41016219-INSSYORI-END-SEC
                   MOVE    1                   TO  SPA-GMN-PAGE
               END-IF
               IF     (SPA-ERRCD      NOT  =   SPACE ) OR
                      (FLG-END             =   1     )
                   MOVE    14              TO  SPA-J025-SYORI
               ELSE
      *
      *            プレビュー確認
                   IF      SPA-XC01-CHK        =   "1"
                       MOVE    "0125"          TO  SPA-JIDCD
                   ELSE
                       PERFORM 4130-PREVIEW-SEC
                   END-IF
               END-IF
           END-IF
           .
       4130-PREVIEW-CHK-EXT.
           EXIT.
      *****************************************************************
      *    プレビュー処理
      *****************************************************************
       4130-PREVIEW-SEC          SECTION.
      *
           PERFORM 41301-PREVIEW-KICK-SEC
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-J01KYOUTU       TO  SPA-WORK
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-J02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "J02"               TO  SPA-MOTOPG
           MOVE    "J02"               TO  SPA-MOTOPG3
           MOVE    "XC01"              TO  SPA-SAKIPG
      *
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                       TO  SPA-PRTKANRI-TBL-KEY
           MOVE    SPA-GMN-PTID        TO  LNK-XC01-TBL-GROUP
           MOVE    0                   TO  LNK-XC01-SHORI-RENNUM
           MOVE    1                   TO  LNK-XC01-RENNUM
           MOVE    1                   TO  LNK-XC01-ST-PAGE
           MOVE    99999               TO  LNK-XC01-ED-PAGE
           MOVE    "1"                 TO  LNK-XC01-MODE
           MOVE    SPA-XC01-HKNKBN     TO  LNK-XC01-HKNKBN
           MOVE    LNK-XC01-AREA       TO  SPA-WORK
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           MOVE   "XC01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4130-PREVIEW-EXT.
           EXIT.
      *****************************************************************
      *    プレビューバッチ起動処理
      *****************************************************************
       41301-PREVIEW-KICK-SEC    SECTION.
      *
      *    自賠責
           IF     SPA-XC01-RSI-HKNKBN  =   "4"
      *D                               OR  "5"
               MOVE    "2"                 TO  SPA-XC01-HKNKBN
           END-IF
      *
      *    第三者行為
           IF     SPA-XC01-RSI-HKNKBN  =   "6"
               MOVE    "4"                 TO  SPA-XC01-HKNKBN
           END-IF
      *
           MOVE    SPA-NAI-RESPRVFLG   TO  SPA-XC01-RESPRVFLG
      *
           MOVE    SPACE               TO  SPA-XC01-CHK
           MOVE    ZERO                TO  WRK-RRK-HKNKBN
      *H24.12
      *    業務区分追加
           MOVE    "1"                 TO  WRK-GYOUMUKBN
           CALL    "ORCSRECEARGMK"     USING
                                       "0"
                                       SPA-GMN-PTID
                                       SPA-NAI-SRYYM
                                       SPA-NYUGAIKBN
      *****************************    WRK-RRK-HKNKBN
                                       SPA-XC01-HKNKBN
                                       SPA-XC01-RSI-HKNKBN
                                       SPA-XC01-RESPRVFLG
                                       WRK-GYOUMUKBN
                                       SPA-AREA
      *
           .
       41301-PREVIEW-KICK-EXT.
           EXIT.
      *****************************************************************
      *    日報キー取得サブ
      *****************************************************************
       800-DAILYKEY-SEC         SECTION.
      *
           CALL    "ORCSDAILYKEY"   USING   SDAILYKEYAREA
                                            SPA-AREA
      *
           .
      *
       800-DAILYKEY-EXT.
           EXIT.
      *
      *H29.2
      *****************************************************************
      *    HAORI対応処理
      *****************************************************************
       300-HAORI-SYORI-SEC             SECTION.
      *
           EVALUATE    MCP-EVENT
               WHEN    "INIT"
      *            初期編集
                   PERFORM 3001-HAORI-INIT-SYORI-SEC
               WHEN    "UPD"
      *            保険組合せ一括変更処理
                   PERFORM 3001-HORI-UPD-SYORI-SEC
      *H30.12
               WHEN    "INIT5"
      *            初期編集（入院調剤料変更）
                   PERFORM 3002-HAORI-INIT5-SYORI-SEC
               WHEN    "CHKZ5"
      *            剤チェック（入院調剤料変更）
                   PERFORM 3002-HAORI-ZAICHK5-SYORI-SEC
               WHEN    "CHKD5"
      *            算定日チェック（入院調剤料変更）
                   PERFORM 3002-HAORI-DAYCHK5-SYORI-SEC
           END-EVALUATE
           .
       300-HAORI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    （HAORI）初期編集処理
      *****************************************************************
       3001-HAORI-INIT-SYORI-SEC          SECTION.
      *
      *     診療年月の末日算定
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    SPA-NAI-SRYYM       TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI(7:2)    TO  SPA-SRYMATU
      *
      *    年齢計算（算定月の１日の年齢）
           MOVE    SPA-NAI-SRYYM       TO  WRK-YMDS2(1:6)
           MOVE    01                  TO  WRK-DDS2
           PERFORM 30101-NENREI-SET-SEC
      *    該当月で６歳になる時、年齢フラグを立てる
           IF     (SPA-NAI-NENREI-YY   =   05  )  AND
                  (SPA-NAI-NENREI-MM   =   11  )  AND
                  (WRK-YMDS1(5:2)      =   WRK-YMDS2(5:2))
               MOVE    1                   TO  SPA-FLG-NENREI
           END-IF
      *
      *    Ｈ１４．１０時点で７０歳未満の時
           IF     (SPA-NAI-SRYYM       >=  "200210"   )  AND
                  (SPA-BIRTHDAY        >=  "19321001" )
               MOVE    75                  TO  WRK-ROU-NEN
           ELSE
               MOVE    70                  TO  WRK-ROU-NEN
           END-IF
      *    該当月１日が７５歳以上の時、老人とする
           IF      SPA-NAI-NENREI-YY   >=  WRK-ROU-NEN
               MOVE    1                   TO  SPA-J02-ROUJIN
           ELSE
               MOVE    ZERO                TO  SPA-J02-ROUJIN
           END-IF
      *    入院期間
           IF      SPA-BEDSU           >   ZERO
               PERFORM 410101-NYUINRRK-CHK-SEC
           END-IF
           .
       3001-HAORI-INIT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    （HAORI）保険一括変更処理
      *****************************************************************
       3001-HORI-UPD-SYORI-SEC          SECTION.
      *
           MOVE    1               TO  FLG-HAORI
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           IF      SPA-NYUGAIKBN       =   "2"
               PERFORM 49012-SYUNOU-SAIKEISAN-SEC
           END-IF
           .
       3001-HORI-UPD-SYORI-EXT.
           EXIT.
      *
      *H30.12
      *****************************************************************
      *    （HAORI）初期編集（入院調剤料変更）処理
      *****************************************************************
       3002-HAORI-INIT5-SYORI-SEC          SECTION.
      *
      *    初期編集
           PERFORM 3001-HAORI-INIT-SYORI-SEC
      *    入院期間外
           IF      SPA-NYUGAIKBN       =   "2"
               GO      TO  3002-HAORI-INIT5-SYORI-EXT
           END-IF
      *
      *    受診履歴編集
           PERFORM 3103-JYURRK-HEN-SEC
      *    保険一覧編集
           PERFORM 3008-HKNCOMBI-HEN-SEC
      *
      *    入院料包括診療チェック処理
           PERFORM 30091-HKTCD-CHK-SEC
      *
      *
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
      *    保存用パラメタ作成
           PERFORM 4201-SRYACCT-PARA-SEC
           MOVE    1                   TO  SPA-GMN-PAGE
           MOVE    1                   TO  SPA-STRNUM
      *
      *    診療会計マスターより編集をする
           PERFORM 410181-SRYACCT-HEN-SEC
      *    画面ＳＰＡ書込み
           PERFORM 1003-J02SPA-OUT-SEC
           .
       3002-HAORI-INIT5-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    （HAORI）剤チェック編集（入院調剤料変更）処理
      *****************************************************************
       3002-HAORI-ZAICHK5-SYORI-SEC          SECTION.
      *
           MOVE    1                   TO  SPA-GMN-CUR
           MOVE    SPA-GMN-BANGO       TO  J02-NUM
           PERFORM 41011-BANGO-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 5101-ERRMSG-SET-SEC
               MOVE    WRK-ERRMSG          TO  SPA-ERRMSG
           END-IF
      *    画面ＳＰＡ書込み
           PERFORM 1003-J02SPA-OUT-SEC
           .
       3002-HAORI-ZAICHK5-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    （HAORI）算定日チェック（入院調剤料変更）処理
      *****************************************************************
       3002-HAORI-DAYCHK5-SYORI-SEC          SECTION.
      *
           MOVE    SPACE               TO  J02
           INITIALIZE                      J02
      *
           MOVE    SPACE               TO  SPA-ERRCD
      *
           PERFORM   VARYING   IDD     FROM    1   BY  1
                     UNTIL    (IDD         >   31  )
               MOVE    SPA-GMN-DAY (IDD)   TO  J02-DAY(IDD)
           END-PERFORM
      *    剤回数
           MOVE    ZERO                TO  SPA-DAY-IDX
           PERFORM 41012-DAY-CHK-SEC
      *
           MOVE    ZERO                TO  WRK-CNT-TUKI
           MOVE    SPA-NAI-BANGO       TO  IDX-C
      *    受診歴有無のチェック
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL    (IDX         >   31  )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-GMN-DAY (IDX)   >   ZERO
      *            受診歴有無のチェック
                   PERFORM 410121-RRK-DAYCHK-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       MOVE    IDX             TO  WRK-IDX
                       MOVE    WRK-IDX         TO  SPA-DAY-IDX
                   END-IF
               END-IF
      *        上限回数のチェック
               IF     (SPA-GMN-DAY (IDX)   >   ZERO  )  AND
                      (SPA-ERRCD           =   SPACE )  AND
                      (SPA-NAI-SANTEIRRKKBN(IDX-C) NOT =   ZERO)
                   PERFORM 4702-SANTEIRRK-CHK-SEC
               END-IF
           END-PERFORM
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    WRK-IDX         TO  SPA-DAY-IDX
           END-IF
      *    受診履歴への反映
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPACE               TO  WRK-ERRCD
               PERFORM 4701-RRK-ZAINUM-SEC
      *
               MOVE    SPA-NAI-BANGO       TO  IDX
               MOVE    SPA-GNAI-DAY-G      TO  SPA-NAI-DAY-AREA (IDX)
      *        合計回数
               MOVE    ZERO                TO  WRK-ZAIKAISU
               PERFORM VARYING    IDK      FROM    1   BY  1
                       UNTIL      IDK      >   31
                   ADD     SPA-NAI-DAY  (IDX 1 IDK)
                                           TO  WRK-ZAIKAISU
               END-PERFORM
               MOVE    WRK-ZAIKAISU        TO  SPA-NAI-ZAIKAISU (IDX)
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 5101-ERRMSG-SET-SEC
               MOVE    WRK-ERRMSG          TO  SPA-ERRMSG
           END-IF
      *
      *    画面ＳＰＡ書込み
           PERFORM 1003-J02SPA-OUT-SEC
           .
       3002-HAORI-DAYCHK5-SYORI-EXT.
           EXIT.
      *H30.12
      *
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF     (SPA-ERRCD           =   SPACE )
            AND   (WRK-HZN-ERRCD   NOT =   SPACE )
               MOVE    WRK-HZN-ERRCD       TO  SPA-ERRCD
           END-IF
      *
      *    入院料算定なし
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-JIDCD           =   SPACE )
               IF      (SPA-NAI-HKTERR     =   1   )  AND
                       (SPA-NAI-HKTKEI     =   ZERO)
                   MOVE    "K077"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-NAI-HKTKEI
               END-IF
           END-IF
      *    内分泌負荷試験
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-JIDCD           =   SPACE )
               IF      (SPA-NAI-NAIBUNERR  NOT =   SPACE)  AND
                       (SPA-NAI-NAIBUNCHK      =   ZERO )
                   MOVE    1               TO  SPA-NAI-NAIBUNCHK
                   MOVE    "0120"          TO  SPA-JIDCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-LOCK        =   2
                   MOVE    "9998"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-LOCK
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-JIDCD       NOT =   SPACE
               PERFORM 520-JIDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-JIDCD2      NOT =   SPACE
               PERFORM 520-JID2SET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "J02    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
           MOVE    SPACE               TO  WRK-ERRMSG
      *    エラーメッセージ編集
           PERFORM 5101-ERRMSG-SET-SEC
      *
           MOVE    SPACE               TO  JERR
           MOVE    SPA-ERRCD           TO  JERR-ERRCODE
           MOVE    WRK-ERRMSG          TO  JERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "J02"               TO  SPA-MOTOPG
           MOVE    "JERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "JERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       5101-ERRMSG-SET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
           MOVE    SPACE               TO  WRK-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "剤番号入力エラー"
                                       TO  WRK-ERRMSG
               WHEN    "0002"
                   MOVE    "剤の変更番号がありません"
                                       TO  WRK-ERRMSG
               WHEN    "0003"
                   MOVE    "診療回数は数字で入力して下さい"
                                       TO  WRK-ERRMSG
               WHEN    "0004"
      ***          MOVE    "対象の診療科で受診履歴がない日です。"
      *                                TO  WRK-ERRMSG
      *            MOVE    "入力できません"
      ****                             TO  WRK-ERRMSG(37:)
                   STRING  "対象の剤は選択した受診履歴に算定が"
                           "ありません。"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-ERRMSG
                   END-STRING
               WHEN    "0005"
                   MOVE    "受診履歴番号がありません。"
                                       TO  WRK-ERRMSG
               WHEN    "0006"
                   MOVE    "受診履歴の選択できない処理です。"
                                       TO  WRK-ERRMSG
               WHEN    "0007"
                   MOVE
                   "変更後診療日が暦日エラーです"
                                       TO  WRK-ERRMSG
               WHEN    "0008"
                   MOVE
                   "変更後診療日は診療月内での変更のみでできます"
                                       TO  WRK-ERRMSG
      *----(01.00.01) LINE ADD START ----------------------------------
               WHEN    "0009"
                   MOVE
                   "剤が修正中です。変更確定かクリアをして下さい。"
                                       TO  WRK-ERRMSG
      *----(01.00.01) LINE ADD END   ----------------------------------
               WHEN    "0010"
                   MOVE    "診療科履歴更新エラー" 
                                       TO  WRK-ERRMSG
               WHEN    "0011"
                   STRING  "既に同じ診療科・保険組合せで受診のある日"
                           "です。変更できません。"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-ERRMSG
                   END-STRING
               WHEN    "0034"
                   STRING  "既に診療日に受診があります。"
                           "変更できません。"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-ERRMSG
                   END-STRING
               WHEN    "0012"
                   MOVE
               "受診日変更処理で受診歴確定時のみ入力できます。"
                                       TO  WRK-ERRMSG
               WHEN    "0013"
                   MOVE    "剤の変更番号を入力して下さい"
                                       TO  WRK-ERRMSG
               WHEN    "0014"
                   MOVE    "パラメタ出力エラー"
                                       TO  WRK-ERRMSG
               WHEN    "0015"
                   MOVE    "誕生日前の診療日は入力できません"
                                       TO  WRK-ERRMSG
               WHEN    "0016"
                   MOVE    "ＩＤ取得サブでエラー" 
                                       TO  WRK-ERRMSG
               WHEN    "0017"
                   MOVE    "該当の患者が存在しません。"
                                       TO  WRK-ERRMSG
               WHEN    "0018"
                   MOVE    "診療年月入力エラー"
                                       TO  WRK-ERRMSG
               WHEN    "0019"
                   MOVE    "患者番号を入力して下さい"
                                       TO  WRK-ERRMSG
               WHEN    "0020"
                   MOVE    "対象の診療年月に受診はありません。"
                                       TO  WRK-ERRMSG
               WHEN    "0021"
                   STRING  "対象の剤は労災の外来管理加算読み替え分です"
                           "。剤変更はできません。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
               WHEN    "0022"
                   STRING  "保険の一括変更は科毎に行って下さい。"
                           "全科では変更できません。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
               WHEN    "0023"
                   STRING  "当月の受診履歴が１００以上になります。"
                           "全体の受診履歴表示はできません。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
               WHEN    "0024"
                   STRING  "当月の受診履歴が１００以上になります。"
                           "追加はできません。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
               WHEN    "0025"
                   STRING  "保険組合せが有効期間外となります。"
                           "追加はできません。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
               WHEN    "0026"
                   STRING  "入院日ではありません。"
                           "追加はできません。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
               WHEN    "0027"
      *H26.10      STRING  "対象の剤は内服薬剤料逓減に関する剤です。"
                   STRING  "対象の剤は薬剤料逓減に関する剤です。"
                           "剤変更はできません。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
      *H28.4
               WHEN    "K037"
                   STRING  "警告！"
                           "対象の剤は湿布薬の院内投薬です。"
                           "枚数が減点対象の時点数計算ができません。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
               WHEN    "0028"
                   MOVE    "入院日です。外来での算定はできません。"
                                       TO  WRK-ERRMSG
               WHEN    "0029"
                   MOVE    "アフターケアは入院での算定はできません。"
                                       TO  WRK-ERRMSG
               WHEN    "0030"
                   MOVE    "入外区分がありません。"
                                       TO  WRK-ERRMSG
               WHEN    "0031"
                   MOVE    "診療種別がありません。"
                                       TO  WRK-ERRMSG
               WHEN    "0032"
                   MOVE    "保険組合せがありません。"
                                       TO  WRK-ERRMSG
               WHEN    "0033"
                   MOVE    "診療種別がありません。"
                                       TO  WRK-ERRMSG
               WHEN    "0035"
                   STRING  "全科では剤の新規登録はできません。"
                           "診療科を選択して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
               WHEN    "0036"
                   STRING  "全科での算定履歴画面遷移はできません。"
                           "診療科を選択して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
               WHEN    "0040"
                   MOVE    "算定できる点滴手技料がありません。"
                                       TO  WRK-ERRMSG
                   MOVE    "訂正で点滴を修正して下さい。"
                                       TO  WRK-ERRMSG(35:)
      *
              WHEN    "0050"
                   MOVE    "点滴一括処理は入院の時のみ選択できます。"
                                       TO  WRK-ERRMSG
               WHEN    "0051"
                   MOVE    "点滴手技料が同時算定されています。"
                                       TO  WRK-ERRMSG
               WHEN    "0052"
                   MOVE    "点滴手技料が算定できる日があります。"
                                       TO  WRK-ERRMSG
               WHEN    "0053"
                   MOVE    "点滴手技料を削除しました。"
                                       TO  WRK-ERRMSG
                   MOVE    "確認して下さい"
                                       TO  WRK-ERRMSG(27:)
               WHEN    "0054"
                   MOVE    "動脈注射手技料が同時算定されています。"
                                       TO  WRK-ERRMSG
               WHEN    "0055"
                   MOVE    "中心静脈注射料が同時算定されています。"
                                       TO  WRK-ERRMSG
               WHEN    "0056"
                   MOVE    "中心静脈注射料が算定できる日があります。"
                                       TO  WRK-ERRMSG
      *
               WHEN    "0066"
                   STRING  "中心静脈注射料が重複算定"
                           "している日があります。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
      *
               WHEN    "0057"
                   MOVE
                   "麻毒・生物学的製剤加算が算定できる日があります。"
                                       TO  WRK-ERRMSG
               WHEN    "0058"
                   STRING  "麻毒・生物学的製剤加算のみ算定している日が"
                           "あります。確認して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
               WHEN    "0060"
                   STRING  "複数科のまとめ入力分です。"
                           "全科表示にしてから変更して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
               WHEN    "0061"
                   MOVE    "一括回数変更内容にエラーがあります。"
                                       TO  WRK-ERRMSG
      *H30.8
               WHEN    "0065"
                   MOVE    "日指定の前に回数の指定をして下さい。"
                                       TO  WRK-ERRMSG
      *
               WHEN    "0062"
                   MOVE    "月末日以降の日があります。"
                                       TO  WRK-ERRMSG
               WHEN    "0063"
                   MOVE    "日付が重複しています。"
                                       TO  WRK-ERRMSG
               WHEN    "0064"
                   MOVE    "更新すべき収納はありません。"
                                       TO  WRK-ERRMSG
      *
               WHEN    "0070"
                   MOVE    "現在までの修正分が未登録です。"
                                       TO  WRK-ERRMSG
                   MOVE    "登録して下さい。"
                                       TO  WRK-ERRMSG(31:)
      *
               WHEN    "0071"
                   MOVE    "プレビューできる診療登録がありません。"
                                       TO  WRK-ERRMSG
                   MOVE    "プレビューはできません。"
                                       TO  WRK-ERRMSG(39:)
               WHEN    "0072"
                   MOVE    "労災・自賠責はプレビューできません。"
                                       TO  WRK-ERRMSG
               WHEN    "0073"
                   STRING  "労災・自賠責・公害が複数あります。"
                           "保険を選択して下さい。"
                                       DELIMITED   BY  SIZE
                                       INTO   WRK-ERRMSG
                   END-STRING
               WHEN    "0074"
                   STRING  "保険が複数あります。"
                           "入院料の保険を選択して下さい。"
                                       DELIMITED   BY  SIZE
                                       INTO   WRK-ERRMSG
                   END-STRING
      *
               WHEN    "0075"
                   STRING  "変更前と変更後が同日です。"
                           "診療日変更はできません。"
                                       DELIMITED   BY  SIZE
                                       INTO   WRK-ERRMSG
                   END-STRING
      *
               WHEN    "0076"
                   STRING  "他端末で使用中です。" 
                           "診療日変更はできません。"
                                       DELIMITED   BY  SIZE
                                       INTO   WRK-ERRMSG
                   END-STRING
      *
               WHEN    "K077"
                   STRING  "警告！！"
                           "当月の入院料が算定されていません。"
                           "入院料包括チェックはできません。"
                                       DELIMITED   BY  SIZE
                                       INTO   WRK-ERRMSG
                   END-STRING
      *
               WHEN    "0078"
                   STRING  "既に変更後受診日の連番が最大数まで登録済み" 
                           "です。診療日の追加はできません。"
                                       DELIMITED   BY  SIZE
                                       INTO   WRK-ERRMSG
                   END-STRING
      *ver.4.7
      *
               WHEN    "0080"
                   STRING  "同日再入院日ではありません。" 
                                       DELIMITED   BY  SIZE
                                       INTO   WRK-ERRMSG
                   END-STRING
               WHEN    "0081"
                   STRING  "同日複数履歴がありません。" 
                                       DELIMITED   BY  SIZE
                                       INTO   WRK-ERRMSG
                   END-STRING
               WHEN    "0083"
                   STRING  "同日再入院日に診療日変更はできません。"
                           "診療回数変更を行って下さい。"
                                       DELIMITED   BY  SIZE
                                       INTO   WRK-ERRMSG
                   END-STRING
      *
               WHEN    "2000"
                   MOVE    "更新エラーです。"
                                       TO  WRK-ERRMSG
               WHEN    "2001"
                   MOVE    "受診履歴の更新エラーです。"
                                       TO  WRK-ERRMSG
               WHEN    "2002"
                   MOVE    "受診履歴の新規エラーです。"
                                       TO  WRK-ERRMSG
               WHEN    "2003"
                   MOVE    "新規作成エラーです。"
                                       TO  WRK-ERRMSG
               WHEN    "2004"
                   MOVE    "削除エラーです。"
                                       TO  WRK-ERRMSG
      *H25.1
               WHEN    "2009"
                   STRING  "収納明細の更新エラーです。"
                           "再計算できませんでした。"
                                       DELIMITED   BY  SIZE
                                       INTO   WRK-ERRMSG
                   END-STRING
      *
               WHEN    "1005"
                   MOVE    "受診履歴番号が存在しません"
                                       TO  WRK-ERRMSG
               WHEN    "1006"
                   MOVE    "受診履歴の伝票番号がゼロのものがあります。"
                                       TO  WRK-ERRMSG
                   MOVE    "変更できません。"
                                       TO  WRK-ERRMSG(43:)
               WHEN    "1007"
                   MOVE    "内分泌負荷検査の月回数上限オーバーです。"
                                       TO  WRK-ERRMSG
                   MOVE    "変更できません。"
                                       TO  WRK-ERRMSG(41:)
               WHEN    "1008"
                   MOVE    "内分泌負荷試験が存在します。"
                                       TO  WRK-ERRMSG
                   MOVE    "追加はできません。"
                                       TO  WRK-ERRMSG(29:)
               WHEN    "1009"
                   MOVE    "受診履歴に存在する剤があります。"
                                       TO  WRK-ERRMSG
                   MOVE    "削除はできません。"
                                       TO  WRK-ERRMSG(33:)
               WHEN    "K100"
                   STRING  "警告！１回の受診履歴の剤が１３５以上に"
                       "なる日があります。受診履歴に登録できません"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
               WHEN    "K060"
                   STRING  "警告！受診履歴の連番が３以上になります。"
                           "訂正での展開はできません。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
      *
               WHEN    "K001"
                   STRING  "受診日変更により当日の受診回数が"
                           "変更されました。診察料を確認して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
               WHEN    "K002"
                   STRING  "収納が変更されました。"
                           "収納を確認して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
               WHEN    "K003"
                   MOVE    "警告！！月上限回数をオーバーしています。"
                                       TO  WRK-ERRMSG
               WHEN    "K004"
                   MOVE    "警告！！日上限回数をオーバーしています。"
                                       TO  WRK-ERRMSG
               WHEN    "K006"
                   MOVE    "警告！！週上限回数をオーバーしています。"
                                       TO  WRK-ERRMSG
               WHEN    "K005"
                   STRING  "警告！！"
                       "保険組合せ有効期間外の調剤料が算定されました。"
                       "保険一括変更をして下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-ERRMSG
                   END-STRING
      *H25.2
               WHEN    "7011"
                   STRING  "レセプト電算データから移行したデータです。"
                           "変更できません。"
                                           DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
      *@@
               WHEN    "9009"
                   STRING  "レセプトの４月改正は未対応です。"
                           "４月診療以降のプレビューはできません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *@@
      *
               WHEN    "9999"
                   MOVE    "他端末で使用中です。"
                                               TO  WRK-ERRMSG
                   MOVE    SLOCK-MSG           TO  WRK-ERRMSG(21:)
               WHEN    "9998"
                   STRING  "他端末で使用中です。" 
                           "更新はできません。"    DELIMITED  BY  SIZE
                           SLOCK-MSG               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
                   END-STRING
      *
               WHEN    OTHER
                   MOVE    SPA-ERRCD
                                       TO  WRK-ERRMSG
      *
           END-EVALUATE
           .
       5101-ERRMSG-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-JIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-JIDCD
               WHEN    "0101"
                   STRING  "剤が選択されています。"
                           "ＯＫで選択内容はクリアされます。"
                                       DELIMITED   BY  SIZE
                                       INTO  WRK-JIDMSG
                   END-STRING
               WHEN    "0102"
                   STRING  "剤が修正されています。"
                           "ＯＫで修正分は反映されずに終了します。"
                                       DELIMITED   BY  SIZE
                                       INTO  WRK-JIDMSG
                   END-STRING
               WHEN    "0103"
                   STRING  "剤選択されました。"
                           "現在選択中の内容はクリアされます。"
                                       DELIMITED   BY  SIZE
                                       INTO  WRK-JIDMSG
                   END-STRING
               WHEN    "0104"
                   STRING  "現在までの修正分を登録後、受診履歴の選択で"
                           "当日の剤内容を追加修正します"
                                       DELIMITED   BY  SIZE
                                       INTO  WRK-JIDMSG
                   END-STRING
      *
               WHEN    "0134"
                   STRING  "現在までの修正分を登録後、"
                           "剤新規登録へ遷移します。"
                                               DELIMITED  BY SIZE
                                               INTO   WRK-JIDMSG
                   END-STRING
      *
               WHEN    "0106"
                   STRING   "診療年月が変更されました。"
                            "現在までの修正分を登録します。"
                                       DELIMITED   BY  SIZE
                                       INTO  WRK-JIDMSG
                   END-STRING
               WHEN    "0107"
                   STRING  "診療科が変更されました。現在までの修正分を"
                           "登録します。"
                                       DELIMITED   BY  SIZE
                                       INTO  WRK-JIDMSG
                   END-STRING
               WHEN    "0108"
                   STRING  "診療年月が変更されました。現在までの修正"
                           "分を登録します。"
                                       DELIMITED   BY  SIZE
                                       INTO  WRK-JIDMSG
                   END-STRING
               WHEN    "0109"
                   STRING  "保険一括変更が選択されました。"
                           "現在までの修正分を登録します。"
                                       DELIMITED   BY  SIZE
                                       INTO  WRK-JIDMSG
                   END-STRING
               WHEN    "0110"
                   STRING  "診療区分が変更されました。"
                           "現在までの修正分を登録します。"
                                       DELIMITED   BY  SIZE
                                       INTO  WRK-JIDMSG
                   END-STRING
               WHEN    "0111"
                   STRING  "保険組合せが変更されました。"
                           "現在までの修正分を登録します。"
                                       DELIMITED   BY  SIZE
                                       INTO  WRK-JIDMSG
                   END-STRING
               WHEN    "0112"
                   STRING  "入外区分が変更されました。"
                           "現在までの修正分を登録します。"
                                       DELIMITED   BY  SIZE
                                       INTO  WRK-JIDMSG
                   END-STRING
               WHEN    "0113"
                   STRING  "現在までの修正分を登録後、"
                           "前頁を表示します。"
                                       DELIMITED   BY  SIZE
                                       INTO  WRK-JIDMSG
                   END-STRING
               WHEN    "0114"
                   STRING  "現在までの修正分を登録後、"
                           "次頁を表示します。"
                                       DELIMITED   BY  SIZE
                                       INTO  WRK-JIDMSG
                   END-STRING
               WHEN    "0115"
                   STRING  "診療日を変更します。現在までの修正分の"
                           "登録もしますので取消はできません。"
                                       DELIMITED   BY  SIZE
                                       INTO  WRK-JIDMSG
                   END-STRING
               WHEN    "0116"
                   STRING  "現在までの修正分を登録後、"
                           "カルテ３号紙を印刷します。"
                                       DELIMITED   BY  SIZE
                                       INTO  WRK-JIDMSG
                   END-STRING
               WHEN    "0117"
                   MOVE    "カルテ３号紙を印刷します。"
                                       TO  WRK-JIDMSG
               WHEN    "0118"
                   STRING  "まとめ入力分もすべて診療日変更します。"
                           "現在までの修正分の登録もします。"
                                       DELIMITED   BY  SIZE
                                       INTO  WRK-JIDMSG
                   END-STRING
               WHEN    "0119"
                   STRING  "月上限回数をオーバーするコードがあります。"
                           "追加後、回数を確認して下さい。"
                                       DELIMITED   BY  SIZE
                                       INTO  WRK-JIDMSG
                   END-STRING
               WHEN    "0131"
                   STRING  "週上限回数をオーバーするコードがあります。"
                           "追加後、回数を確認して下さい。"
                                       DELIMITED   BY  SIZE
                                       INTO  WRK-JIDMSG
                   END-STRING
               WHEN    "0120"
      *            内分泌負荷試験
                   MOVE    SPA-NAI-NAIBUNERR
                                       TO  WRK-JIDMSG
               WHEN    "0130"
                   STRING  "当月の回数がゼロの剤を削除します。"
                           "復元はできません。よろしいですか？"
                                       DELIMITED   BY  SIZE
                                       INTO  WRK-JIDMSG
                   END-STRING
      *
               WHEN    "0140"
                   STRING  "剤のない受診履歴を削除します。"
                           "よろしいですか？"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-JIDMSG
                   END-STRING
      *H25.4
               WHEN    "0149"
                   STRING  "表示しているレセ電移行データを削除します。"
                           "復元はできません。よろしいですか？"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-JIDMSG
                   END-STRING
      *
               WHEN    "0123"
                   STRING  "既に受診のある日です。"
                           "連番号を追加します。よろしいですか？"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-JIDMSG
                   END-STRING
      *
               WHEN    "0124"
                   STRING  "受診の連番号が３以上になります。"
                           "訂正での展開ができません。よろしいですか？"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-JIDMSG
                   END-STRING
      *
               WHEN    "0121"
                   STRING   "プレビューが押下されました。"
                            "現在までの修正分を登録します。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-JIDMSG
                   END-STRING
      *
               WHEN    "0125"
                   STRING   "健保のプレビューを行います。"
                            "労災・自賠責・公害は保険組合せを"
                            "選択して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-JIDMSG
                   END-STRING
      *
               WHEN    OTHER
                   MOVE    SPA-JIDCD
                                       TO  WRK-JIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-JID1-FLG
      *
           MOVE    SPACE               TO  JID1
           INITIALIZE                      JID1
           MOVE    SPA-JIDCD           TO  JID1-ID1CODE
           MOVE    WRK-JIDMSG          TO  JID1-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *H25.4
           EVALUATE    SPA-JIDCD
               WHEN    "0149"
                   MOVE    "B01"               TO  MCP-WIDGET
           END-EVALUATE
      *
           MOVE    "J02"               TO  SPA-MOTOPG
           MOVE    "JID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "JID1"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-JIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-JID2SET-SEC              SECTION.
      *
           MOVE    SPACE               TO  JID2
           INITIALIZE                      JID2
           EVALUATE    SPA-JIDCD2
               WHEN    "0115"
                   MOVE    "診療日の変更、または追加を選択して下さい。"
                                       TO  JID2-ID2MSG
                   STRING  "現在までの変更分の登録もしますので"
                           "取消はできません。"
                                       DELIMITED   BY  SIZE
                                       INTO  JID2-ID2MSG2
                   END-STRING
               WHEN    "0118"
                   STRING  "診療日の変更、または追加を選択して下さい。"
                           "まとめ入力分をすべて対象とします。"
                                       DELIMITED   BY  SIZE
                                       INTO  JID2-ID2MSG
                   END-STRING
                   STRING  "現在までの変更分の登録もしますので"
                           "取消はできません。"
                                       DELIMITED   BY  SIZE
                                       INTO  JID2-ID2MSG2
                   END-STRING
               WHEN    OTHER
                   MOVE    SPA-JIDCD2
                                       TO  JID2-ID2MSG
           END-EVALUATE
      *
           MOVE    ZERO                TO  SPA-JID2-FLG
      *
           MOVE    SPA-JIDCD2          TO  JID2-ID2CODE
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "J02"               TO  SPA-MOTOPG
           MOVE    "JID2"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "JID2"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-JID2SET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    システム管理マスタキー読み込み処理
      *****************************************************************
       9001-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
      *    システム管理検索
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC     TO  SYSKANRI-REC
               ELSE
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       9001-SYSKANRI-READ-SEC-EXT.
           EXIT.
      *
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
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    WRK-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    WRK-SRYYMD          TO  TNS-YUKOEDYMD
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
      *    検査正式名称
           IF      SPA-FORMALFLG       =   "1"
               IF     (TNS-SRYCD(1:1)      =   "1") AND
                      (TNS-SRYKBN          =   "60"
                                           OR  "64"   )  AND
                      (TNS-FORMALNAME  NOT =   SPACE )  AND
                      (TNS-FORMALNAME  NOT =   TNS-NAME )
                   MOVE    TNS-FORMALNAME  TO  TNS-NAME
               END-IF
           END-IF
      *    附加情報の上限回数設定
           IF     (FLG-TENSU           =   ZERO )  AND
                  (TNS-SRYCD(1:1)      =   "1"  )
               PERFORM 9101-TENSUPLUS-SYORI-SEC
           END-IF
      *
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
           MOVE    WRK-SRYYMD          TO  TNSP-YUKOSTYMD
           MOVE    WRK-SRYYMD          TO  TNSP-YUKOEDYMD
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
               MOVE    TNSP-JGNCNT1W       TO  TNS-JGNCNT1W
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
      *    診療会計マスター次読込
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
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
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       930-SRYACT-RAED-SEC         SECTION.
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
       930-SRYACT-RAED-EXT.
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
      *
      *****************************************************************
      *    患者コメントマスタ読み込み
      *****************************************************************
       950-PTCOM-READ-SEC              SECTION.
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
                   INITIALIZE                  PTCOM-REC
                   MOVE    1                   TO  FLG-PTCOM
               END-IF
           ELSE
               INITIALIZE                  PTCOM-REC
               MOVE    1                   TO  FLG-PTCOM
           END-IF
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       950-KNJCOM-READ-EXT.
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
      *********INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       970-JYURRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定歴マスタ読み込み
      *****************************************************************
       920-SANTEI-READ-SEC         SECTION.
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
      *    算定歴マスタ読み込み
      *****************************************************************
       920-SANTEIPLUS-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SANTEIPLUS-REC
               MOVE    ZERO                TO  FLG-SANTEIPLUS
           ELSE
               INITIALIZE                      SANTEIPLUS-REC
               MOVE    1                   TO  FLG-SANTEIPLUS
           END-IF
      *
           .
       920-SANTEIPLUS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードマスター読込
      *****************************************************************
       930-INPUTCD-READ-SEC         SECTION.
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  INPUTCD-REC
                   MOVE    ZERO                TO  FLG-INPUTCD
               ELSE
                   INITIALIZE                  INPUTCD-REC
                   MOVE    1                   TO  FLG-INPUTCD
               END-IF
           ELSE
               INITIALIZE                  INPUTCD-REC
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
      *
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       930-INPUTCD-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災保険情報読み込み
      *****************************************************************
       993-PTRSIINF-NEXT-SEC        SECTION.
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
      *    収納マスター読込
      *****************************************************************
       980-SYUNOU-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       980-SYUNOU-READ-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    パラメタ情報読み込み
      *****************************************************************
       990-PARA-READ-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PARA-REC
               MOVE    ZERO            TO  FLG-PARA
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           .
       990-PARA-READ-EXT.
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
      *
      *****************************************************************
      *    収納合計マスター読込
      *****************************************************************
       981-SYUTTL-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYUTTL-REC
               MOVE    ZERO                TO  FLG-SYUTTL
           ELSE
               INITIALIZE                      SYUTTL-REC
               MOVE    1                   TO  FLG-SYUTTL
           END-IF
      *
           .
       981-SYUTTL-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細マスター読込
      *****************************************************************
       982-SYUMEI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYUMEI-REC
               MOVE    ZERO                TO  FLG-SYUMEI
           ELSE
               INITIALIZE                      SYUMEI-REC
               MOVE    1                   TO  FLG-SYUMEI
           END-IF
      *
           .
       982-SYUMEI-READ-EXT.
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
      *TEST
      *****************************************************************
      *    一時保存テーブル読み込み
      *****************************************************************
       990-SPATMP-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 910-SPATMP-CALL-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA2-REC    TO  SPATMP-REC
               MOVE    ZERO            TO  FLG-SPATMP
           ELSE
               INITIALIZE                  SPATMP-REC
               MOVE    1               TO  FLG-SPATMP
           END-IF
      *
           .
       990-SPATMP-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-SPATMP-DBSELECT-SEC                SECTION.
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 910-SPATMP-CALL-SEC
      *
           .
       910-SPATMP-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-SPATMP-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 910-SPATMP-CALL-SEC
      *
           .
       990-SPATMP-DBCLOSE-EXT.
           EXIT.
      *****************************************************************
      *    一時保存テーブル検索処理
      *****************************************************************
       910-SPATMP-CALL-SEC                SECTION.
      *
           MOVE    "tbl_spa_tmp"       TO  MCP-TABLE
      *
           CALL    "ORCDBSPATMP"       USING   MCPAREA
                                               MCPDATA2-REC
                                               SPA-AREA
      *
           .
       910-SPATMP-CALL-EXT.
           EXIT.
      *TEST
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
      *
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
      *
      *****************************************************************
      *    ファイル削除処理
      *****************************************************************
       999-FILE-DEL-SEC         SECTION.
      *
      **** MOVE    SPA-TERMID          TO  FILE-TERMID
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM
      *    MOVE    ZERO                TO  IF-RESULT
      *    MOVE    FILE-NAME1          TO  IF-FILENAME
      *    CALL    "orcfiledel"        USING
      ****                             ORCSFDELAREA
           MOVE    1                   TO  FLG-END2
      *
      *    パラメタ削除
           INITIALIZE                      PARA-REC
           MOVE    "J02"               TO  PARA-GYOUMUID
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
      *    パラメタ削除
           INITIALIZE                      SPATMP-REC
           MOVE    "J02"               TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 910-SPATMP-CALL-SEC
      *
           .
       999-FILE-DEL-EXT.
           EXIT.
      *
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
