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
      ************************************************************
      *         システム管理マスタ     (CPSK5001.INC)   *
      *         （５００１）                            *
      *         病棟マスタ                              *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SYS-5001-REC.
           03  SYS-5001-KEY.
               05  SYS-5001-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-5001-KANRICD            PIC  X(04).
      *区分コード
               05  SYS-5001-KBNCD              PIC  X(08).
      *有効開始年月日
               05  SYS-5001-STYUKYMD           PIC  X(08).
      *有効終了年月日
               05  SYS-5001-EDYUKYMD           PIC  X(08).
      *管理テーブル
           03  SYS-5001-TBL.
      *病棟番号
               05  SYS-5001-BTU-NUM            PIC  X(02).
      *病棟名称
               05  SYS-5001-BTU-NAME           PIC  X(20).
      *病棟短縮名称
               05  SYS-5001-BTU-TANNAME        PIC  X(10).
      *特定入院料
               05  SYS-5001-BTU-TOKTEI-NYUIN
                                               PIC  X(02).
      *病棟科
               05  SYS-5001-BTU-KA             PIC  X(02).
      *病棟種別
               05  SYS-5001-BTU-SBT
                                               PIC  X(02).
      *収容人数
               05  SYS-5001-BTU-NINZU          PIC  9(03).
      *平均在院日数区分
               05  SYS-5001-BTU-HEIKINZAIKBN
                                               PIC  X(02).
      *看護配置区分
               05  SYS-5001-BTU-KNGHAICHIKBN
                                               PIC  X(02).
      *看護師比率区分
               05  SYS-5001-BTU-KNGHIRITUKBN
                                               PIC  X(02).
      *看護補助比率区分
               05  SYS-5001-BTU-KNGHJOHAI
                                               PIC  X(02).
      *看護補助加算区分
               05  SYS-5001-BTU-KNGHJOKSN
                                               PIC  X(02).
      *夜間勤務等看護加算区分
               05  SYS-5001-BTU-YAKANKNGKSN
                                               PIC  X(02).
      *食堂加算区分
               05  SYS-5001-BTU-SYOKUDOKBN
                                               PIC  X(01).
      *看護師人数区分
               05  SYS-5001-BTU-KNGSISUKBN
                                               PIC  X(01).
      *保育士人数区分
               05  SYS-5001-BTU-HIKSISUKBN
                                               PIC  X(01).
      *入院基本料点数コード
               05  SYS-5001-BTU-KHNSRYCD
                                               PIC  X(09).
      *初期加算点数コード
               05  SYS-5001-BTU-KSNSRYCD
                                               PIC  X(09).
      *長期減算点数コード
               05  SYS-5001-BTU-GENSRYCD
                                               PIC  X(09).
      *病棟終了区分
               05  SYS-5001-BTU-EDKBN
                                               PIC  X(01).
      *加算領域
               05  SYS-5001-KSNAREA.
      *難病患者等特別入院診療加算
                 07  SYS-5001-NANBYO.
                   09  SYS-5001-NANBYO-CD      PIC X(09).
                   09  SYS-5001-NANBYO-KBN     PIC X(01).
      *特殊疾患入院施設管理加算
                 07  SYS-5001-TOKUSYU.
                   09  SYS-5001-TOKUSYU-CD     PIC X(09).
                   09  SYS-5001-TOKUSYU-KBN    PIC X(01).
      *療養病棟療養環境加算
      *（病室管理情報に移動 H15.02.27)
                 07  SYS-5001-YORYO              OCCURS  3.
                   09  SYS-5001-YORYO-CD       PIC X(09).
                   09  SYS-5001-YORYO-KBN      PIC X(01).
      *児童・思春期精神科入院医療管理加算
                 07  SYS-5001-JIDO.
                   09  SYS-5001-JIDO-CD        PIC X(09).
                   09  SYS-5001-JIDO-KBN       PIC X(01).
      *精神科応急入院施設管理加算
                 07  SYS-5001-SEISIN.
                   09  SYS-5001-SEISIN-CD      PIC X(09).
                   09  SYS-5001-SEISIN-KBN     PIC X(01).
      *看護配置加算
                 07  SYS-5001-KANGO              OCCURS  5.
                   09  SYS-5001-KANGO-CD       PIC X(09).
                   09  SYS-5001-KANGO-KBN      PIC X(01).
      *看護配置適合加算
                 07  SYS-5001-KNGTEK.
                   09  SYS-5001-KNGTEK-CD      PIC X(09).
                   09  SYS-5001-KNGTEK-KBN     PIC X(01).
      *複数医師及び看護配置適合加算
                 07  SYS-5001-DRTEK.
                   09  SYS-5001-DRTEK-CD       PIC X(09).
                   09  SYS-5001-DRTEK-KBN      PIC X(01).
      *入院料加算再定義
      *平成２０年４月以降は算定する加算コードが配列の１番目より
      *順次設定される。
               05  SYS-5001-KSNTBLR  REDEFINES SYS-5001-KSNAREA.
                 07  SYS-5001-KSN              OCCURS  14.
                   09  SYS-5001-KSNCD          PIC X(09).
                   09  SYS-5001-KSNKBN         PIC X(01).
      *入院料加算（ユーザ設定領域）
               05  SYS-5001-KSN2.
                 09  SYS-5001-KSNCD2           PIC X(09) OCCURS 10.
      *予備 ** 予備の後にも項目があるので注意すること **
               05  SYS-5001-YOBI               PIC X(130).
      *看護職員比率過措置区分（平成３０年改正　療養病棟入院料２の経過措置）
               05  SYS-5001-SYOKUINKBN2    PIC  X(01).
      *重症患者割合特別入院基本料区分（平成３０年改正　結核病棟）
               05  SYS-5001-JUUSHOUKNGKBN  PIC  X(01).
      *病棟診療行為コード（特定入院料）
               05  SYS-5001-TSRYCD         PIC  X(09).
      *病棟診療行為コード
               05  SYS-5001-SRYCD          PIC  X(09).
      *看護職員数等経過措置区分
               05  SYS-5001-SYOKUINKBN     PIC  X(01).
      *夜勤時間特別入院基本料区分
               05  SYS-5001-YKNTOKUKBN     PIC  X(01).
      *月平均夜勤時間超過減算
               05  SYS-5001-YKNCHOKAKBN    PIC  X(01).
      *診療所一般・療養病床相互算定区分
      *      病棟種別＝08（有床一般）
      *      空白：相互算定しない
      *        ０：相互算定しない
      *        １：相互算定する
      *      病棟種別＝09（有床療養）
      *      空白：相互算定しない
      *        ０：相互算定しない
      *        １：相互算定する（有床診療所入院基本料１）
      *        ２：相互算定する（有床診療所入院基本料２）
      *        ３：相互算定する（有床診療所入院基本料３）
               05  SYS-5001-CRYOYOKBN          PIC X(01).
      *一般病棟９０日超算定 *      空白：特定入院基本料を算定
      *        ０：特定入院基本料を算定
      *        １：療養病棟入院基本料１の例により算定
               05  SYS-5001-HRYOYOKBN          PIC X(01).
               05  SYS-5001-KHNSRYCD           PIC X(09).
               05  SYS-5001-KHNKBNCD           PIC X(03).
      *選定入院特別料金（一般）
               05  SYS-5001-SENTEI-IPN-KINGAKU-X
                                               PIC  X(04).
               05  SYS-5001-SENTEI-IPN-KINGAKU-R
                                           REDEFINES
                   SYS-5001-SENTEI-IPN-KINGAKU-X.
                   07  SYS-5001-SENTEI-IPN-KINGAKU
                                               PIC  9(04).
      *選定入院特別料金（老人）
               05  SYS-5001-SENTEI-RJN-KINGAKU-X
                                               PIC  X(04).
               05  SYS-5001-SENTEI-RJN-KINGAKU-R
                                           REDEFINES
                   SYS-5001-SENTEI-RJN-KINGAKU-X.
                   07  SYS-5001-SENTEI-RJN-KINGAKU
                                               PIC  9(04).
      *選定入院設定区分
               05  SYS-5001-BTU-ISISUKBN       PIC  X(01).
      *選定入院設定区分
               05  SYS-5001-SENTEI-FTNKBN      PIC  X(01).
      *選定入院消費税区分
               05  SYS-5001-SENTEI-TAXKBN      PIC  X(01).
      *選定入院点数単価
               05  SYS-5001-SENTEI-TANKA-X     PIC  X(02).
               05  SYS-5001-SENTEI-TANKA-R     REDEFINES
                   SYS-5001-SENTEI-TANKA-X.
                   07  SYS-5001-SENTEI-TANKA   PIC  9(02).
      *選定入院特別料金（共通）
               05  SYS-5001-SENTEI-KINGAKU-X   PIC  X(04).
               05  SYS-5001-SENTEI-KINGAKU-R   REDEFINES
                   SYS-5001-SENTEI-KINGAKU-X.
                   07  SYS-5001-SENTEI-KINGAKU PIC  9(04).
      *レセプト表示区分
               05  SYS-5001-RECEDSPKBN         PIC  X(02).
      *端末ＩＤ
           03  SYS-5001-TERMID                 PIC  X(16).
      *オペーレータＩＤ
           03  SYS-5001-OPID                   PIC  X(16).
      *作成年月日
           03  SYS-5001-CREYMD                 PIC  X(08).
      *更新年月日
           03  SYS-5001-UPYMD                  PIC  X(08).
      *更新時間
           03  SYS-5001-UPHMS                  PIC  X(06).
      *主キー更新用項目
           03  SYS-5001-UPD-KEY.
               05  SYS-5001-UPD-HOSPNUM     PIC 9(2).
               05  SYS-5001-UPD-KANRICD     PIC X(04).
               05  SYS-5001-UPD-KBNCD       PIC X(08).
               05  SYS-5001-UPD-STYUKYMD    PIC X(08).
               05  SYS-5001-UPD-EDYUKYMD    PIC X(08).
