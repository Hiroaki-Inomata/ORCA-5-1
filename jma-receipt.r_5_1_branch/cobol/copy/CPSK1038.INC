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
      *         システム管理マスタ     (CPSK1038.INC)   *
      *         （１０３８）                            *
      *         診療行為一括入金管理情報                *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  04/07/05  請求書発行方法追加
      *  01.00.02    NACL-多々納  06/07/11  ＤＯ初期表示他追加
      *  03.04.00    NACL-多々納  06/11/13  検査正式名称表示追加
      *  04.01.00    NACL-多々納  07/10/25  腫瘍マーカ一覧表示追加
      *  04.01.00    NACL-多々納  07/12/XX  投薬種別変換有無追加
      *                                     指導料と初診料の警告追加
      *  04.03.00    NACL-多々納  08/07/04  お薬手帳連動追加
      *  04.03.00    NACL-多々納  08/08/20  レセプトブレビュー区分追加
      *  04.03.00    NACL-多々納  08/11/04  薬情・お薬手帳対象区分追加
      *  04.06.00    NACL-多々納  10/07/01  残量廃棄区分追加
      *  04.06.00    NACL-多々納  10/08/03  入金・返金額設定区分追加
      *  04.06.00    NACL-多々納  10/11/10  発行日変更区分追加
      *  04.07.00    NACL-多々納  11/07/10  リハビリ・患者情報表示区分追加
      *  04.07.00    NACL-多々納  12/01/25  入力スクロール背景色追加
      *  04.07.00    NACL-多々納  12/04/27  確認画面名称表示追加
      *  04.07.00    NACL-多々納  12/11/21  レセプトプレビューレイアウト追加
      *  04.08.00    NACL-多々納  16/06/XX  セット展開区分追加
      *  04.08.00    NACL-多々納  17/07/XX  心身療法２０未満加算区分追加
      *  04.08.00    NACL-多々納  18/09/XX  選択式コメント一覧表示追加
      ************************************************************
       01  SYS-1038-REC.
           03  SYS-1038-KEY.
               05  SYS-1038-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-1038-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-1038-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-1038-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-1038-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  SYS-1038-TBL.
      *入金管理区分
               05  SYS-1038-NYKNKNRKBN     PIC  X(01).
      *複数科まとめ集計
               05  SYS-1038-KAGRPFLG       PIC  X(01).
      *請求書発行方法
               05  SYS-1038-HAKHOUFLG      PIC  X(01).
      *ＤＯ初期表示
               05  SYS-1038-DOSRYKBNFLG    PIC  X(02).
      *中途表示一覧
               05  SYS-1038-WKSRYHYOFLG    PIC  X(01).
      *検査正式名称表示
               05  SYS-1038-FORMALFLG      PIC  X(01).
      *同日再診・訂正選択
               05  SYS-1038-TEISEIFLG      PIC  X(01).
      *皮下筋肉注射料変換選択
               05  SYS-1038-CHG310FLG      PIC  X(01).
      *訂正請求書金額選択
               05  SYS-1038-TEIKINKBN      PIC  X(01).
      *薬剤情報提供発行
               05  SYS-1038-YKJCHKKBN      PIC  X(01).
      *腫瘍マーカ一覧表示
               05  SYS-1038-AKUSEIHYOFLG   PIC  X(01).
      *投薬診療種別変換選択
               05  SYS-1038-CHGSRYSYU-FLG  PIC  X(01).
      *お薬手帳発行
               05  SYS-1038-OKUSURIKBN     PIC  X(01).
      *レセプトブレビュー
               05  SYS-1038-RESPRVFLG     PIC  X(01).
      *薬情・お薬手帳対象
               05  SYS-1038-YKJOKUKBN     PIC  X(01).
      *残量廃棄区分
               05  SYS-1038-HAIKIKBN      PIC  X(01).
      *入金・返金額設定区分
               05  SYS-1038-NYKHNKKBN     PIC  X(01).
      *精神療法２０未満加算区分
               05  SYS-1038-SEI20KSNKBN   PIC  X(01).
      *発行日変更区分
               05  SYS-1038-YMDHENKBN     PIC  X(01).
      *H24.4
      *確認画面名称表示区分
               05  SYS-1038-GENERICFLG    PIC  X(01).
      *
      *リハビリ・患者情報表示区分
               05  SYS-1038-GMNHYOKBN-G.
                   07  SYS-1038-GMNHYOKBN     PIC  X(01)
                                              OCCURS   5.
      *入力スクロール背景色
               05  SYS-1038-BGCOLORS       PIC  X(16).
      *入力スクロール背景行区分
               05  SYS-1038-BGLINEKBN      PIC  X(01).
      *環境設定背景色変更
               05  SYS-1038-M95BGCOLORS-G.
      *            標準
                   07  SYS-1038-BGCOLORSJ0      PIC  X(16).
      *            システム日付変更
                   07  SYS-1038-BGCOLORSDAY     PIC  X(16).
      *            時間外区分
                   07  SYS-1038-BGCOLORSJ-G.
                       09 SYS-1038-BGCOLORSJ   PIC  X(16)
                                               OCCURS   8.
      *エラー背景色
               05  SYS-1038-ERRCOLORS       PIC  X(16).
      *警告背景色
               05  SYS-1038-KEICOLORS       PIC  X(16).
      *レセプトブレビュー　レイアウト
               05  SYS-1038-PRVLAYOUT       PIC  X(01).
      *セット展開区分
               05  SYS-1038-SETALLKBN       PIC  X(01).
      *H28.10
      *リハビリ・患者情報・介護表示区分６
               05  SYS-1038-GMNHYOKBN-6    PIC  X(01).
      *H29.7
      *心身療法２０未満加算区分
               05  SYS-1038-SIN20KSNKBN    PIC  X(01).
      *H30.9
      *選択式コメント一覧区分
               05  SYS-1038-RECEKISAIFLG   PIC  X(01).
      *予備 
      ******   05                          PIC  X(488).
      ******   05  SYS-1038-YOBI           PIC  X(480).
      ****     05  SYS-1038-YOBI           PIC  X(454).
      ****     05  SYS-1038-YOBI           PIC  X(253).
      *****    05  SYS-1038-YOBI           PIC  X(213).
      ****     05  SYS-1038-YOBI           PIC  X(259).
               05  SYS-1038-YOBI           PIC  X(256).
      *端末ＩＤ
           03  SYS-1038-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-1038-OPID            PIC  X(16).
      *作成年月日
           03  SYS-1038-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-1038-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-1038-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-1038-UPD-KEY.
               05  SYS-1038-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1038-UPD-KANRICD     PIC X(04).
               05  SYS-1038-UPD-KBNCD       PIC X(08).
               05  SYS-1038-UPD-STYUKYMD    PIC X(08).
               05  SYS-1038-UPD-EDYUKYMD    PIC X(08).
