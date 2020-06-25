      ******************************************************************
      *    CPJIDO （XXXXXXXXXXXXXX)
      ******************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  02.00.00    NACL-多々納  04/03/19  改正対応
      ************************************************************
       01  CPJIDOTBL.
           02  JIDOTBL-T.
             03  JIDOTBL-T1.
      *　処方せん料（７種類以上）
               05  FILLER              PIC  X(9)   VALUE   "180016410" .
      *　 処方せん料（その他）
               05  FILLER              PIC  X(9)   VALUE   "180001110" .
      *　 薬剤料逓減（９０／１００）（内服薬）
               05  FILLER              PIC  X(9)   VALUE   "630010002" .
      *　調剤料（内服薬・浸煎薬・屯服薬）
               05  FILLER              PIC  X(9)   VALUE   "120000710" .
      *　 調剤料（外用薬）
               05  FILLER              PIC  X(9)   VALUE   "120001010" .
      *　処方料（７種類以上）
               05  FILLER              PIC  X(9)   VALUE   "120002610" .
      *　 処方料（その他）
               05  FILLER              PIC  X(9)   VALUE   "120001210" .
      *　調剤料（麻・向・覚・毒）（入院外）
               05  FILLER              PIC  X(9)   VALUE   "120000110" .
      *　処方料（麻・向・覚・毒）
               05  FILLER              PIC  X(9)   VALUE   "120001310" .
      *　調基（その他）
               05  FILLER              PIC  X(9)   VALUE   "120001810" .
      *　薬剤情報提供料
               05  FILLER              PIC  X(9)   VALUE   "120002370" .
      *　処方せん（乳幼児）加算
               05  FILLER              PIC  X(9)   VALUE   "120002470" .
      *　老人薬剤情報提供料（健康手帳に記載する場合）
               05  FILLER              PIC  X(9)   VALUE   "113701310" .
      *　老人薬剤情報提供料（健康手帳に記載しない場合）
               05  FILLER              PIC  X(9)   VALUE   "113701410" .
      *　老人薬剤情報提供料（健康手帳に記載する場合）
               05  FILLER              PIC  X(9)   VALUE   "113701310" .
      *　老人薬剤情報提供料（健康手帳に記載しない場合）
               05  FILLER              PIC  X(9)   VALUE   "113701410" .
      *　（減）
               05  FILLER              PIC  X(9)   VALUE   "820000047" .
      *　尿・糞便等検査判断料
               05  FILLER              PIC  X(9)   VALUE   "160061710" .
      *　尿・糞便等検査判断料
               05  FILLER              PIC  X(9)   VALUE   "160700510" .
      *　血液学的検査判断料
               05  FILLER              PIC  X(9)   VALUE   "160061810" .
      *　血液学的検査判断料
               05  FILLER              PIC  X(9)   VALUE   "160700610" .
      *　生化学的検査（１）判断料
               05  FILLER              PIC  X(9)   VALUE   "160061910" .
      *　生化学的検査（１）判断料
               05  FILLER              PIC  X(9)   VALUE   "160700710" .
      *　生化学的検査（２）判断料
               05  FILLER              PIC  X(9)   VALUE   "160062010" .
      *　生化学的検査（２）判断料
               05  FILLER              PIC  X(9)   VALUE   "160700810" .
      *　免疫学的検査判断料
               05  FILLER              PIC  X(9)   VALUE   "160062110" .
      *　免疫学的検査判断料
               05  FILLER              PIC  X(9)   VALUE   "160700910" .
      *　微生物学的検査判断料
               05  FILLER              PIC  X(9)   VALUE   "160062210" .
      *　微生物学的検査判断料
               05  FILLER              PIC  X(9)   VALUE   "160701010" .
      *　病理学的検査判断料
               05  FILLER              PIC  X(9)   VALUE   "160062310" .
      *　呼吸機能検査等判断料
               05  FILLER              PIC  X(9)   VALUE   "160146910" .
      *　脈波図、心機図、ポリグラフ検査判断料
               05  FILLER              PIC  X(9)   VALUE   "160147010" .
      *　脳波検査判断料
               05  FILLER              PIC  X(9)   VALUE   "160147610" .
      *　神経・筋検査判断料
               05  FILLER              PIC  X(9)   VALUE   "160147710" .
      *　ラジオアイソトープ検査判断料
               05  FILLER              PIC  X(9)   VALUE   "160147910" .
      ******** 動脈血採血
      *        05  FILLER              PIC  X(9)   VALUE   "160101210" .
      *        静脈血採血
      *        05  FILLER              PIC  X(9)   VALUE   "160095710" .
      *        抹消採血
      ******** 05  FILLER              PIC  X(9)   VALUE   "160095810" .
      *Ｈ１４．４改正分
      *        処方せん料
      *　処方せん料（７種類以上）（後発医薬品を含まない）
               05  FILLER              PIC  X(9)   VALUE   "120002810" .
      *　処方せん料（その他）（後発医薬品を含まない）
               05  FILLER              PIC  X(9)   VALUE   "120003010" .
      *　処方せん料（７種類以上）（後発医薬品を含む
               05  FILLER              PIC  X(9)   VALUE   "120002710" .
      *　処方せん料（その他）（後発医薬品を含む）
               05  FILLER              PIC  X(9)   VALUE   "120002910" .
      *　
      *入院分
      *　調基（入院）
               05  FILLER              PIC  X(9)   VALUE   "120001710" .
      *　 調剤料（入院）
               05  FILLER              PIC  X(9)   VALUE   "120001110" .
      *　調剤料（麻・向・覚・毒）（入院）
               05  FILLER              PIC  X(9)   VALUE   "120000410" .
      *
           02  JIDOTBL-R  REDEFINES   JIDOTBL-T.
      *******03  JIDOTBL-TX           OCCURS  38.
      *******03  JIDOTBL-TX           OCCURS  42.
      ****** 03  JIDOTBL-TX           OCCURS  39.
             03  JIDOTBL-TX           OCCURS  42.
               05  JIDOTBL-CODE        PIC  X(9).
       01  JIDO-MAX                PIC 9(04)   VALUE   42.
      *
