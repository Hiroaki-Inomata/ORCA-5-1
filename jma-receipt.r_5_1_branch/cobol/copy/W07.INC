        02  W07.
    ********02  W07-FIXED1.
              03  W07-B03.
                04  W07-B03-STATE   PIC S9(9)   BINARY.
              03  W07-YUKOSTYMD.
                04  W07-VALUE   PIC X(9).
              03  W07-YUKOEDYMD.
                04  W07-VALUE   PIC X(9).
              03  W07-NOTEBOOK1.
                04  W07-PAGENO  PIC S9(9)   BINARY.
                04  W07-TAG1.
                  05  W07-DATA01-COMBO.
                    06  W07-DATA01.
                      07  W07-VALUE   PIC X(12).
                    06  W07-DATA01-COUNT   PIC S9(9)   BINARY.
                    06  W07-DATA01-ITEM
                            PIC X(12)   OCCURS  2 TIMES.
                  05  W07-DATA02-COMBO.
                    06  W07-DATA02.
                      07  W07-VALUE   PIC X(22).
                    06  W07-DATA02-COUNT   PIC S9(9)   BINARY.
                    06  W07-DATA02-ITEM
                            PIC X(22)   OCCURS  3 TIMES.
                  05  W07-DATA03-COMBO.
                    06  W07-DATA03.
                      07  W07-VALUE   PIC X(22).
                    06  W07-DATA03-COUNT   PIC S9(9)   BINARY.
                    06  W07-DATA03-ITEM
                            PIC X(22)   OCCURS  3 TIMES.
                  05  W07-DATA04-COMBO.
                    06  W07-DATA04.
                      07  W07-VALUE   PIC X(22).
                    06  W07-DATA04-COUNT   PIC S9(9)   BINARY.
                    06  W07-DATA04-ITEM
                            PIC X(22)   OCCURS  3 TIMES.
                  05  W07-DATA05-COMBO.
                    06  W07-DATA05.
                      07  W07-VALUE   PIC X(12).
                    06  W07-DATA05-COUNT   PIC S9(9)   BINARY.
                    06  W07-DATA05-ITEM
                            PIC X(12)   OCCURS  2 TIMES.
                  05  W07-DATA06-COMBO.
                    06  W07-DATA06.
                      07  W07-VALUE   PIC X(12).
                    06  W07-DATA06-COUNT   PIC S9(9)   BINARY.
                    06  W07-DATA06-ITEM
                            PIC X(12)   OCCURS  2 TIMES.
                  05  W07-DATA07-COMBO.
                    06  W07-DATA07.
                      07  W07-VALUE   PIC X(12).
                    06  W07-DATA07-COUNT   PIC S9(9)   BINARY.
                    06  W07-DATA07-ITEM
                            PIC X(12)   OCCURS  2 TIMES.
                  05  W07-SRYKACD-G.
                    06  W07-SRYKACD-STATE   PIC S9(9)   BINARY.
                    06  W07-SRYKACD         PIC X(2).
                  05  W07-SRYKAMEI.
                    06  W07-VALUE   PIC X(20).
                  05  W07-NYUSYOSIN-COMB.
                    06  W07-NYUSYOSIN.
                      07  W07-VALUE   PIC X(12).
                    06  W07-NYUSYOSIN-COUNT   PIC S9(9)   BINARY.
                    06  W07-NYUSYOSIN-ITEM    PIC X(12)   OCCURS  2.
                  05  W07-KIKAN.
                    06  W07-VALUE   PIC S9(2).
                  05  W07-KINKCHK-COMBO.
                    06  W07-KINKCHK.
                      07  W07-VALUE   PIC X(40).
                    06  W07-KINKCHK-COUNT   PIC S9(9)   BINARY.
                    06  W07-KINKCHK-ITEM    PIC X(40)   OCCURS  3.
                  05  W07-GAIKANKBN-COMBO.
                    06  W07-GAIKANKBN.
                      07  W07-VALUE   PIC X(32).
                    06  W07-GAIKANKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-GAIKANKBN-ITEM    PIC X(32)   OCCURS  9.
                  05  W07-GAIMOVE-COMBO.
                    06  W07-GAIMOVE.
                      07  W07-VALUE   PIC X(22).
                    06  W07-GAIMOVE-COUNT   PIC S9(9)   BINARY.
                    06  W07-GAIMOVE-ITEM    PIC X(22)   OCCURS  9 TIMES.
                  05  W07-HOUKATU-COMBO.
                    06  W07-HOUKATU.
                      07  W07-VALUE   PIC X(30).
                    06  W07-HOUKATU-COUNT   PIC S9(9)   BINARY.
                    06  W07-HOUKATU-ITEM    PIC X(30)   OCCURS  2 TIMES.
                  05  W07-SURYOZERO-COMB.
                    06  W07-SURYOZERO.
                      07  W07-VALUE   PIC X(22).
                    06  W07-SURYOZERO-COUNT   PIC S9(9)   BINARY.
                    06  W07-SURYOZERO-ITEM    PIC X(22)   OCCURS  2.
                  05  W07-CHOKICHK-COMB.
                    06  W07-CHOKICHK.
                      07  W07-VALUE   PIC X(22).
                    06  W07-CHOKICHK-COUNT   PIC S9(9)   BINARY.
                    06  W07-CHOKICHK-ITEM    PIC X(22)   OCCURS  2.
                  05  W07-JIKANTOKUNYU-COMB.
                    06  W07-JIKANTOKUNYU.
                      07  W07-VALUE   PIC X(30).
                    06  W07-JIKANTOKUNYU-COUNT   PIC S9(9)   BINARY.
                    06  W07-JIKANTOKUNYU-ITEM
                            PIC X(30)   OCCURS  9 TIMES.
                  05  W07-GAZOUKSN1-COMB.
                    06  W07-GAZOUKSN1.
                      07  W07-VALUE   PIC X(12).
                    06  W07-GAZOUKSN1-COUNT   PIC S9(9)   BINARY.
                    06  W07-GAZOUKSN1-ITEM
                            PIC X(12)   OCCURS  2 TIMES.
                  05  W07-JIKANTOKU-CHK-COMB.
                    06  W07-JIKANTOKU-CHK.
                      07  W07-VALUE   PIC X(22).
                    06  W07-JIKANTOKU-CHK-COUNT   PIC S9(9)   BINARY.
                    06  W07-JIKANTOKU-CHK-ITEM
                            PIC X(22)   OCCURS  2 TIMES.
                  05  W07-JIKANW1-TIME  OCCURS  2.
                     06  W07-JIKANW1-HH-G.
                        07  W07-JIKANW1-HH-STATE   PIC S9(9)   BINARY.
                        07  W07-JIKANW1-HH     PIC X(2).
                     06  W07-JIKANW1-MM-G.
                        07  W07-JIKANW1-MM-STATE   PIC S9(9)   BINARY.
                        07  W07-JIKANW1-MM     PIC X(2).
                     06  W07-JIKANW2-HH-G.
                        07  W07-JIKANW2-HH-STATE   PIC S9(9)   BINARY.
                        07  W07-JIKANW2-HH     PIC X(2).
                     06  W07-JIKANW2-MM-G.
                        07  W07-JIKANW2-MM-STATE   PIC S9(9)   BINARY.
                        07  W07-JIKANW2-MM     PIC X(2).
                  05  W07-JIKANS1-TIME  OCCURS  2.
                     06  W07-JIKANS1-HH-G.
                        07  W07-JIKANS1-HH-STATE   PIC S9(9)   BINARY.
                        07  W07-JIKANS1-HH     PIC X(2).
                     06  W07-JIKANS1-MM-G.
                        07  W07-JIKANS1-MM-STATE   PIC S9(9)   BINARY.
                        07  W07-JIKANS1-MM     PIC X(2).
                     06  W07-JIKANS2-HH-G.
                        07  W07-JIKANS2-HH-STATE   PIC S9(9)   BINARY.
                        07  W07-JIKANS2-HH     PIC X(2).
                     06  W07-JIKANS2-MM-G.
                        07  W07-JIKANS2-MM-STATE   PIC S9(9)   BINARY.
                        07  W07-JIKANS2-MM     PIC X(2).
                  05  W07-LASTKIKAN.
                    06  W07-VALUE   PIC S9(2).
                  05  W07-CLEARCHK-COMBO.
                    06  W07-CLEARCHK.
                      07  W07-VALUE   PIC X(22).
                    06  W07-CLEARCHK-COUNT   PIC S9(9)   BINARY.
                    06  W07-CLEARCHK-ITEM
                            PIC X(22)   OCCURS  2 TIMES.
                  05  W07-CHUTOTENKAI-COMB.
                    06  W07-CHUTOTENKAI.
                      07  W07-VALUE   PIC X(12).
                    06  W07-CHUTOTENKAI-COUNT   PIC S9(9)   BINARY.
                    06  W07-CHUTOTENKAI-ITEM
                            PIC X(12)   OCCURS  2 TIMES.
                  05  W07-YAKJYOCHK-COMB.
                    06  W07-YAKJYOCHK.
                      07  W07-VALUE   PIC X(22).
                    06  W07-YAKJYOCHK-COUNT   PIC S9(9)   BINARY.
                    06  W07-YAKJYOCHK-ITEM
                            PIC X(22)   OCCURS  2 TIMES.
                  05  W07-ZENHKNCHK-COMB.
                    06  W07-ZENHKNCHK.
                      07  W07-VALUE   PIC X(30).
                    06  W07-ZENHKNCHK-COUNT   PIC S9(9)   BINARY.
                    06  W07-ZENHKNCHK-ITEM
                            PIC X(30)   OCCURS  3 TIMES.
                  05  W07-TEISEI-COMB.
                    06  W07-TEISEI.
                      07  W07-VALUE   PIC X(12).
                    06  W07-TEISEI-COUNT   PIC S9(9)   BINARY.
                    06  W07-TEISEI-ITEM
                            PIC X(12)   OCCURS  2 TIMES.
                  05  W07-TEISEITOKU-COMB.
                    06  W07-TEISEITOKU.
                      07  W07-VALUE   PIC X(12).
                    06  W07-TEISEITOKU-COUNT   PIC S9(9)   BINARY.
                    06  W07-TEISEITOKU-ITEM
                            PIC X(12)   OCCURS  2 TIMES.
                  05  W07-BYOHYOKBN-COMB.
                    06  W07-BYOHYOKBN.
                      07  W07-VALUE   PIC X(40).
                    06  W07-BYOHYOKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-BYOHYOKBN-ITEM
                            PIC X(40)   OCCURS  5 TIMES.
                  05  W07-ZAIJUNKBN-COMB.
                    06  W07-ZAIJUNKBN.
                      07  W07-VALUE   PIC X(16).
                    06  W07-ZAIJUNKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-ZAIJUNKBN-ITEM
                            PIC X(16)   OCCURS  2 TIMES.
                  05  W07-KENSAAUTO-COMB.
                    06  W07-KENSAAUTO.
                      07  W07-VALUE   PIC X(30).
                    06  W07-KENSAAUTO-COUNT   PIC S9(9)   BINARY.
                    06  W07-KENSAAUTO-ITEM
                            PIC X(30)   OCCURS  3 TIMES.
                  05  W07-BYOMEIAUTO-COMB.
                    06  W07-BYOMEIAUTO.
                      07  W07-VALUE   PIC X(12).
                    06  W07-BYOMEIAUTO-COUNT   PIC S9(9)   BINARY.
                    06  W07-BYOMEIAUTO-ITEM
                            PIC X(12)   OCCURS  2 TIMES.
                  05  W07-TEINYUAUTO-COMB.
                    06  W07-TEINYUAUTO.
                      07  W07-VALUE   PIC X(12).
                    06  W07-TEINYUAUTO-COUNT   PIC S9(9)   BINARY.
                    06  W07-TEINYUAUTO-ITEM
                            PIC X(12)   OCCURS  2 TIMES.
                  05  W07-TEISHOHOU-COMB.
                    06  W07-TEISHOHOU.
                      07  W07-VALUE   PIC X(12).
                    06  W07-TEISHOHOU-COUNT   PIC S9(9)   BINARY.
                    06  W07-TEISHOHOU-ITEM
                            PIC X(12)   OCCURS  2 TIMES.
                  05  W07-NYUKENSA-COMB.
                    06  W07-NYUKENSA.
                      07  W07-VALUE   PIC X(12).
                    06  W07-NYUKENSA-COUNT   PIC S9(9)   BINARY.
                    06  W07-NYUKENSA-ITEM
                            PIC X(12)   OCCURS  2 TIMES.
                  05  W07-INITAUTO-COMB.
                    06  W07-INITAUTO.
                      07  W07-VALUE   PIC X(12).
                    06  W07-INITAUTO-COUNT   PIC S9(9)   BINARY.
                    06  W07-INITAUTO-ITEM
                            PIC X(12)   OCCURS  2 TIMES.
                  05  W07-TECHOKBN-COMB.
                    06  W07-TECHOKBN.
                      07  W07-VALUE   PIC X(30).
                    06  W07-TECHOKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-TECHOKBN-ITEM
                            PIC X(30)   OCCURS  5 TIMES.
                  05  W07-JGAIKEIKBN-COMB.
                    06  W07-JGAIKEIKBN.
                      07  W07-VALUE   PIC X(30).
                    06  W07-JGAIKEIKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-JGAIKEIKBN-ITEM
                            PIC X(30)   OCCURS  5 TIMES.
                  05  W07-TOUYOCHK-COMB.
                    06  W07-TOUYOCHK.
                      07  W07-VALUE   PIC X(40).
                    06  W07-TOUYOCHK-COUNT   PIC S9(9)   BINARY.
                    06  W07-TOUYOCHK-ITEM
                            PIC X(40)   OCCURS  5 TIMES.
                  05  W07-SHOHOU30CHK-COMB.
                    06  W07-SHOHOU30CHK.
                      07  W07-TEXTDATA    PIC X(40).
                    06  W07-SHOHOU30CHK-COUNT   PIC S9(9)   BINARY.
                    06  W07-SHOHOU30CHK-ITEM
                            PIC X(40)   OCCURS  5 TIMES.
                04  W07-TAGNAME1.
                  05  W07-TAGNAME1-STATE   PIC S9(9)   BINARY.
                04  W07-TAG2.
                  05  W07-BYOMEIKBN-COMBO.
                    06  W07-BYOMEIKBN.
                      07  W07-VALUE   PIC X(28).
                    06  W07-BYOMEIKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-BYOMEIKBN-ITEM    PIC X(28)   OCCURS  3.
                  05  W07-JISYOKBN-COMBO.
                    06  W07-JISYOKBN.
                      07  W07-VALUE   PIC X(28).
                    06  W07-JISYOKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-JISYOKBN-ITEM    PIC X(28)   OCCURS  2.
                  05  W07-HAKKOKBN-COMBO.
                    06  W07-HAKKOKBN.
                      07  W07-VALUE   PIC X(28).
                    06  W07-HAKKOKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-HAKKOKBN-ITEM    PIC X(28)   OCCURS  3.
                  05  W07-HOJYOKBN-COMBO.
                    06  W07-HOJYOKBN.
                      07  W07-VALUE   PIC X(28).
                    06  W07-HOJYOKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-HOJYOKBN-ITEM    PIC X(28)   OCCURS  2.
                  05  W07-DOUMEI-CHKFLG-COMBO.
                    06  W07-DOUMEI-CHKFLG.
                      07  W07-VALUE   PIC X(28).
                    06  W07-DOUMEI-CHKFLG-COUNT   PIC S9(9)   BINARY.
                    06  W07-DOUMEI-CHKFLG-ITEM
                            PIC X(28)   OCCURS  2 TIMES.
                  05  W07-JYUFUKU-CHKKBN-CONBO.
                    06  W07-JYUFUKU-CHKKBN.
                      07  W07-VALUE   PIC X(60).
                    06  W07-JYUFUKU-CHKKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-JYUFUKU-CHKKBN-ITEM
                            PIC X(60)   OCCURS  5 TIMES.
                  05  W07-KOHONLY-CHKFLG-COMBO.
                    06  W07-KOHONLY-CHKFLG.
                      07  W07-VALUE   PIC X(28).
                    06  W07-KOHONLY-CHKFLG-COUNT   PIC S9(9)   BINARY.
                    06  W07-KOHONLY-CHKFLG-ITEM
                            PIC X(28)   OCCURS  2 TIMES.
                  05  W07-PTNUMDEL-FLG-COMBO.
                    06  W07-PTNUMDEL-FLG.
                      07  W07-VALUE   PIC X(28).
                    06  W07-PTNUMDEL-FLG-COUNT   PIC S9(9)   BINARY.
                    06  W07-PTNUMDEL-FLG-ITEM
                            PIC X(28)   OCCURS  2 TIMES.
                  05  W07-HONKZKKBN-COMBO.
                    06  W07-HONKZKKBN.
                      07  W07-VALUE   PIC X(28).
                    06  W07-HONKZKKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-HONKZKKBN-ITEM
                            PIC X(28)   OCCURS  2 TIMES.
                  05  W07-KZKNEN.
                    06  W07-VALUE   PIC X(2).
                  05  W07-HIHKNJAKBN-COMB.
                    06  W07-HIHKNJAKBN.
                      07  W07-VALUE   PIC X(28).
                    06  W07-HIHKNJAKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-HIHKNJAKBN-ITEM
                            PIC X(28)   OCCURS  5 TIMES.
                  05  W07-SETAIKBN-COMB.
                    06  W07-SETAIKBN.
                      07  W07-VALUE   PIC X(28).
                    06  W07-SETAIKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-SETAIKBN-ITEM
                            PIC X(28)   OCCURS  5 TIMES.
                  05  W07-GRPPTLST-COMB.
                    06  W07-GRPPTLST.
                      07  W07-VALUE   PIC X(28).
                    06  W07-GRPPTLST-COUNT   PIC S9(9)   BINARY.
                    06  W07-GRPPTLST-ITEM
                            PIC X(28)   OCCURS  5 TIMES.
                  05  W07-KOHRRK-COMB.
                    06  W07-KOHRRK.
                      07  W07-VALUE   PIC X(28).
                    06  W07-KOHRRK-COUNT   PIC S9(9)   BINARY.
                    06  W07-KOHRRK-ITEM
                            PIC X(28)   OCCURS  5 TIMES.
                  05  W07-POSTKBN-COMB.
                    06  W07-POSTKBN.
                      07  W07-VALUE   PIC X(40).
                    06  W07-POSTKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-POSTKBN-ITEM
                            PIC X(40)   OCCURS  5 TIMES.
                  05  W07-KENMEIKBN-COMB.
                    06  W07-KENMEIKBN.
                      07  W07-VALUE   PIC X(28).
                    06  W07-KENMEIKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-KENMEIKBN-ITEM
                            PIC X(28)   OCCURS  5 TIMES.
                  05  W07-TOWNNAME2KBN-COMB.
                    06  W07-TOWNNAME2KBN.
                      07  W07-VALUE   PIC X(28).
                    06  W07-TOWNNAME2KBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-TOWNNAME2KBN-ITEM
                            PIC X(28)   OCCURS  5 TIMES.
                  05  W07-KOHHBTKBN-COMB.
                    06  W07-KOHHBTKBN.
                      07  W07-VALUE   PIC X(28).
                    06  W07-KOHHBTKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-KOHHBTKBN-ITEM
                            PIC X(28)   OCCURS  5 TIMES.
                  05  W07-HKNINITKBN-COMB.
                    06  W07-HKNINITKBN.
                      07  W07-VALUE   PIC X(32).
                    06  W07-HKNINITKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-HKNINITKBN-ITEM
                            PIC X(32)   OCCURS  5 TIMES.
                04  W07-TAGNAME2.
                  05  W07-TAGNAME2-STATE   PIC S9(9)   BINARY.
                04  W07-TAG3.
                  05  W07-SRYNYKNKNRKBN-COMBO.
                    06  W07-SRYNYKNKNRKBN.
                      07  W07-VALUE   PIC X(40).
                    06  W07-SRYNYKNKNRKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-SRYNYKNKNRKBN-ITEM    PIC X(40)   OCCURS  5.
                  05  W07-HAKHOUFLG-COMB.
                    06  W07-HAKHOUFLG.
                      07  W07-VALUE   PIC X(40).
                    06  W07-HAKHOUFLG-COUNT   PIC S9(9)   BINARY.
                    06  W07-HAKHOUFLG-ITEM
                            PIC X(40)   OCCURS  5 TIMES.
                  05  W07-KAGRPFLG-COMB.
                    06  W07-KAGRPFLG.
                      07  W07-VALUE   PIC X(40).
                    06  W07-KAGRPFLG-COUNT   PIC S9(9)   BINARY.
                    06  W07-KAGRPFLG-ITEM
                            PIC X(40)   OCCURS  2 TIMES.
                  05  W07-DOSRYKBN-FLG-COMBO.
                    06  W07-DOSRYKBN-FLG.
                      07  W07-VALUE   PIC X(40).
                    06  W07-DOSRYKBN-FLG-COUNT   PIC S9(9)   BINARY.
                    06  W07-DOSRYKBN-FLG-ITEM
                            PIC X(40)   OCCURS  20 TIMES.
                  05  W07-WKSRYHYO-FLG-COMBO.
                    06  W07-WKSRYHYO-FLG.
                      07  W07-VALUE   PIC X(40).
                    06  W07-WKSRYHYO-FLG-COUNT   PIC S9(9)   BINARY.
                    06  W07-WKSRYHYO-FLG-ITEM
                            PIC X(40)   OCCURS  5  TIMES.
                  05  W07-FORMAL-FLG-COMB.
                    06  W07-FORMAL-FLG.
                      07  W07-VALUE   PIC X(40).
                    06  W07-FORMAL-FLG-COUNT   PIC S9(9)   BINARY.
                    06  W07-FORMAL-FLG-ITEM
                            PIC X(40)   OCCURS  5 TIMES.
                  05  W07-TEISEIFLG-COMB.
                    06  W07-TEISEIFLG.
                      07  W07-VALUE   PIC X(40).
                    06  W07-TEISEIFLG-COUNT   PIC S9(9)   BINARY.
                    06  W07-TEISEIFLG-ITEM
                            PIC X(40)   OCCURS  3 TIMES.
                  05  W07-CHG310FLG-COMB.
                    06  W07-CHG310FLG.
                      07  W07-VALUE   PIC X(40).
                    06  W07-CHG310FLG-COUNT   PIC S9(9)   BINARY.
                    06  W07-CHG310FLG-ITEM
                            PIC X(40)   OCCURS  2 TIMES.
                  05  W07-TEIKINKBN-COMB.
                    06  W07-TEIKINKBN.
                      07  W07-VALUE   PIC X(40).
                    06  W07-TEIKINKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-TEIKINKBN-ITEM
                            PIC X(40)   OCCURS  2 TIMES.
                  05  W07-YKJCHKKBN-COMB.
                    06  W07-YKJCHKKBN.
                      07  W07-VALUE   PIC X(40).
                    06  W07-YKJCHKKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-YKJCHKKBN-ITEM
                            PIC X(40)   OCCURS  2 TIMES.
                  05  W07-AKUSEIHYO-COMB.
                    06  W07-AKUSEIHYO.
                      07  W07-VALUE   PIC X(40).
                    06  W07-AKUSEIHYO-COUNT   PIC S9(9)   BINARY.
                    06  W07-AKUSEIHYO-ITEM
                            PIC X(40)   OCCURS  2 TIMES.
                  05  W07-CHGSRYSYU-COMB.
                    06  W07-CHGSRYSYU.
                      07  W07-VALUE   PIC X(40).
                    06  W07-CHGSRYSYU-COUNT   PIC S9(9)   BINARY.
                    06  W07-CHGSRYSYU-ITEM
                            PIC X(40)   OCCURS  2 TIMES.
                  05  W07-OKUSURIKBN-COMB.
                    06  W07-OKUSURIKBN.
                      07  W07-VALUE   PIC X(60).
                    06  W07-OKUSURIKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-OKUSURIKBN-ITEM
                            PIC X(60)   OCCURS  5 TIMES.
                  05  W07-RESPRV038-COMB.
                    06  W07-RESPRV038.
                      07  W07-VALUE   PIC X(40).
                    06  W07-RESPRV038-COUNT   PIC S9(9)   BINARY.
                    06  W07-RESPRV038-ITEM
                            PIC X(40)   OCCURS  5 TIMES.
                  05  W07-YKJOKUKBN-COMB.
                    06  W07-YKJOKUKBN.
                      07  W07-VALUE   PIC X(40).
                    06  W07-YKJOKUKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-YKJOKUKBN-ITEM
                            PIC X(40)   OCCURS  5 TIMES.
                  05  W07-HAIKIKBN-COMB.
                    06  W07-HAIKIKBN.
                      07  W07-VALUE   PIC X(40).
                    06  W07-HAIKIKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-HAIKIKBN-ITEM
                            PIC X(40)   OCCURS  5 TIMES.
                  05  W07-NYKHNKKBN-COMB.
                    06  W07-NYKHNKKBN.
                      07  W07-VALUE   PIC X(60).
                    06  W07-NYKHNKKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-NYKHNKKBN-ITEM
                            PIC X(60)   OCCURS  5 TIMES.
                  05  W07-SEI20KSNKBN-COMB.
                    06  W07-SEI20KSNKBN.
                      07  W07-SEI20KSNKBN-VALUE   PIC X(40).
                    06  W07-SEI20KSNKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-SEI20KSNKBN-ITEM
                            PIC X(40)   OCCURS  5 TIMES.
                  05  W07-SIN20KSNKBN-COMB.
                    06  W07-SIN20KSNKBN.
                      07  TEXTDATA    PIC X(40).
                    06  W07-SIN20KSNKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-SIN20KSNKBN-ITEM
                            PIC X(40)   OCCURS  5 TIMES.
                  05  W07-YMDHENKBN-COMB.
                    06  W07-YMDHENKBN.
                      07  W07-VALUE   PIC X(60).
                    06  W07-YMDHENKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-YMDHENKBN-ITEM
                            PIC X(60)   OCCURS  5 TIMES.
                  05  W07-GMNHYOKBN1-COMB.
                    06  W07-GMNHYOKBN1.
                      07  W07-VALUE   PIC X(40).
                    06  W07-GMNHYOKBN1-COUNT   PIC S9(9)   BINARY.
                    06  W07-GMNHYOKBN1-ITEM
                            PIC X(40)   OCCURS  10 TIMES.
                  05  W07-GMNHYOKBN2-COMB.
                    06  W07-GMNHYOKBN2.
                      07  W07-VALUE   PIC X(40).
                    06  W07-GMNHYOKBN2-COUNT   PIC S9(9)   BINARY.
                    06  W07-GMNHYOKBN2-ITEM
                            PIC X(40)   OCCURS  10 TIMES.
                  05  W07-GMNHYOKBN3-COMB.
                    06  W07-GMNHYOKBN3.
                      07  W07-VALUE   PIC X(40).
                    06  W07-GMNHYOKBN3-COUNT   PIC S9(9)   BINARY.
                    06  W07-GMNHYOKBN3-ITEM
                            PIC X(40)   OCCURS  10 TIMES.
                  05  W07-GMNHYOKBN4-COMB.
                    06  W07-GMNHYOKBN4.
                      07  W07-VALUE   PIC X(40).
                    06  W07-GMNHYOKBN4-COUNT   PIC S9(9)   BINARY.
                    06  W07-GMNHYOKBN4-ITEM
                            PIC X(40)   OCCURS  10 TIMES.
                  05  W07-GMNHYOKBN5-COMB.
                    06  W07-GMNHYOKBN5.
                      07  W07-VALUE   PIC X(40).
                    06  W07-GMNHYOKBN5-COUNT   PIC S9(9)   BINARY.
                    06  W07-GMNHYOKBN5-ITEM
                            PIC X(40)   OCCURS  10 TIMES.
                  05  W07-GMNHYOKBN6-COMB.
                    06  W07-GMNHYOKBN6.
                      07  W07-VALUE   PIC X(40).
                    06  W07-GMNHYOKBN6-COUNT   PIC S9(9)   BINARY.
                    06  W07-GMNHYOKBN6-ITEM
                            PIC X(40)   OCCURS  10 TIMES.
                  05  W07-BGCOLORS.
                    06  W07-COLOR   PIC X(16).
                  05  W07-BGLINEKBN-COMB.
                    06  W07-BGLINEKBN.
                      07  W07-VALUE   PIC X(40).
                    06  W07-BGLINEKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-BGLINEKBN-ITEM
                            PIC X(40)   OCCURS  5 TIMES.
                  05  W07-BGCOLORSJ0.
                    06  W07-COLOR   PIC X(16).
                  05  W07-BGCOLORSDAY.
                    06  W07-COLOR   PIC X(16).
                  05  W07-BGCOLORSJ1.
                    06  W07-COLOR   PIC X(16).
                  05  W07-BGCOLORSJ2.
                    06  W07-COLOR   PIC X(16).
                  05  W07-BGCOLORSJ3.
                    06  W07-COLOR   PIC X(16).
                  05  W07-BGCOLORSJ4.
                    06  W07-COLOR   PIC X(16).
                  05  W07-BGCOLORSJ5.
                    06  W07-COLOR   PIC X(16).
                  05  W07-BGCOLORSJ6.
                    06  W07-COLOR   PIC X(16).
                  05  W07-BGCOLORSJ7.
                    06  W07-COLOR   PIC X(16).
                  05  W07-BGCOLORSJ8.
                    06  W07-COLOR   PIC X(16).
                  05  W07-GENERIC-FLG-COMB.
                    06  W07-GENERIC-FLG.
                      07  W07-VALUE   PIC X(40).
                    06  W07-GENERIC-FLG-COUNT   PIC S9(9)   BINARY.
                    06  W07-GENERIC-FLG-ITEM
                            PIC X(40)   OCCURS  5 TIMES.
                  05  W07-RECEKISAI-FLG-COMB.
                    06  W07-RECEKISAI-FLG.
                      07  W07-VALUE   PIC X(40).
                    06  W07-RECEKISAI-FLG-COUNT   PIC S9(9)   BINARY.
                    06  W07-RECEKISAI-FLG-ITEM
                            PIC X(40)   OCCURS  5 TIMES.
                  05  W07-ERRCOLORS.
                    06  W07-COLOR   PIC X(16).
                  05  W07-KEICOLORS.
                    06  W07-COLOR   PIC X(16).
                  05  W07-PRVLAYOUT038-COMB.
                    06  W07-PRVLAYOUT038.
                      07  W07-VALUE   PIC X(60).
                    06  W07-PRVLAYOUT038-COUNT   PIC S9(9)   BINARY.
                    06  W07-PRVLAYOUT038-ITEM
                            PIC X(60)   OCCURS  5 TIMES.
                  05  W07-SETALLKBN-COMB.
                    06  W07-SETALLKBN.
                      07  W07-VALUE   PIC X(40).
                    06  W07-SETALLKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-SETALLKBN-ITEM
                            PIC X(40)   OCCURS  5 TIMES.
                04  W07-TAGNAME3.
                  05  W07-TAGNAME3-STATE   PIC S9(9)   BINARY.
                04  W07-TAG4.
                  05  W07-S02SKYSAI-COMBO.
                    06  W07-S02SKYSAI.
                      07  W07-VALUE   PIC X(32).
                    06  W07-S02SKYSAI-COUNT   PIC S9(9)   BINARY.
                    06  W07-S02SKYSAI-ITEM    PIC X(32)   OCCURS  5.
                  05  W07-S02SKYMISYUPRT-COMBO.
                    06  W07-S02SKYMISYUPRT.
                      07  W07-VALUE   PIC X(32).
                    06  W07-S02SKYMISYUPRT-COUNT   PIC S9(9)   BINARY.
                    06  W07-S02SKYMISYUPRT-ITEM    PIC X(32)   OCCURS 5.
                  05  W07-TUKIGRP-COMBO.
                    06  W07-TUKIGRP.
                      07  W07-VALUE   PIC X(32).
                    06  W07-TUKIGRP-COUNT          PIC S9(9) BINARY.
                    06  W07-TUKIGRP-ITEM           PIC X(32) OCCURS 5.
                  05  W07-TUKISKYMISYUPRT-COMBO.
                    06  W07-TUKISKYMISYUPRT.
                      07  W07-VALUE   PIC X(32).
                    06  W07-TUKISKYMISYUPRT-COUNT   PIC S9(9) BINARY.
                    06  W07-TUKISKYMISYUPRT-ITEM    PIC X(32) OCCURS 5.
                  05  W07-TUKISUM-COMBO.
                    06  W07-TUKISUM.
                      07  W07-VALUE   PIC X(40).
                    06  W07-TUKISUM-COUNT          PIC S9(9) BINARY.
                    06  W07-TUKISUM-ITEM           PIC X(40) OCCURS 5.
                  05  W07-S02MODE-COMBO.
                    06  W07-S02MODE.
                      07  W07-VALUE   PIC X(22).
                    06  W07-S02MODE-COUNT   PIC S9(9)   BINARY.
                    06  W07-S02MODE-ITEM    PIC X(22)   OCCURS  5.
                  05  W07-LBLS02LIMIT       PIC X(30).
                  05  W07-S02LIMIT-VISIBLE  PIC X(1).
                  05  W07-S02LIMIT    PIC S9(3).
                  05  W07-S02SORTKBN-COMBO.
                    06  W07-S02SORTKBN.
                      07  W07-VALUE   PIC X(22).
                    06  W07-S02SORTKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-S02SORTKBN-ITEM    PIC X(22)   OCCURS  5.
                  05  W07-SAIKEISANSKYCHK-COMBO.
                    06  W07-SAIKEISANSKYCHK       PIC X(60).
                    06  W07-SAIKEISANSKYCHK-COUNT PIC S9(9) BINARY.
                    06  W07-SAIKEISANSKYCHK-ITEM  PIC X(60) OCCURS  5.
                  05  W07-IKTSORTKBN-COMBO.
                    06  W07-IKTSORTKBN.
                      07  W07-VALUE   PIC X(22).
                    06  W07-IKTSORTKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-IKTSORTKBN-ITEM    PIC X(22)   OCCURS  5.
                  05  W07-IKTKEISANKBN-COMBO.
                    06  W07-IKTKEISANKBN.
                      07  W07-VALUE   PIC X(52).
                    06  W07-IKTKEISANKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-IKTKEISANKBN-ITEM    PIC X(52)   OCCURS  5.
                  05  W07-SYUNYKNKNRKBN-COMBO.
                    06  W07-SYUNYKNKNRKBN.
                      07  W07-VALUE   PIC X(40).
                    06  W07-SYUNYKNKNRKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-SYUNYKNKNRKBN-ITEM    PIC X(40)   OCCURS  5.
                  05  W07-IKTSRYYMKBN-COMBO.
                    06  W07-IKTSRYYMKBN.
                      07  W07-VALUE   PIC X(32).
                    06  W07-IKTSRYYMKBN-COUNT PIC S9(9)   BINARY.
                    06  W07-IKTSRYYMKBN-ITEM  PIC X(32)   OCCURS  5.
                  05  W07-S04SHIHARAIKBN-COMBO.
                    06  W07-S04SHIHARAIKBN       PIC X(12).
                    06  W07-S04SHIHARAIKBN-COUNT PIC S9(9)   BINARY.
                    06  W07-S04SHIHARAIKBN-ITEM  PIC X(12)   OCCURS  3.
                  05  W07-S04NYUGAIKBN-COMBO.
                    06  W07-S04NYUGAIKBN          PIC X(6).
                    06  W07-S04NYUGAIKBN-COUNT    PIC S9(9)   BINARY.
                    06  W07-S04NYUGAIKBN-ITEM     PIC X(6)    OCCURS  3.
                  05  W07-S04SRYKBN-COMBO.
                    06  W07-S04SRYKBN             PIC X(8).
                    06  W07-S04SRYKBN-COUNT       PIC S9(9)   BINARY.
                    06  W07-S04SRYKBN-ITEM        PIC X(8)    OCCURS  3.
                  05  W07-S04KIKANKBN-COMBO.
                    06  W07-S04KIKANKBN           PIC X(10).
                    06  W07-S04KIKANKBN-COUNT     PIC S9(9)   BINARY.
                    06  W07-S04KIKANKBN-ITEM      PIC X(10)   OCCURS  2.
                  05  W07-S04DSPKBN-COMBO.
                    06  W07-S04DSPKBN             PIC X(20).
                    06  W07-S04DSPKBN-STATE       PIC S9(9)   BINARY.
                    06  W07-S04DSPKBN-COUNT       PIC S9(9)   BINARY.
                    06  W07-S04DSPKBN-ITEM        PIC X(20)   OCCURS  4.
                    06  W07-S04DSPKBN-ITEM-STATE  PIC S9(9)   BINARY.
                  05  W07-S04-SUMKBN-COMBO.
                    06  W07-S04SUMKBN          PIC X(18).
                    06  W07-S04SUMKBN-STATE    PIC S9(9)   BINARY.
                    06  W07-S04SUMKBN-COUNT    PIC S9(9)   BINARY.
                    06  W07-S04SUMKBN-ITEM     PIC X(18)   OCCURS  2.
                    06  W07-S04SUMKBN-ITEM-STATE PIC S9(9)   BINARY.
                  05  W07-S04-SYORIKBN-COMBO.
                    06  W07-S04SYORIKBN           PIC X(18).
                    06  W07-S04SYORIKBN-STATE     PIC S9(9)   BINARY.
                    06  W07-S04SYORIKBN-COUNT     PIC S9(9)   BINARY.
                    06  W07-S04SYORIKBN-ITEM      PIC X(18)   OCCURS  2.
                    06  W07-S04SYORIKBN-ITEM-STATE PIC S9(9)   BINARY.
                  05  W07-S04-JIHIPRTKBN-COMBO.
                    06  W07-S04JIHIPRTKBN           PIC X(24).
                    06  W07-S04JIHIPRTKBN-STATE     PIC S9(9)   BINARY.
                    06  W07-S04JIHIPRTKBN-COUNT     PIC S9(9)   BINARY.
                    06  W07-S04JIHIPRTKBN-ITEM      PIC X(24) OCCURS  5.
                    06  W07-S04JIHIPRTKBN-ITEM-STATE PIC S9(9)   BINARY.
                  05  W07-S06SKYPRT-COMBO.
                    06  W07-S06SKYPRT           PIC X(24).
                    06  W07-S06SKYPRT-COUNT   PIC S9(9)   BINARY.
                    06  W07-S06SKYPRT-ITEM    PIC X(24)   OCCURS  5.
                  05  W07-S06SKYSUM-COMBO.
                    06  W07-S06SKYSUM.
                      07  W07-VALUE   PIC X(42).
                    06  W07-S06SKYSUM-COUNT   PIC S9(9)   BINARY.
                    06  W07-S06SKYSUM-ITEM    PIC X(42)   OCCURS  5.
                  05  W07-SYUSKYPRTG-COMBO.
                    06  W07-SYUSKYPRTG.
                      07  W07-VALUE   PIC X(24).
                    06  W07-SYUSKYPRTG-COUNT      PIC S9(9)   BINARY.
                    06  W07-SYUSKYPRTG-ITEM       PIC X(24)   OCCURS  2.
                  05  W07-SYUSRYPRTG-COMBO.
                    06  W07-SYUSRYPRTG.
                      07  W07-VALUE   PIC X(24).
                    06  W07-SYUSRYPRTG-COUNT      PIC S9(9)   BINARY.
                    06  W07-SYUSRYPRTG-ITEM       PIC X(24)   OCCURS  2.
                  05  W07-SYUSKYPRTN-COMBO.
                    06  W07-SYUSKYPRTN.
                      07  W07-VALUE   PIC X(24).
                    06  W07-SYUSKYPRTN-COUNT      PIC S9(9)   BINARY.
                    06  W07-SYUSKYPRTN-ITEM       PIC X(24)   OCCURS  2.
                  05  W07-S03SKYMISYUPRT-COMBO.
                    06  W07-S03SKYMISYUPRT.
                      07  W07-VALUE   PIC X(32).
                    06  W07-S03SKYMISYUPRT-COUNT   PIC S9(9)   BINARY.
                    06  W07-S03SKYMISYUPRT-ITEM    PIC X(32)   OCCURS 5.
                  05  W07-SYUSRYPRTN-COMBO.
                    06  W07-SYUSRYPRTN.
                      07  W07-VALUE   PIC X(24).
                    06  W07-SYUSRYPRTN-COUNT      PIC S9(9)   BINARY.
                    06  W07-SYUSRYPRTN-ITEM       PIC X(24)   OCCURS  2.
                  05  W07-SYUSTIME-HH             PIC S9(02).
                  05  W07-SYUSTIME-MM             PIC S9(02).
                  05  W07-SYUKARITIME-G.
                    06  W07-SYUKARITIME-OCC1      OCCURS      2.
                      07  W07-SYUKARIDAY          PIC X       OCCURS  7.
                      07  W07-SYUKARITIME-OCC2    OCCURS      2.
                        08  W07-SYUKARISTTIME.
                          09  W07-SYUKARIST-HH      PIC S9(02).
                          09  W07-SYUKARIST-MM      PIC S9(02).
                        08  W07-SYUKARIEDTIME.
                          09  W07-SYUKARIED-HH      PIC S9(02).
                          09  W07-SYUKARIED-MM      PIC S9(02).
                  05  W07-MISYUKIKANKBN-COMBO.
                    06  W07-MISYUKIKANKBN.
                      07  W07-VALUE   PIC X(50).
                    06  W07-MISYUKIKANKBN-COUNT PIC S9(9)   BINARY.
                    06  W07-MISYUKIKANKBN-ITEM  PIC X(50)   OCCURS  5.
                04  W07-TAGNAME4.
                  05  W07-TAGNAME4-STATE   PIC S9(9)   BINARY.
                04  W07-TAG5.
                  05  W07-MSG1-DSPKBN-COMBO.
                    06  W07-MSG1-DSPKBN.
                      07  W07-VALUE   PIC X(12).
                    06  W07-MSG1-DSPKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-MSG1-DSPKBN-ITEM    PIC X(12)   OCCURS  2.
                  05  W07-MSG2-DSPKBN-COMBO.
                    06  W07-MSG2-DSPKBN.
                      07  W07-VALUE   PIC X(12).
                    06  W07-MSG2-DSPKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-MSG2-DSPKBN-ITEM    PIC X(12)   OCCURS  2.
                  05  W07-MSG3-DSPKBN-COMBO.
                    06  W07-MSG3-DSPKBN.
                      07  W07-VALUE   PIC X(12).
                    06  W07-MSG3-DSPKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-MSG3-DSPKBN-ITEM    PIC X(12)   OCCURS  2.
                  05  W07-MSG4-DSPKBN-COMBO.
                    06  W07-MSG4-DSPKBN.
                      07  W07-VALUE   PIC X(12).
                    06  W07-MSG4-DSPKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-MSG4-DSPKBN-ITEM    PIC X(12)   OCCURS  2.
                  05  W07-MSG5-DSPKBN-COMBO.
                    06  W07-MSG5-DSPKBN.
                      07  W07-VALUE   PIC X(12).
                    06  W07-MSG5-DSPKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-MSG5-DSPKBN-ITEM    PIC X(12)   OCCURS  2.
                  05  W07-SRYKA-DSPKBN-COMBO.
                    06  W07-SRYKA-DSPKBN.
                      07  W07-VALUE   PIC X(40).
                    06  W07-SRYKA-DSPKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-SRYKA-DSPKBN-ITEM    PIC X(40)   OCCURS  3.
                  05  W07-SYOKBN-DSPKBN-COMBO.
                    06  W07-SYOKBN-DSPKBN.
                      07  W07-VALUE   PIC X(12).
                    06  W07-SYOKBN-DSPKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-SYOKBN-DSPKBN-ITEM    PIC X(12)   OCCURS  3.
                  05  W07-UTAGAIFLG-KBN-COMBO.
                    06  W07-UTAGAIFLG-KBN.
                      07  W07-VALUE   PIC X(36).
                    06  W07-UTAGAIFLG-KBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-UTAGAIFLG-KBN-ITEM    PIC X(36)   OCCURS  5.
                  05  W07-SJKBN-COMBO.
                    06  W07-SJKBN.
                      07  W07-VALUE   PIC X(53).
                    06  W07-SJKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-SJKBN-ITEM
                            PIC X(53)   OCCURS  20 TIMES.
                04  W07-TAGNAME5.
                  05  W07-TAGNAME5-STATE   PIC S9(9)   BINARY.
                04  W07-TAG6.
                  05  W07-SRYKBN-FLG-COMB.
                    06  W07-SRYKBN-FLG.
                      07  W07-VALUE   PIC X(12).
                    06  W07-SRYKBN-FLG-COUNT   PIC S9(9)   BINARY.
                    06  W07-SRYKBN-FLG-ITEM
                            PIC X(12)   OCCURS  2 TIMES.
                  05  W07-SRYKA-FLG-COMB.
                    06  W07-SRYKA-FLG.
                      07  W07-VALUE   PIC X(20).
                    06  W07-SRYKA-FLG-COUNT   PIC S9(9)   BINARY.
                    06  W07-SRYKA-FLG-ITEM
                            PIC X(20)   OCCURS  5 TIMES.
                  05  W07-RESPRV043-COMB.
                    06  W07-RESPRV043.
                      07  W07-VALUE   PIC X(40).
                    06  W07-RESPRV043-COUNT   PIC S9(9)   BINARY.
                    06  W07-RESPRV043-ITEM
                            PIC X(40)   OCCURS  5 TIMES.
                  05  W07-PRVLAYOUT043-COMB.
                    06  W07-PRVLAYOUT043.
                      07  W07-VALUE   PIC X(60).
                    06  W07-PRVLAYOUT043-COUNT   PIC S9(9)   BINARY.
                    06  W07-PRVLAYOUT043-ITEM
                            PIC X(60)   OCCURS  5 TIMES.
                04  W07-TAGNAME6.
                  05  W07-TAGNAME6-STATE   PIC S9(9)   BINARY.
                04  W07-TAG7.
                  05  W07-YYKINIT-COMB.
                    06  W07-YYKINIT-FLG.
                      07  W07-VALUE   PIC X(20).
                    06  W07-YYKINIT-FLG-COUNT   PIC S9(9)   BINARY.
                    06  W07-YYKINIT-FLG-ITEM
                            PIC X(20)   OCCURS  5 TIMES.
                  05  W07-YYKMAXCNT.
                    06  W07-VALUE   PIC S9(2).
                  05  W07-INIT-DRCD-COMB.
                    06  W07-INIT-DRCD.
                      07  W07-VALUE   PIC X(30).
                    06  W07-INIT-DRCD-COUNT   PIC S9(9)   BINARY.
                    06  W07-INIT-DRCD-ITEM
                            PIC X(30)   OCCURS  99 TIMES.
                  05  W07-YYKHYO-KBN-COMB.
                    06  W07-YYKHYO-KBN.
                      07  W07-VALUE   PIC X(20).
                    06  W07-YYKHYO-KBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-YYKHYO-KBN-ITEM
                            PIC X(20)   OCCURS  5 TIMES.
                  05  W07-YYKPTNUM-KBN-COMB.
                    06  W07-YYKPTNUM-KBN.
                      07  W07-VALUE   PIC X(40).
                    06  W07-YYKPTNUM-KBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-YYKPTNUM-KBN-ITEM
                            PIC X(40)   OCCURS  5 TIMES.
                  05  W07-YYKUKE-KBN-COMB.
                    06  W07-YYKUKE-KBN.
                      07  W07-VALUE   PIC X(40).
                    06  W07-YYKUKE-KBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-YYKUKE-KBN-ITEM
                            PIC X(40)   OCCURS  5 TIMES.
                  05  W07-UKEHKNCHK-COMB.
                    06  W07-UKEHKNCHK.
                      07  W07-VALUE   PIC X(50).
                    06  W07-UKEHKNCHK-COUNT   PIC S9(9)   BINARY.
                    06  W07-UKEHKNCHK-ITEM
                            PIC X(50)   OCCURS  5 TIMES.
                  05  W07-YYK-DRKIKAN.
                    06  W07-TEXTDATA    PIC X(30).
                04  W07-TAGNAME7.
                  05  W07-TAGNAME7-STATE   PIC S9(9)   BINARY.
                04  W07-TAG8.
                  05  W07-DAKETUKBN-COMB.
                    06  W07-DAKETUKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-DAKETUKBN-ITEM
                            PIC X(50)   OCCURS  5 TIMES.
                    06  W07-DAKETUKBN.
                      07  W07-TEXTDATA    PIC X(50).
                  05  W07-TEIRITUKBN-COMB.
                    06  W07-TEIRITUKBN-COUNT   PIC S9(9)   BINARY.
                    06  W07-TEIRITUKBN-ITEM
                            PIC X(50)   OCCURS  5 TIMES.
                    06  W07-TEIRITUKBN.
                      07  W07-TEXTDATA    PIC X(50).
                04  W07-TAGNAME8.
                  05  W07-TAGNAME8-STATE   PIC S9(9)   BINARY.
