      *******************************************************************
      * Project code name "ORCA"
      * ∆¸∞Â…∏Ω‡•Ï•ª•◊•»•Ω•’•»° JMA standard receipt software°À
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
       PROGRAM-ID.                 ORCSTOKNYUIN2.
      *****************************************************************
      *  •∑•π•∆•‡Ãæ        : £œ£“£√£¡
      *  •µ•÷•∑•π•∆•‡Ãæ    : ∆˛±°
      *  •≥•Û•›°º•Õ•Û•»Ãæ  : ∆√ƒÍ∆˛±°Œ¡ªªƒÍ¥¸¥÷ºË∆¿ºË∆¿•µ•÷
      *  ¥…Õ˝º‘            : 
      *  ∫Ó¿Æ∆¸…’    ∫Ó∂»º‘        µ≠Ω“
      *  13/08/06    NACL°›æÆ∆¶¬Ù  ø∑µ¨∫Ó¿Æ
      *****************************************************************
      *  •◊•Ì•∞•È•‡Ω§¿µÕ˙ŒÚ
      * Maj/Min/Rev  Ω§¿µº‘       ∆¸…’      ∆‚Õ∆
      *  05.00.01    NACL-æÆ∆¶¬Ù  17/11/20  (A314)«ß√Œæ…º£Œ≈…¬≈Ô∆˛±°Œ¡§Œ
      *                                     µØªª∆¸ —ππ
      *  05.00.01    NACL-æÆ∆¶¬Ù  18/08/28  ≤˛¿µ§«ªªƒÍ•≥°º•…§¨ —ππ§»§ §√§ø
      *                                     ∆√ƒÍ∆˛±°Œ¡§ŒƒÃªª∆¸øÙºË∆¿ —ππ
      *  05.00.01    NACL-æÆ∆¶¬Ù  18/11/20  ª‹¿ﬂ¥Ω‡ —ππ§«¥Àœ¬•±•¢…¬≈Ô∆˛±°Œ¡
      *                                     §Œ£±§»£≤§¨∫Æ∫ﬂ§π§ÎæÏπÁ§Œ∆√ƒÍ∆˛±°Œ¡
      *                                     §ŒƒÃªª∆¸øÙºË∆¿ —ππ
      *  05.00.01    NACL-æÆ∆¶¬Ù  18/12/06  ª‹¿ﬂ¥Ω‡ —ππ§«√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°Œ¡°¢
      *                                     √œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡§Œ£±§»£≤§¨
      *                                     ∫Æ∫ﬂ§π§ÎæÏπÁ§Œ∆√ƒÍ∆˛±°Œ¡§ŒƒÃªª∆¸øÙ
      *                                     ºË∆¿ —ππ° ∆√ƒÍ√œ∞Ë§œÃ§¬–±˛°À
      *  05.00.01    NACL-æÆ∆¶¬Ù  18/12/06  √œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°Œ¡°¢
      *                                     √œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡§Œ
      *                                     ƒÃªª∆¸øÙºË∆¿ —ππ
      *  05.00.01    NACL-æÆ∆¶¬Ù  19/04/17  √œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°Œ¡°¢
      *                                     √œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡§Œ
      *                                     ≤∏Ó§´§È§Œ∞€∆∞° µﬁ¿≠¡˝∞≠°À¬–±˛
      *****************************************************************
      *
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
      *    •’•È•∞ŒŒ∞Ë
       01  FLG-AREA.
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-HIT                     PIC 9(01).
           03  FLG-PTCONF                  PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
      *  ° £∞°ßº£Œ≈ºº√±∞Ã∞ ≥∞§Œ∆√ƒÍ∆˛±°Œ¡°¢£±°ßº£Œ≈ºº√±∞Ã§Œ∆√ƒÍ∆˛±°Œ¡°À
           03  FLG-ROOM                    PIC 9(01).
           03  FLG-KAIGO                   PIC 9(01).
      *
      *    ≈∫ª˙ŒŒ∞Ë
       01  IDX-AREA.
           03  IDX                         PIC 9(04).
           03  TN-IDX                      PIC 9(04).
      *
      *    ∞Ïª˛ŒŒ∞Ë
       01  WRK-AREA.
      *
           03  WRK-TOKNYUIN2-SRYCD         PIC X(09)  OCCURS 5.
           03  WRK-JGN-NISSU               PIC 9(05)  OCCURS 5.
           03  WRK-SRYCD-COUNT             PIC 9(02).
           03  WRK-TOKUTEI-DAY             PIC 9(03).
           03  WRK-TOKUTEI-DAY1            PIC 9(03).
           03  WRK-TOKUTEI-DAY2            PIC 9(03).
           03  WRK-TOKUTEI-DAY3            PIC 9(03).
           03  WRK-TOKUTEI-DAY4            PIC 9(03).
           03  WRK-TOKUTEI-DAY5            PIC 9(03).
           03  WRK-TOKU-TAISYONISSU        PIC 9(05).
      *    ∆√ƒÍ∆˛±°Œ¡§Œ≤Ò∑◊∫Ó¿Æ∏Â§À∞Ï»Ã§Œ∆˛±°Œ¡§«≤Ò∑◊§Ú∫Ó¿Æ§π§Î§ø§·§Œ
      *    ŒŒ∞Ë
           03  IPN-NYUKAIKEI-SAKUSEI-FLG   PIC X(01).
           03  IPN-SANTEISTYMD             PIC X(08).
           03  WRK-IPN-HANTEI-YMD1.
               05  WRK-IPN-HANTEI-YM1.
                   07  WRK-IPN-HANTEI-Y1   PIC 9(04).
                   07  WRK-IPN-HANTEI-M1   PIC 9(02).
               05  WRK-IPN-HANTEI-D1       PIC 9(02).
           03  WRK-IPN-HANTEI-YMD2         PIC X(08).
      *
           03  WRK-WEIGHT                  PIC 9(04).
           03  WRK-TOKUTEI-ROOM-NISSU      PIC 9(04).
           03  WRK-CHIIKI-HOUKATSU-NISSU   PIC 9(04).
      *
      *****************************************************************
      *    •’•°•§•Î•Ï•§•¢•¶•»
      *****************************************************************
      *
      *    •∑•π•∆•‡¥…Õ˝
           COPY    "CPSK5001.INC".
      *
      *    ¥µº‘∏ƒ Ã¿ﬂƒÍ
       01  PTCONF-REC.
           COPY    "CPPTCONF.INC".
      *
      *    ¥µº‘∆˛±°Õ˙ŒÚ
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    £ƒ£¬∏°∫˜
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    •µ•÷•◊•ÌÕ—°°ŒŒ∞Ë
      *****************************************************************
      *
      *    ∆¸…’ —¥π•µ•÷
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
      *    ƒÃªª∆¸øÙºË∆¿•µ•÷
           COPY "CPORCSTUSAN.INC".
      *
      *****************************************************************
      *    œ¢ÕÌ°°ŒŒ∞Ë
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    ∆√ƒÍ∆˛±°Œ¡ªªƒÍ¥¸¥÷ºË∆¿•—•È•·•ø
           COPY    "CPORCSTOKNYUIN2.INC".
      *
      *    •π•—ŒŒ∞Ë
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSTOKNYUIN2-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    ºÁ°°°°ΩËÕ˝
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      LNK-TOKNYUIN2-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-TOKNYUIN2-RC
      *
           PERFORM    200-EDIT-SEC
           .
      *
       000-PROC-EXT.
           EXIT   PROGRAM
           .
      *
      *****************************************************************
      *    ∆√ƒÍ∆˛±°Œ¡•≥°º•…•ª•√•»
      *****************************************************************
       200-EDIT-SEC                    SECTION.
      *
           DISPLAY  "====  ORCSTOKNYUIN2 SET  ===="
           DISPLAY  "LNK-TOKNYUIN2-TOKUTEI-NYUIN  = " 
                     LNK-TOKNYUIN2-TOKUTEI-NYUIN
           DISPLAY  "LNK-TOKNYUIN2-IDOKBN    = " LNK-TOKNYUIN2-IDOKBN
           DISPLAY  "LNK-TOKNYUIN2-NYUINYMD  = " LNK-TOKNYUIN2-NYUINYMD
           DISPLAY  "LNK-TOKNYUIN2-SANTEIYMD = " LNK-TOKNYUIN2-SANTEIYMD
           DISPLAY  "LNK-TOKNYUIN2-NISSU08   = " LNK-TOKNYUIN2-NISSU08
      *
      *    ∆√ ÃΩËÕ˝
      *    ¿∫ø¿≤ µﬁ¿≠¥¸º£Œ≈…¬≈Ô∆˛±°Œ¡£±§Œµﬁ¿≠¡˝∞≠§À§Ë§Î≈æ≈Ô
           IF   (LNK-TOKNYUIN2-SET-SRYCD   =  "190121110")  AND
                (LNK-TOKNYUIN2-IDOKBN      =  "1")          AND
                (LNK-TOKNYUIN2-TOKUTEI-NYUIN   =   "24")
               MOVE    ZERO            TO  LNK-TOKNYUIN2-NISSU08
           END-IF
      *    ¿∫ø¿≤ µﬁ¿≠¥¸º£Œ≈…¬≈Ô∆˛±°Œ¡£≤§Œµﬁ¿≠¡˝∞≠§À§Ë§Î≈æ≈Ô
           IF   (LNK-TOKNYUIN2-SET-SRYCD   =  "190121210")  AND
                (LNK-TOKNYUIN2-IDOKBN      =  "1")          AND
                (LNK-TOKNYUIN2-TOKUTEI-NYUIN   =   "25")
               MOVE    ZERO            TO  LNK-TOKNYUIN2-NISSU08
           END-IF
      *
      *    Ω–¿∏ª˘¬ŒΩ≈§ŒºË∆¿
           MOVE   SPACE                TO  PTCONF-REC
           INITIALIZE                      PTCONF-REC
           MOVE   LNK-TOKNYUIN2-HOSPNUM
                                       TO  PTCONF-HOSPNUM
           MOVE   LNK-TOKNYUIN2-PTID   TO  PTCONF-PTID
           MOVE   "BIRTHWEIGHT"        TO  PTCONF-CKEY
           PERFORM 900-PTCONF-READ-SEC
           IF     FLG-PTCONF           =   ZERO
               MOVE   PTCONF-CDATA(1:4)  TO  WRK-WEIGHT
           ELSE
               MOVE   9999               TO  WRK-WEIGHT
           END-IF
           DISPLAY  "ORCSTOKNYUIN2  WRK-WEIGHT = " WRK-WEIGHT
      *
      *    ∆√ƒÍ∆˛±°Œ¡•≥°º•…§»ªªƒÍæÂ∏¬∆¸øÙ§Œ•ª•√•»°  ø¿Æ£±£∏«Ø£≥∑Ó£≥£±∆¸∞ ¡∞°À
           IF     LNK-TOKNYUIN2-SANTEIYMD   <=   "20060331"
             EVALUATE    LNK-TOKNYUIN2-SET-SRYCD
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£±
              WHEN      "190024510"
                  MOVE  "190024510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190074510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£±° π‚≈Ÿ°À
              WHEN      "190110210"
                  MOVE  "190110210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190110610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£±° £¡°À
              WHEN      "190110310"
                  MOVE  "190110310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190110710"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£≤
              WHEN      "190024310"
                  MOVE  "190024310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190024410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£≤° π‚≈Ÿ°À
              WHEN      "190110410"
                  MOVE  "190110410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190110810"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£≤° £¡°À
              WHEN      "190110510"
                  MOVE  "190110510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190110910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301)∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡° Ω≈æ…º‘≈˘»ÊŒ®¥Ω‡Ã§À˛°À
              WHEN      "190116310"
                  MOVE  "190116310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190116410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301)∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡
              WHEN      "190024610"
                  MOVE  "190024610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190074610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301-2)•œ•§•±•¢•Ê•À•√•»∆˛±°∞ÂŒ≈¥…Õ˝Œ¡
              WHEN      "190117310"
                  MOVE  "190117310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A302)ø∑¿∏ª˘∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡
      *      (A303)¡ÌπÁº˛ª∫¥¸∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡
      *          ¢®æÂµ≠∆˛±°Œ¡§œΩ–¿∏ª˛¬ŒΩ≈§À§Ë§ÍæÂ∏¬∆¸øÙ§¨ —∆∞§π§Î∞Ÿ°¢
      *            §≥§Œ•µ•÷•◊•Ì•∞•È•‡§«§œ¬–±˛§∑§ §§
      *      (A304)π≠»œ∞œ«ÆΩ˝∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡
              WHEN      "190024810"
                  MOVE  "190024810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A305)∞ÏŒ‡¥∂¿˜æ…¥µº‘∆˛±°∞ÂŒ≈¥…Õ˝Œ¡
              WHEN      "190075510"
                  MOVE  "190075510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190075610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A308)≤Û…¸¥¸•Í•œ•”•Í•∆°º•∑•Á•Û…¬≈Ô∆˛±°Œ¡
              WHEN      "190075910"
                  MOVE  "190075910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  180            TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A308-2)∞°µﬁ¿≠¥¸∆˛±°∞ÂŒ≈¥…Õ˝Œ¡
              WHEN      "190117410"
                  MOVE  "190117410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  90             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A316)ø«Œ≈ΩÍœ∑øÕ∞ÂŒ≈¥…Õ˝Œ¡
              WHEN      "190774110"
                  MOVE  "190774110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  "190774210"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
             END-EVALUATE
           END-IF
      *
      *    ∆√ƒÍ∆˛±°Œ¡•≥°º•…§»ªªƒÍæÂ∏¬∆¸øÙ§Œ•ª•√•»
      *    °  ø¿Æ£±£∏«Ø£¥∑Ó£±∆¸§´§È ø¿Æ£≤£∞«Ø£≥∑Ó£≥£±∆¸°À
           IF    (LNK-TOKNYUIN2-SANTEIYMD   >=   "20060401")  AND
                 (LNK-TOKNYUIN2-SANTEIYMD   <=   "20080331")
             EVALUATE    LNK-TOKNYUIN2-SET-SRYCD
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£±
              WHEN      "190024510"
                  MOVE  "190024510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190074510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£±° π‚≈Ÿ°À
              WHEN      "190110210"
                  MOVE  "190110210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190110610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£±° £¡°À
              WHEN      "190110310"
                  MOVE  "190110310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190110710"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£≤
              WHEN      "190024310"
                  MOVE  "190024310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190024410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£≤° π‚≈Ÿ°À
              WHEN      "190110410"
                  MOVE  "190110410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190110810"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£≤° £¡°À
              WHEN      "190110510"
                  MOVE  "190110510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190110910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301)∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡
              WHEN      "190116310"
                  MOVE  "190116310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190116410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301)∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡° Ω≈æ…º‘≈˘»ÊŒ®¥Ω‡≈¨πÁ°À
              WHEN      "190024610"
                  MOVE  "190024610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190074610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301-2)•œ•§•±•¢•Ê•À•√•»∆˛±°∞ÂŒ≈¥…Õ˝Œ¡
              WHEN      "190117310"
                  MOVE  "190117310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A301-3)«æ¬¥√Ê•±•¢•Ê•À•√•»∆˛±°∞ÂŒ≈¥…Õ˝Œ¡
              WHEN      "190120810"
                  MOVE  "190120810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A302)ø∑¿∏ª˘∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡
      *      (A303)¡ÌπÁº˛ª∫¥¸∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡
      *          ¢®æÂµ≠∆˛±°Œ¡§œΩ–¿∏ª˛¬ŒΩ≈§À§Ë§ÍæÂ∏¬∆¸øÙ§¨ —∆∞§π§Î∞Ÿ°¢
      *            §≥§Œ•µ•÷•◊•Ì•∞•È•‡§«§œ¬–±˛§∑§ §§
      *      (A304)π≠»œ∞œ«ÆΩ˝∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡
              WHEN      "190024810"
                  MOVE  "190024810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A305)∞ÏŒ‡¥∂¿˜æ…¥µº‘∆˛±°∞ÂŒ≈¥…Õ˝Œ¡
              WHEN      "190075510"
                  MOVE  "190075510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190075610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A308-2)∞°µﬁ¿≠¥¸∆˛±°∞ÂŒ≈¥…Õ˝Œ¡
              WHEN      "190117410"
                  MOVE  "190117410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  90             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A311)¿∫ø¿≤ µﬂµﬁ∆˛±°Œ¡
              WHEN      "190121010"
                  MOVE  "190121010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190111410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-2)¿∫ø¿≤ µﬁ¿≠¥¸º£Œ≈…¬≈Ô∆˛±°Œ¡£±
              WHEN      "190121110"
                  MOVE  "190121110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190066910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-2)¿∫ø¿≤ µﬁ¿≠¥¸º£Œ≈…¬≈Ô∆˛±°Œ¡£≤
              WHEN      "190121210"
                  MOVE  "190121210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190067010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A314)œ∑øÕ¿≠«ß√Œæ…º¿¥µº£Œ≈…¬≈Ô∆˛±°Œ¡£±
              WHEN      "190739910"
                  MOVE  "190739910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  90             TO  WRK-JGN-NISSU(1)
                  MOVE  "190740010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A314)œ∑øÕ¿≠«ß√Œæ…º¿¥µº£Œ≈…¬≈Ô∆˛±°Œ¡£≤
              WHEN      "190813410"
                  MOVE  "190813410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  90             TO  WRK-JGN-NISSU(1)
                  MOVE  "190813510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A316)ø«Œ≈ΩÍœ∑øÕ∞ÂŒ≈¥…Õ˝Œ¡
              WHEN      "190774110"
                  MOVE  "190774110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  "190774210"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A316)ø«Œ≈ΩÍœ∑øÕ∞ÂŒ≈¥…Õ˝Œ¡° ¿∏≥ËŒ≈Õ‹°À
              WHEN      "190125010"
                  MOVE  "190125010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  "190125110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
             END-EVALUATE
           END-IF
      *
      *    ∆√ƒÍ∆˛±°Œ¡•≥°º•…§»ªªƒÍæÂ∏¬∆¸øÙ§Œ•ª•√•»
      *    °  ø¿Æ£≤£∞«Ø£¥∑Ó£±∆¸§´§È ø¿Æ£≤£≤«Ø£≥∑Ó£≥£±∆¸°À
           IF    (LNK-TOKNYUIN2-SANTEIYMD   >=   "20080401")  AND
                 (LNK-TOKNYUIN2-SANTEIYMD   <=   "20100331")
             EVALUATE    LNK-TOKNYUIN2-SET-SRYCD
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£±
              WHEN      "190024510"
                  MOVE  "190024510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190128610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190074510"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£≤
              WHEN      "190024310"
                  MOVE  "190024310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190128710"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190024410"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A301)∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡
              WHEN      "190116310"
                  MOVE  "190116310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190116410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301)∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡° Ω≈æ…º‘≈˘»ÊŒ®¥Ω‡≈¨πÁ°À
              WHEN      "190024610"
                  MOVE  "190024610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190074610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301-2)•œ•§•±•¢•Ê•À•√•»∆˛±°∞ÂŒ≈¥…Õ˝Œ¡
              WHEN      "190117310"
                  MOVE  "190117310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A301-3)«æ¬¥√Ê•±•¢•Ê•À•√•»∆˛±°∞ÂŒ≈¥…Õ˝Œ¡
              WHEN      "190120810"
                  MOVE  "190120810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A302)ø∑¿∏ª˘∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡
      *      (A303)¡ÌπÁº˛ª∫¥¸∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡
      *          ¢®æÂµ≠∆˛±°Œ¡§œΩ–¿∏ª˛¬ŒΩ≈§À§Ë§ÍæÂ∏¬∆¸øÙ§¨ —∆∞§π§Î∞Ÿ°¢
      *            §≥§Œ•µ•÷•◊•Ì•∞•È•‡§«§œ¬–±˛§∑§ §§
      *      (A304)π≠»œ∞œ«ÆΩ˝∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡
              WHEN      "190024810"
                  MOVE  "190024810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A305)∞ÏŒ‡¥∂¿˜æ…¥µº‘∆˛±°∞ÂŒ≈¥…Õ˝Œ¡
              WHEN      "190075510"
                  MOVE  "190075510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190075610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A308-2)∞°µﬁ¿≠¥¸∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£±
              WHEN      "190117410"
                  MOVE  "190117410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  90             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A308-2)∞°µﬁ¿≠¥¸∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≤
              WHEN      "190129910"
                  MOVE  "190129910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A311)¿∫ø¿≤ µﬂµﬁ∆˛±°Œ¡£±
              WHEN      "190121010"
                  MOVE  "190121010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190111410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311)¿∫ø¿≤ µﬂµﬁ∆˛±°Œ¡£≤
              WHEN      "190130010"
                  MOVE  "190130010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-2)¿∫ø¿≤ µﬁ¿≠¥¸º£Œ≈…¬≈Ô∆˛±°Œ¡£±
              WHEN      "190121110"
                  MOVE  "190121110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190066910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-2)¿∫ø¿≤ µﬁ¿≠¥¸º£Œ≈…¬≈Ô∆˛±°Œ¡£≤
              WHEN      "190121210"
                  MOVE  "190121210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190067010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-3)¿∫ø¿≤ µﬂµﬁ°¶πÁ ªæ…∆˛±°Œ¡
              WHEN      "190130210"
                  MOVE  "190130210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130310"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A314)«ß√Œæ……¬≈Ô∆˛±°Œ¡£±
              WHEN      "190739910"
                  MOVE  "190739910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  90             TO  WRK-JGN-NISSU(1)
                  MOVE  "190740010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A314)«ß√Œæ……¬≈Ô∆˛±°Œ¡£≤
              WHEN      "190813410"
                  MOVE  "190813410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  90             TO  WRK-JGN-NISSU(1)
                  MOVE  "190813510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A316)ø«Œ≈ΩÍ∏Â¥¸π‚Œº‘∞ÂŒ≈¥…Õ˝Œ¡
              WHEN      "190774110"
                  MOVE  "190774110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  "190774210"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A316)ø«Œ≈ΩÍ∏Â¥¸π‚Œº‘∞ÂŒ≈¥…Õ˝Œ¡° ¿∏≥ËŒ≈Õ‹°À
              WHEN      "190125010"
                  MOVE  "190125010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  "190125110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
             END-EVALUATE
           END-IF
      *
      *    ∆√ƒÍ∆˛±°Œ¡•≥°º•…§»ªªƒÍæÂ∏¬∆¸øÙ§Œ•ª•√•»
      *    °  ø¿Æ£≤£≤«Ø£¥∑Ó£±∆¸§´§È ø¿Æ£≤£¥«Ø£≥∑Ó£≥£±∆¸°À
           IF    (LNK-TOKNYUIN2-SANTEIYMD   >=   "20100401")  AND
                 (LNK-TOKNYUIN2-SANTEIYMD   <=   "20120331")
             EVALUATE    LNK-TOKNYUIN2-SET-SRYCD
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£±
              WHEN      "190024510"
                  MOVE  "190024510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190128610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190074510"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£≤
              WHEN      "190024310"
                  MOVE  "190024310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190128710"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190024410"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£≥° µﬂÃøµﬂµﬁ∆˛±°Œ¡°À
              WHEN      "190138110"
                  MOVE  "190138110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138210"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138310"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£≥° π≠»œ∞œ«ÆΩ˝∆√ƒÍΩ∏√Êº£Œ≈°À
              WHEN      "190138410"
                  MOVE  "190138410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138610"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  60             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£¥° µﬂÃøµﬂµﬁ∆˛±°Œ¡°À
              WHEN      "190138710"
                  MOVE  "190138710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138810"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138910"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£¥° π≠»œ∞œ«ÆΩ˝∆√ƒÍΩ∏√Êº£Œ≈°À
              WHEN      "190139010"
                  MOVE  "190139010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190139110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190139210"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  "190139310"    TO  WRK-TOKNYUIN2-SRYCD(4)
                  MOVE  60             TO  WRK-JGN-NISSU(4)
                  MOVE  4              TO  WRK-SRYCD-COUNT
      *      (A301)∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£±
              WHEN      "190116310"
                  MOVE  "190116310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190116410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301)∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£≤° ∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡°À
              WHEN      "190139810"
                  MOVE  "190139810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190139910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301)∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£≤° π≠»œ∞œ«ÆΩ˝∆√ƒÍΩ∏√Êº£Œ≈°À
              WHEN      "190140010"
                  MOVE  "190140010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190140110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301-2)•œ•§•±•¢•Ê•À•√•»∆˛±°∞ÂŒ≈¥…Õ˝Œ¡
              WHEN      "190117310"
                  MOVE  "190117310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A301-3)«æ¬¥√Ê•±•¢•Ê•À•√•»∆˛±°∞ÂŒ≈¥…Õ˝Œ¡
              WHEN      "190120810"
                  MOVE  "190120810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A302)ø∑¿∏ª˘∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£±
              WHEN      "190024710"
                  MOVE  "190024710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A302)ø∑¿∏ª˘∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£≤
              WHEN      "190140410"
                  MOVE  "190140410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A303)¡ÌπÁº˛ª∫¥¸∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡°  Ï¬Œ°¶¬€ª˘°À
              WHEN      "190066710"
                  MOVE  "190066710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A303)¡ÌπÁº˛ª∫¥¸∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡° ø∑¿∏ª˘°À
              WHEN      "190066810"
                  MOVE  "190066810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A303-2)ø∑¿∏ª˘º£Œ≈≤Û…¸ºº∆˛±°∞ÂŒ≈¥…Õ˝Œ¡
              WHEN      "190140510"
                  MOVE  "190140510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  120        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A305)∞ÏŒ‡¥∂¿˜æ…¥µº‘∆˛±°∞ÂŒ≈¥…Õ˝Œ¡
              WHEN      "190075510"
                  MOVE  "190075510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190075610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A308-2)∞°µﬁ¿≠¥¸∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£±
              WHEN      "190117410"
                  MOVE  "190117410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  90             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A308-2)∞°µﬁ¿≠¥¸∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≤
              WHEN      "190129910"
                  MOVE  "190129910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A311)¿∫ø¿≤ µﬂµﬁ∆˛±°Œ¡£±
              WHEN      "190121010"
                  MOVE  "190121010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190111410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311)¿∫ø¿≤ µﬂµﬁ∆˛±°Œ¡£≤
              WHEN      "190130010"
                  MOVE  "190130010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-2)¿∫ø¿≤ µﬁ¿≠¥¸º£Œ≈…¬≈Ô∆˛±°Œ¡£±
              WHEN      "190121110"
                  MOVE  "190121110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190066910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-2)¿∫ø¿≤ µﬁ¿≠¥¸º£Œ≈…¬≈Ô∆˛±°Œ¡£≤
              WHEN      "190121210"
                  MOVE  "190121210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190067010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-3)¿∫ø¿≤ µﬂµﬁ°¶πÁ ªæ…∆˛±°Œ¡
              WHEN      "190130210"
                  MOVE  "190130210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130310"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A314)«ß√Œæ…º£Œ≈…¬≈Ô∆˛±°Œ¡£±
              WHEN      "190739910"
                  MOVE  "190739910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  "190740010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A314)«ß√Œæ…º£Œ≈…¬≈Ô∆˛±°Œ¡£≤
              WHEN      "190813410"
                  MOVE  "190813410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  "190813510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
             END-EVALUATE
           END-IF
      *
      *    ∆√ƒÍ∆˛±°Œ¡•≥°º•…§»ªªƒÍæÂ∏¬∆¸øÙ§Œ•ª•√•»
      *    °  ø¿Æ£≤£¥«Ø£¥∑Ó£±∆¸§´§È ø¿Æ£≤£∂«Ø£≥∑Ó£≥£±∆¸°À
           IF    (LNK-TOKNYUIN2-SANTEIYMD   >=   "20120401")  AND
                 (LNK-TOKNYUIN2-SANTEIYMD   <=   "20140331")
             EVALUATE    LNK-TOKNYUIN2-SET-SRYCD
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£±
              WHEN      "190024510"
                  MOVE  "190024510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190128610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190074510"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£≤
              WHEN      "190024310"
                  MOVE  "190024310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190128710"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190024410"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£≥° µﬂÃøµﬂµﬁ∆˛±°Œ¡°À
              WHEN      "190138110"
                  MOVE  "190138110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138210"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138310"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£≥° π≠»œ∞œ«ÆΩ˝∆√ƒÍΩ∏√Êº£Œ≈°À
              WHEN      "190138410"
                  MOVE  "190138410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138610"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  60             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£¥° µﬂÃøµﬂµﬁ∆˛±°Œ¡°À
              WHEN      "190138710"
                  MOVE  "190138710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138810"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138910"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£¥° π≠»œ∞œ«ÆΩ˝∆√ƒÍΩ∏√Êº£Œ≈°À
              WHEN      "190139010"
                  MOVE  "190139010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190139110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190139210"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  "190139310"    TO  WRK-TOKNYUIN2-SRYCD(4)
                  MOVE  60             TO  WRK-JGN-NISSU(4)
                  MOVE  4              TO  WRK-SRYCD-COUNT
      *      (A301)∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£±
              WHEN      "190116310"
                  MOVE  "190116310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190116410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301)∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£≤° ∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡°À
              WHEN      "190139810"
                  MOVE  "190139810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190139910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301)∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£≤° π≠»œ∞œ«ÆΩ˝∆√ƒÍΩ∏√Êº£Œ≈°À
              WHEN      "190140010"
                  MOVE  "190140010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190140110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301-2)•œ•§•±•¢•Ê•À•√•»∆˛±°∞ÂŒ≈¥…Õ˝Œ¡
              WHEN      "190117310"
                  MOVE  "190117310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A301-3)«æ¬¥√Ê•±•¢•Ê•À•√•»∆˛±°∞ÂŒ≈¥…Õ˝Œ¡
              WHEN      "190120810"
                  MOVE  "190120810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A301-4)æÆª˘∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡
              WHEN      "190149910"
                  MOVE  "190149910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190150010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A302)ø∑¿∏ª˘∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£±
              WHEN      "190024710"
                  MOVE  "190024710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A302)ø∑¿∏ª˘∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£≤
              WHEN      "190140410"
                  MOVE  "190140410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A303)¡ÌπÁº˛ª∫¥¸∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡°  Ï¬Œ°¶¬€ª˘°À
              WHEN      "190066710"
                  MOVE  "190066710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A303)¡ÌπÁº˛ª∫¥¸∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡° ø∑¿∏ª˘°À
              WHEN      "190066810"
                  MOVE  "190066810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A303-2)ø∑¿∏ª˘º£Œ≈≤Û…¸ºº∆˛±°∞ÂŒ≈¥…Õ˝Œ¡
              WHEN      "190140510"
                  MOVE  "190140510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  120        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A305)∞ÏŒ‡¥∂¿˜æ…¥µº‘∆˛±°∞ÂŒ≈¥…Õ˝Œ¡
              WHEN      "190075510"
                  MOVE  "190075510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190075610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A308-2)∞°µﬁ¿≠¥¸∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£±
              WHEN      "190117410"
                  MOVE  "190117410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  IF    LNK-TOKNYUIN2-TOKUBETSU-FLG(1)  =  "1"
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  ELSE
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A308-2)∞°µﬁ¿≠¥¸∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£±° ªÿƒÍ√œ∞Ë°À
              WHEN      "190150310"
                  MOVE  "190150310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A308-2)∞°µﬁ¿≠¥¸∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≤
              WHEN      "190129910"
                  MOVE  "190129910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  IF    LNK-TOKNYUIN2-TOKUBETSU-FLG(1)  =  "1"
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  ELSE
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A308-2)∞°µﬁ¿≠¥¸∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≤° ªÿƒÍ√œ∞Ë°À
              WHEN      "190150410"
                  MOVE  "190150410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A310)¥Àœ¬•±•¢…¬≈Ô∆˛±°Œ¡
              WHEN      "190028910"
                  MOVE  "190028910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190150510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190150610"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A311)¿∫ø¿≤ µﬂµﬁ∆˛±°Œ¡£±
              WHEN      "190121010"
                  MOVE  "190121010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190111410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311)¿∫ø¿≤ µﬂµﬁ∆˛±°Œ¡£≤
              WHEN      "190130010"
                  MOVE  "190130010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-2)¿∫ø¿≤ µﬁ¿≠¥¸º£Œ≈…¬≈Ô∆˛±°Œ¡£±
              WHEN      "190121110"
                  MOVE  "190121110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190066910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-2)¿∫ø¿≤ µﬁ¿≠¥¸º£Œ≈…¬≈Ô∆˛±°Œ¡£≤
              WHEN      "190121210"
                  MOVE  "190121210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190067010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-3)¿∫ø¿≤ µﬂµﬁ°¶πÁ ªæ…∆˛±°Œ¡
              WHEN      "190130210"
                  MOVE  "190130210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130310"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A314)«ß√Œæ…º£Œ≈…¬≈Ô∆˛±°Œ¡£±
              WHEN      "190151610"
                  MOVE  "190151610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190739910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190740010"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A314)«ß√Œæ…º£Œ≈…¬≈Ô∆˛±°Œ¡£≤
              WHEN      "190151710"
                  MOVE  "190151710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190813410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190813510"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A317)∆√ƒÍ∞Ï»Ã…¬≈Ô∆˛±°Œ¡° ∞°µﬁ¿≠¥¸∆˛±°∞ÂŒ≈¥…Õ˝°À
              WHEN      "190152610"
                  MOVE  "190152610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A317)∆√ƒÍ∞Ï»Ã…¬≈Ô∆˛±°Œ¡° ∞°µﬁ¿≠¥¸∆˛±°∞ÂŒ≈¥…Õ˝°¶•Í•œ•”•ÍªªƒÍ¥µº‘°À
              WHEN      "190152710"
                  MOVE  "190152710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
             END-EVALUATE
           END-IF
      *
      *    ∆√ƒÍ∆˛±°Œ¡•≥°º•…§»ªªƒÍæÂ∏¬∆¸øÙ§Œ•ª•√•»
      *    °  ø¿Æ£≤£∂«Ø£¥∑Ó£±∆¸§´§È ø¿Æ£≤£∏«Ø£≥∑Ó£≥£±∆¸°À
           IF    (LNK-TOKNYUIN2-SANTEIYMD   >=   "20140401")  AND
                 (LNK-TOKNYUIN2-SANTEIYMD   <=   "20160331")
             EVALUATE    LNK-TOKNYUIN2-SET-SRYCD
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£±
              WHEN      "190024510"
                  MOVE  "190024510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190128610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190074510"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£≤
              WHEN      "190024310"
                  MOVE  "190024310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190128710"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190024410"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£≥° µﬂÃøµﬂµﬁ∆˛±°Œ¡°À
              WHEN      "190138110"
                  MOVE  "190138110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138210"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138310"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£≥° π≠»œ∞œ«ÆΩ˝∆√ƒÍΩ∏√Êº£Œ≈°À
              WHEN      "190138410"
                  MOVE  "190138410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138610"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  60             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£¥° µﬂÃøµﬂµﬁ∆˛±°Œ¡°À
              WHEN      "190138710"
                  MOVE  "190138710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138810"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138910"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£¥° π≠»œ∞œ«ÆΩ˝∆√ƒÍΩ∏√Êº£Œ≈°À
              WHEN      "190139010"
                  MOVE  "190139010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190139110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190139210"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  "190139310"    TO  WRK-TOKNYUIN2-SRYCD(4)
                  MOVE  60             TO  WRK-JGN-NISSU(4)
                  MOVE  4              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£±
              WHEN      "190174410"
                  MOVE  "190174410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190174510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£≤° ∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡°À
              WHEN      "190174610"
                  MOVE  "190174610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190174710"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£≤° π≠»œ∞œ«ÆΩ˝∆√ƒÍΩ∏√Êº£Œ≈°À
              WHEN      "190174810"
                  MOVE  "190174810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190174910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£≥
              WHEN      "190116310"
                  MOVE  "190116310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190116410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£¥° ∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡°À
              WHEN      "190139810"
                  MOVE  "190139810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190139910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£¥° π≠»œ∞œ«ÆΩ˝∆√ƒÍΩ∏√Êº£Œ≈°À
              WHEN      "190140010"
                  MOVE  "190140010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190140110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-2)•œ•§•±•¢•Ê•À•√•»∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£±
              WHEN      "190175010"
                  MOVE  "190175010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-2)•œ•§•±•¢•Ê•À•√•»∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≤
              WHEN      "190175110"
                  MOVE  "190175110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-2)•œ•§•±•¢•Ê•À•√•»∆˛±°∞ÂŒ≈¥…Õ˝Œ¡
              WHEN      "190117310"
                  MOVE  "190117310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-3)«æ¬¥√Ê•±•¢•Ê•À•√•»∆˛±°∞ÂŒ≈¥…Õ˝Œ¡
              WHEN      "190120810"
                  MOVE  "190120810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-4)æÆª˘∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡
              WHEN      "190149910"
                  MOVE  "190149910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190150010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A302)ø∑¿∏ª˘∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£±
      **          ¬ŒΩ≈£±°§£µ£∞£∞•∞•È•‡∞ æÂ§«°§∏¸¿∏œ´∆Ø¬Áø√§¨ƒÍ§·§Îº¿¥µ
      **          §ÚºÁ…¬§»§π§Î¥µº‘§œ£≥£µ∆¸∏¬≈Ÿ§¿§¨°¢∏Ωæı§«§œ»ΩƒÍ§«§≠§ §§
      **          ∞Ÿ°¢Ã§¬–±˛
              WHEN      "190024710"
                  MOVE  "190024710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A302)ø∑¿∏ª˘∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£≤
      **          ¬ŒΩ≈£±°§£µ£∞£∞•∞•È•‡∞ æÂ§«°§∏¸¿∏œ´∆Ø¬Áø√§¨ƒÍ§·§Îº¿¥µ
      **          §ÚºÁ…¬§»§π§Î¥µº‘§œ£≥£µ∆¸∏¬≈Ÿ§¿§¨°¢∏Ωæı§«§œ»ΩƒÍ§«§≠§ §§
      **          ∞Ÿ°¢Ã§¬–±˛
              WHEN      "190140410"
                  MOVE  "190140410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303)¡ÌπÁº˛ª∫¥¸∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡°  Ï¬Œ°¶¬€ª˘°À
              WHEN      "190066710"
                  MOVE  "190066710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303)¡ÌπÁº˛ª∫¥¸∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡° ø∑¿∏ª˘°À
      **          ¬ŒΩ≈£±°§£µ£∞£∞•∞•È•‡∞ æÂ§«°§∏¸¿∏œ´∆Ø¬Áø√§¨ƒÍ§·§Îº¿¥µ
      **          §ÚºÁ…¬§»§π§Î¥µº‘§œ£≥£µ∆¸∏¬≈Ÿ§¿§¨°¢∏Ωæı§«§œ»ΩƒÍ§«§≠§ §§
      **          ∞Ÿ°¢Ã§¬–±˛
              WHEN      "190066810"
                  MOVE  "190066810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303-2)ø∑¿∏ª˘º£Œ≈≤Û…¸ºº∆˛±°∞ÂŒ≈¥…Õ˝Œ¡
      **          ¬ŒΩ≈£±°§£µ£∞£∞•∞•È•‡∞ æÂ§«°§∏¸¿∏œ´∆Ø¬Áø√§¨ƒÍ§·§Îº¿¥µ
      **          §ÚºÁ…¬§»§π§Î¥µº‘§œ£µ£∞∆¸∏¬≈Ÿ§¿§¨°¢∏Ωæı§«§œ»ΩƒÍ§«§≠§ §§
      **          ∞Ÿ°¢Ã§¬–±˛
              WHEN      "190140510"
                  MOVE  "190140510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  120        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A305)∞ÏŒ‡¥∂¿˜æ…¥µº‘∆˛±°∞ÂŒ≈¥…Õ˝Œ¡
              WHEN      "190075510"
                  MOVE  "190075510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190075610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A308-2)∞°µﬁ¿≠¥¸∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£±
              WHEN      "190117410"
                  MOVE  "190117410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  IF    LNK-TOKNYUIN2-TOKUBETSU-FLG(1)  =  "1"
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  ELSE
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-2)∞°µﬁ¿≠¥¸∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£±° ªÿƒÍ√œ∞Ë°À
              WHEN      "190150310"
                  MOVE  "190150310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-2)∞°µﬁ¿≠¥¸∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≤
              WHEN      "190129910"
                  MOVE  "190129910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  IF    LNK-TOKNYUIN2-TOKUBETSU-FLG(1)  =  "1"
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  ELSE
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-2)∞°µﬁ¿≠¥¸∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≤° ªÿƒÍ√œ∞Ë°À
              WHEN      "190150410"
                  MOVE  "190150410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£±
              WHEN      "190175310"
                  MOVE  "190175310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£±° ¿∏≥ËŒ≈Õ‹°À
              WHEN      "190175410"
                  MOVE  "190175410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£±
              WHEN      "190175510"
                  MOVE  "190175510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£±° ¿∏≥ËŒ≈Õ‹°À
              WHEN      "190175610"
                  MOVE  "190175610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£≤
              WHEN      "190175710"
                  MOVE  "190175710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£≤° ¿∏≥ËŒ≈Õ‹°À
              WHEN      "190175810"
                  MOVE  "190175810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≤
              WHEN      "190175910"
                  MOVE  "190175910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≤° ¿∏≥ËŒ≈Õ‹°À
              WHEN      "190176010"
                  MOVE  "190176010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£±° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190176110"
                  MOVE  "190176110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£±° ¿∏≥ËŒ≈Õ‹°À° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190176210"
                  MOVE  "190176210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£±° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190176310"
                  MOVE  "190176310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£±° ¿∏≥ËŒ≈Õ‹°À° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190176410"
                  MOVE  "190176410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£≤° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190176510"
                  MOVE  "190176510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£≤° ¿∏≥ËŒ≈Õ‹°À° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190176610"
                  MOVE  "190176610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≤° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190176710"
                  MOVE  "190176710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≤° ¿∏≥ËŒ≈Õ‹°À° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190176810"
                  MOVE  "190176810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A310)¥Àœ¬•±•¢…¬≈Ô∆˛±°Œ¡
              WHEN      "190028910"
                  MOVE  "190028910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190150510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190150610"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311)¿∫ø¿≤ µﬂµﬁ∆˛±°Œ¡£±
              WHEN      "190121010"
                  MOVE  "190121010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190111410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311)¿∫ø¿≤ µﬂµﬁ∆˛±°Œ¡£≤
              WHEN      "190130010"
                  MOVE  "190130010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311-2)¿∫ø¿≤ µﬁ¿≠¥¸º£Œ≈…¬≈Ô∆˛±°Œ¡£±
              WHEN      "190121110"
                  MOVE  "190121110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190066910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311-2)¿∫ø¿≤ µﬁ¿≠¥¸º£Œ≈…¬≈Ô∆˛±°Œ¡£≤
              WHEN      "190121210"
                  MOVE  "190121210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190067010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311-3)¿∫ø¿≤ µﬂµﬁ°¶πÁ ªæ…∆˛±°Œ¡
              WHEN      "190130210"
                  MOVE  "190130210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130310"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A314)«ß√Œæ…º£Œ≈…¬≈Ô∆˛±°Œ¡£±
              WHEN      "190151610"
                  MOVE  "190151610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190739910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190740010"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A314)«ß√Œæ…º£Œ≈…¬≈Ô∆˛±°Œ¡£≤
              WHEN      "190151710"
                  MOVE  "190151710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190813410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190813510"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A317)∆√ƒÍ∞Ï»Ã…¬≈Ô∆˛±°Œ¡° ∞°µﬁ¿≠¥¸∆˛±°∞ÂŒ≈¥…Õ˝°À
              WHEN      "190152610"
                  MOVE  "190152610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A317)∆√ƒÍ∞Ï»Ã…¬≈Ô∆˛±°Œ¡° ∞°µﬁ¿≠¥¸∆˛±°∞ÂŒ≈¥…Õ˝°¶•Í•œ•”•ÍªªƒÍ¥µº‘°À
              WHEN      "190152710"
                  MOVE  "190152710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A317)∆√ƒÍ∞Ï»Ã…¬≈Ô∆˛±°Œ¡° √œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝£±°À
              WHEN      "190182310"
                  MOVE  "190182310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A317)∆√ƒÍ∞Ï»Ã…¬≈Ô∆˛±°Œ¡° √œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝£≤°À
              WHEN      "190182410"
                  MOVE  "190182410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
             END-EVALUATE
           END-IF
      *
      *    ∆√ƒÍ∆˛±°Œ¡•≥°º•…§»ªªƒÍæÂ∏¬∆¸øÙ§Œ•ª•√•»
      *    °  ø¿Æ£≤£∏«Ø£¥∑Ó£±∆¸§´§È ø¿Æ£≥£∞«Ø£≥∑Ó£≥£±∆¸°À
           IF    (LNK-TOKNYUIN2-SANTEIYMD   >=   "20160401")  AND
                 (LNK-TOKNYUIN2-SANTEIYMD   <=   "20180331")
             EVALUATE    LNK-TOKNYUIN2-SET-SRYCD
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£±
              WHEN      "190024510"
                  MOVE  "190024510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190128610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190074510"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£≤
              WHEN      "190024310"
                  MOVE  "190024310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190128710"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190024410"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£≥° µﬂÃøµﬂµﬁ∆˛±°Œ¡°À
              WHEN      "190138110"
                  MOVE  "190138110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138210"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138310"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£≥° π≠»œ∞œ«ÆΩ˝∆√ƒÍΩ∏√Êº£Œ≈°À
              WHEN      "190138410"
                  MOVE  "190138410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138610"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  60             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£¥° µﬂÃøµﬂµﬁ∆˛±°Œ¡°À
              WHEN      "190138710"
                  MOVE  "190138710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138810"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138910"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£¥° π≠»œ∞œ«ÆΩ˝∆√ƒÍΩ∏√Êº£Œ≈°À
              WHEN      "190139010"
                  MOVE  "190139010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190139110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190139210"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  "190139310"    TO  WRK-TOKNYUIN2-SRYCD(4)
                  MOVE  60             TO  WRK-JGN-NISSU(4)
                  MOVE  4              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£±
              WHEN      "190174410"
                  MOVE  "190174410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190174510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£≤° ∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡°À
              WHEN      "190174610"
                  MOVE  "190174610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190174710"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£≤° π≠»œ∞œ«ÆΩ˝∆√ƒÍΩ∏√Êº£Œ≈°À
              WHEN      "190174810"
                  MOVE  "190174810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190174910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£≥
              WHEN      "190116310"
                  MOVE  "190116310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190116410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£¥° ∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡°À
              WHEN      "190139810"
                  MOVE  "190139810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190139910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£¥° π≠»œ∞œ«ÆΩ˝∆√ƒÍΩ∏√Êº£Œ≈°À
              WHEN      "190140010"
                  MOVE  "190140010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190140110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-2)•œ•§•±•¢•Ê•À•√•»∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£±
              WHEN      "190175010"
                  MOVE  "190175010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-2)•œ•§•±•¢•Ê•À•√•»∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≤
              WHEN      "190175110"
                  MOVE  "190175110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-2)•œ•§•±•¢•Ê•À•√•»∆˛±°∞ÂŒ≈¥…Õ˝Œ¡
              WHEN      "190117310"
                  MOVE  "190117310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-3)«æ¬¥√Ê•±•¢•Ê•À•√•»∆˛±°∞ÂŒ≈¥…Õ˝Œ¡
              WHEN      "190120810"
                  MOVE  "190120810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-4)æÆª˘∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡
              WHEN      "190149910"
                  MOVE  "190149910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190150010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A302)ø∑¿∏ª˘∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£±
      **          ¬ŒΩ≈£±°§£µ£∞£∞•∞•È•‡∞ æÂ§«°§∏¸¿∏œ´∆Ø¬Áø√§¨ƒÍ§·§Îº¿¥µ
      **          §ÚºÁ…¬§»§π§Î¥µº‘§œ£≥£µ∆¸∏¬≈Ÿ§¿§¨°¢∏Ωæı§«§œ»ΩƒÍ§«§≠§ §§
      **          ∞Ÿ°¢Ã§¬–±˛
              WHEN      "190024710"
                  MOVE  "190024710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A302)ø∑¿∏ª˘∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£≤
      **          ¬ŒΩ≈£±°§£µ£∞£∞•∞•È•‡∞ æÂ§«°§∏¸¿∏œ´∆Ø¬Áø√§¨ƒÍ§·§Îº¿¥µ
      **          §ÚºÁ…¬§»§π§Î¥µº‘§œ£≥£µ∆¸∏¬≈Ÿ§¿§¨°¢∏Ωæı§«§œ»ΩƒÍ§«§≠§ §§
      **          ∞Ÿ°¢Ã§¬–±˛
              WHEN      "190140410"
                  MOVE  "190140410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303)¡ÌπÁº˛ª∫¥¸∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡°  Ï¬Œ°¶¬€ª˘°À
              WHEN      "190066710"
                  MOVE  "190066710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303)¡ÌπÁº˛ª∫¥¸∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡° ø∑¿∏ª˘°À
      **          ¬ŒΩ≈£±°§£µ£∞£∞•∞•È•‡∞ æÂ§«°§∏¸¿∏œ´∆Ø¬Áø√§¨ƒÍ§·§Îº¿¥µ
      **          §ÚºÁ…¬§»§π§Î¥µº‘§œ£≥£µ∆¸∏¬≈Ÿ§¿§¨°¢∏Ωæı§«§œ»ΩƒÍ§«§≠§ §§
      **          ∞Ÿ°¢Ã§¬–±˛
              WHEN      "190066810"
                  MOVE  "190066810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303-2)ø∑¿∏ª˘º£Œ≈≤Û…¸ºº∆˛±°∞ÂŒ≈¥…Õ˝Œ¡
      **          ¬ŒΩ≈£±°§£µ£∞£∞•∞•È•‡∞ æÂ§«°§∏¸¿∏œ´∆Ø¬Áø√§¨ƒÍ§·§Îº¿¥µ
      **          §ÚºÁ…¬§»§π§Î¥µº‘§œ£µ£∞∆¸∏¬≈Ÿ§¿§¨°¢∏Ωæı§«§œ»ΩƒÍ§«§≠§ §§
      **          ∞Ÿ°¢Ã§¬–±˛
              WHEN      "190140510"
                  MOVE  "190140510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  120        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A305)∞ÏŒ‡¥∂¿˜æ…¥µº‘∆˛±°∞ÂŒ≈¥…Õ˝Œ¡
              WHEN      "190194310"
                  MOVE  "190194310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  "190194410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A308-2)∞°µﬁ¿≠¥¸∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£±
              WHEN      "190117410"
                  MOVE  "190117410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  IF    LNK-TOKNYUIN2-TOKUBETSU-FLG(1)  =  "1"
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  ELSE
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-2)∞°µﬁ¿≠¥¸∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£±° ªÿƒÍ√œ∞Ë°À
              WHEN      "190150310"
                  MOVE  "190150310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-2)∞°µﬁ¿≠¥¸∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≤
              WHEN      "190129910"
                  MOVE  "190129910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  IF    LNK-TOKNYUIN2-TOKUBETSU-FLG(1)  =  "1"
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  ELSE
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-2)∞°µﬁ¿≠¥¸∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≤° ªÿƒÍ√œ∞Ë°À
              WHEN      "190150410"
                  MOVE  "190150410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£±
              WHEN      "190175310"
                  MOVE  "190175310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£±° ¿∏≥ËŒ≈Õ‹°À
              WHEN      "190175410"
                  MOVE  "190175410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£±
              WHEN      "190175510"
                  MOVE  "190175510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£±° ¿∏≥ËŒ≈Õ‹°À
              WHEN      "190175610"
                  MOVE  "190175610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£≤
              WHEN      "190175710"
                  MOVE  "190175710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£≤° ¿∏≥ËŒ≈Õ‹°À
              WHEN      "190175810"
                  MOVE  "190175810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≤
              WHEN      "190175910"
                  MOVE  "190175910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≤° ¿∏≥ËŒ≈Õ‹°À
              WHEN      "190176010"
                  MOVE  "190176010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£±° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190176110"
                  MOVE  "190176110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£±° ¿∏≥ËŒ≈Õ‹°À° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190176210"
                  MOVE  "190176210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£±° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190176310"
                  MOVE  "190176310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£±° ¿∏≥ËŒ≈Õ‹°À° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190176410"
                  MOVE  "190176410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£≤° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190176510"
                  MOVE  "190176510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£≤° ¿∏≥ËŒ≈Õ‹°À° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190176610"
                  MOVE  "190176610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≤° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190176710"
                  MOVE  "190176710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≤° ¿∏≥ËŒ≈Õ‹°À° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190176810"
                  MOVE  "190176810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A310)¥Àœ¬•±•¢…¬≈Ô∆˛±°Œ¡
              WHEN      "190028910"
                  MOVE  "190028910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190150510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190150610"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311)¿∫ø¿≤ µﬂµﬁ∆˛±°Œ¡£±
              WHEN      "190121010"
                  MOVE  "190121010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190111410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311)¿∫ø¿≤ µﬂµﬁ∆˛±°Œ¡£≤
              WHEN      "190130010"
                  MOVE  "190130010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311-2)¿∫ø¿≤ µﬁ¿≠¥¸º£Œ≈…¬≈Ô∆˛±°Œ¡£±
              WHEN      "190121110"
                  MOVE  "190121110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190066910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311-2)¿∫ø¿≤ µﬁ¿≠¥¸º£Œ≈…¬≈Ô∆˛±°Œ¡£≤
              WHEN      "190121210"
                  MOVE  "190121210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190067010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311-3)¿∫ø¿≤ µﬂµﬁ°¶πÁ ªæ…∆˛±°Œ¡
              WHEN      "190130210"
                  MOVE  "190130210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130310"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A314)«ß√Œæ…º£Œ≈…¬≈Ô∆˛±°Œ¡£±
              WHEN      "190151610"
                  MOVE  "190151610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190739910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190740010"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A314)«ß√Œæ…º£Œ≈…¬≈Ô∆˛±°Œ¡£≤
              WHEN      "190151710"
                  MOVE  "190151710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190813410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190813510"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A317)∆√ƒÍ∞Ï»Ã…¬≈Ô∆˛±°Œ¡° ∞°µﬁ¿≠¥¸∆˛±°∞ÂŒ≈¥…Õ˝°À
              WHEN      "190152610"
                  MOVE  "190152610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A317)∆√ƒÍ∞Ï»Ã…¬≈Ô∆˛±°Œ¡° ∞°µﬁ¿≠¥¸∆˛±°∞ÂŒ≈¥…Õ˝°¶•Í•œ•”•ÍªªƒÍ¥µº‘°À
              WHEN      "190152710"
                  MOVE  "190152710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A317)∆√ƒÍ∞Ï»Ã…¬≈Ô∆˛±°Œ¡° √œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝£±°À
              WHEN      "190182310"
                  MOVE  "190182310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A317)∆√ƒÍ∞Ï»Ã…¬≈Ô∆˛±°Œ¡° √œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝£≤°À
              WHEN      "190182410"
                  MOVE  "190182410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
             END-EVALUATE
           END-IF
      *
      *    ∆√ƒÍ∆˛±°Œ¡•≥°º•…§»ªªƒÍæÂ∏¬∆¸øÙ§Œ•ª•√•»°  ø¿Æ£≥£∞«Ø£¥∑Ó£±∆¸∞ πﬂ°À
           IF     LNK-TOKNYUIN2-SANTEIYMD   >=   "20180401"
             EVALUATE    LNK-TOKNYUIN2-SET-SRYCD
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£±
              WHEN      "190024510"
                  MOVE  "190024510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190128610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190074510"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£≤
              WHEN      "190024310"
                  MOVE  "190024310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190128710"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190024410"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£≥° µﬂÃøµﬂµﬁ∆˛±°Œ¡°À
              WHEN      "190138110"
                  MOVE  "190138110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138210"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138310"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£≥° π≠»œ∞œ«ÆΩ˝∆√ƒÍΩ∏√Êº£Œ≈°À
              WHEN      "190138410"
                  MOVE  "190138410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138610"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  60             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£¥° µﬂÃøµﬂµﬁ∆˛±°Œ¡°À
              WHEN      "190138710"
                  MOVE  "190138710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138810"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138910"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£¥° π≠»œ∞œ«ÆΩ˝∆√ƒÍΩ∏√Êº£Œ≈°À
              WHEN      "190139010"
                  MOVE  "190139010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190139110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190139210"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  "190139310"    TO  WRK-TOKNYUIN2-SRYCD(4)
                  MOVE  60             TO  WRK-JGN-NISSU(4)
                  MOVE  4              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£±
              WHEN      "190174410"
                  MOVE  "190174410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190174510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£≤° ∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡°À
              WHEN      "190174610"
                  MOVE  "190174610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190174710"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£≤° π≠»œ∞œ«ÆΩ˝∆√ƒÍΩ∏√Êº£Œ≈°À
              WHEN      "190174810"
                  MOVE  "190174810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190174910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£≥
              WHEN      "190116310"
                  MOVE  "190116310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190116410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£¥° ∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡°À
              WHEN      "190139810"
                  MOVE  "190139810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190139910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£¥° π≠»œ∞œ«ÆΩ˝∆√ƒÍΩ∏√Êº£Œ≈°À
              WHEN      "190140010"
                  MOVE  "190140010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190140110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-2)•œ•§•±•¢•Ê•À•√•»∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£±
              WHEN      "190175010"
                  MOVE  "190175010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-2)•œ•§•±•¢•Ê•À•√•»∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≤
              WHEN      "190175110"
                  MOVE  "190175110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-2)•œ•§•±•¢•Ê•À•√•»∆˛±°∞ÂŒ≈¥…Õ˝Œ¡
              WHEN      "190117310"
                  MOVE  "190117310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-3)«æ¬¥√Ê•±•¢•Ê•À•√•»∆˛±°∞ÂŒ≈¥…Õ˝Œ¡
              WHEN      "190120810"
                  MOVE  "190120810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-4)æÆª˘∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡
              WHEN      "190149910"
                  MOVE  "190149910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190150010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A302)ø∑¿∏ª˘∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£±
      **          ¬ŒΩ≈£±°§£µ£∞£∞•∞•È•‡∞ æÂ§«°§∏¸¿∏œ´∆Ø¬Áø√§¨ƒÍ§·§Îº¿¥µ
      **          §ÚºÁ…¬§»§π§Î¥µº‘§œ£≥£µ∆¸∏¬≈Ÿ§¿§¨°¢∏Ωæı§«§œ»ΩƒÍ§«§≠§ §§
      **          ∞Ÿ°¢Ã§¬–±˛
              WHEN      "190024710"
                  MOVE  "190024710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A302)ø∑¿∏ª˘∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£≤
      **          ¬ŒΩ≈£±°§£µ£∞£∞•∞•È•‡∞ æÂ§«°§∏¸¿∏œ´∆Ø¬Áø√§¨ƒÍ§·§Îº¿¥µ
      **          §ÚºÁ…¬§»§π§Î¥µº‘§œ£≥£µ∆¸∏¬≈Ÿ§¿§¨°¢∏Ωæı§«§œ»ΩƒÍ§«§≠§ §§
      **          ∞Ÿ°¢Ã§¬–±˛
              WHEN      "190140410"
                  MOVE  "190140410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303)¡ÌπÁº˛ª∫¥¸∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡°  Ï¬Œ°¶¬€ª˘°À
              WHEN      "190066710"
                  MOVE  "190066710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303)¡ÌπÁº˛ª∫¥¸∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡° ø∑¿∏ª˘°À
      **          ¬ŒΩ≈£±°§£µ£∞£∞•∞•È•‡∞ æÂ§«°§∏¸¿∏œ´∆Ø¬Áø√§¨ƒÍ§·§Îº¿¥µ
      **          §ÚºÁ…¬§»§π§Î¥µº‘§œ£≥£µ∆¸∏¬≈Ÿ§¿§¨°¢∏Ωæı§«§œ»ΩƒÍ§«§≠§ §§
      **          ∞Ÿ°¢Ã§¬–±˛
              WHEN      "190066810"
                  MOVE  "190066810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303-2)ø∑¿∏ª˘º£Œ≈≤Û…¸ºº∆˛±°∞ÂŒ≈¥…Õ˝Œ¡
      **          ¬ŒΩ≈£±°§£µ£∞£∞•∞•È•‡∞ æÂ§«°§∏¸¿∏œ´∆Ø¬Áø√§¨ƒÍ§·§Îº¿¥µ
      **          §ÚºÁ…¬§»§π§Î¥µº‘§œ£µ£∞∆¸∏¬≈Ÿ§¿§¨°¢∏Ωæı§«§œ»ΩƒÍ§«§≠§ §§
      **          ∞Ÿ°¢Ã§¬–±˛
              WHEN      "190140510"
                  MOVE  "190140510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  120        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A305)∞ÏŒ‡¥∂¿˜æ…¥µº‘∆˛±°∞ÂŒ≈¥…Õ˝Œ¡
              WHEN      "190194310"
                  MOVE  "190194310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  "190194410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£±
              WHEN      "190209110"
                  MOVE  "190209110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£±° ¿∏≥ËŒ≈Õ‹°À
              WHEN      "190209210"
                  MOVE  "190209210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£±
              WHEN      "190209310"
                  MOVE  "190209310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£±° ¿∏≥ËŒ≈Õ‹°À
              WHEN      "190209410"
                  MOVE  "190209410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£≤
              WHEN      "190209510"
                  MOVE  "190209510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£≤° ¿∏≥ËŒ≈Õ‹°À
              WHEN      "190209610"
                  MOVE  "190209610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≤
              WHEN      "190209710"
                  MOVE  "190209710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≤° ¿∏≥ËŒ≈Õ‹°À
              WHEN      "190209810"
                  MOVE  "190209810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£≥
              WHEN      "190209910"
                  MOVE  "190209910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£≥° ¿∏≥ËŒ≈Õ‹°À
              WHEN      "190210010"
                  MOVE  "190210010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≥
              WHEN      "190210110"
                  MOVE  "190210110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≥° ¿∏≥ËŒ≈Õ‹°À
              WHEN      "190210210"
                  MOVE  "190210210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£¥
              WHEN      "190210310"
                  MOVE  "190210310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£¥° ¿∏≥ËŒ≈Õ‹°À
              WHEN      "190210410"
                  MOVE  "190210410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£¥
              WHEN      "190210510"
                  MOVE  "190210510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£¥° ¿∏≥ËŒ≈Õ‹°À
              WHEN      "190210610"
                  MOVE  "190210610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£±° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190210710"
                  MOVE  "190210710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£±° ¿∏≥ËŒ≈Õ‹°À° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190210810"
                  MOVE  "190210810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£±° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190210910"
                  MOVE  "190210910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£±° ¿∏≥ËŒ≈Õ‹°À° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190211010"
                  MOVE  "190211010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£≤° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190211110"
                  MOVE  "190211110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£≤° ¿∏≥ËŒ≈Õ‹°À° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190211210"
                  MOVE  "190211210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≤° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190211310"
                  MOVE  "190211310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≤° ¿∏≥ËŒ≈Õ‹°À° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190211410"
                  MOVE  "190211410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£≥° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190211510"
                  MOVE  "190211510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£≥° ¿∏≥ËŒ≈Õ‹°À° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190211610"
                  MOVE  "190211610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≥° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190211710"
                  MOVE  "190211710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≥° ¿∏≥ËŒ≈Õ‹°À° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190211810"
                  MOVE  "190211810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£¥° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190211910"
                  MOVE  "190211910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£¥° ¿∏≥ËŒ≈Õ‹°À° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190212010"
                  MOVE  "190212010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£¥° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190212110"
                  MOVE  "190212110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£¥° ¿∏≥ËŒ≈Õ‹°À° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190212210"
                  MOVE  "190212210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A310)¥Àœ¬•±•¢…¬≈Ô∆˛±°Œ¡£±
              WHEN      "190028910"
                  MOVE  "190028910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190150510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190150610"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A310)¥Àœ¬•±•¢…¬≈Ô∆˛±°Œ¡£≤
              WHEN      "190212710"
                  MOVE  "190212710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190212810"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190212910"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311)¿∫ø¿≤ µﬂµﬁ∆˛±°Œ¡£±
              WHEN      "190121010"
                  MOVE  "190121010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190111410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311)¿∫ø¿≤ µﬂµﬁ∆˛±°Œ¡£≤
              WHEN      "190130010"
                  MOVE  "190130010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311-2)¿∫ø¿≤ µﬁ¿≠¥¸º£Œ≈…¬≈Ô∆˛±°Œ¡£±
              WHEN      "190121110"
                  MOVE  "190121110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190066910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311-2)¿∫ø¿≤ µﬁ¿≠¥¸º£Œ≈…¬≈Ô∆˛±°Œ¡£≤
              WHEN      "190121210"
                  MOVE  "190121210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190067010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311-3)¿∫ø¿≤ µﬂµﬁ°¶πÁ ªæ…∆˛±°Œ¡
              WHEN      "190130210"
                  MOVE  "190130210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130310"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A314)«ß√Œæ…º£Œ≈…¬≈Ô∆˛±°Œ¡£±
              WHEN      "190151610"
                  MOVE  "190151610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190739910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190740010"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A314)«ß√Œæ…º£Œ≈…¬≈Ô∆˛±°Œ¡£≤
              WHEN      "190151710"
                  MOVE  "190151710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190813410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190813510"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A317)∆√ƒÍ∞Ï»Ã…¬≈Ô∆˛±°Œ¡° √œ∞Ë Ò≥Á•±•¢£±°À
              WHEN      "190213310"
                  MOVE  "190213310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A317)∆√ƒÍ∞Ï»Ã…¬≈Ô∆˛±°Œ¡° √œ∞Ë Ò≥Á•±•¢£≤°À
              WHEN      "190213410"
                  MOVE  "190213410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A317)∆√ƒÍ∞Ï»Ã…¬≈Ô∆˛±°Œ¡° √œ∞Ë Ò≥Á•±•¢£≥°À
              WHEN      "190213510"
                  MOVE  "190213510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A317)∆√ƒÍ∞Ï»Ã…¬≈Ô∆˛±°Œ¡° √œ∞Ë Ò≥Á•±•¢£¥°À
              WHEN      "190213610"
                  MOVE  "190213610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
             END-EVALUATE
           END-IF
      *
      *    ∆√ƒÍ∆˛±°Œ¡§À•“•√•»§∑§øæÏπÁ§Œ•’•È•∞•ª•√•»
           IF        WRK-TOKNYUIN2-SRYCD(1)   NOT =   SPACE
               MOVE  1                 TO  FLG-HIT
           END-IF
      *
      *    ªªƒÍæÂ∏¬∆¸øÙ§Ú•◊•È•π∑◊ªª§«ª»Õ—§π§Î∞Ÿ§À•ﬁ•§• •π£±§∑§∆§™§Ø
           IF    (WRK-JGN-NISSU(1)     >=     1)   AND
                 (WRK-JGN-NISSU(1)     <=   99998)
               COMPUTE  WRK-JGN-NISSU(1)  =   WRK-JGN-NISSU(1)   -  1
           END-IF
           IF    (WRK-JGN-NISSU(2)     >=     1)   AND
                 (WRK-JGN-NISSU(2)     <=   99998)
               COMPUTE  WRK-JGN-NISSU(2)  =   WRK-JGN-NISSU(2)   -  1
           END-IF
           IF    (WRK-JGN-NISSU(3)     >=     1)   AND
                 (WRK-JGN-NISSU(3)     <=   99998)
               COMPUTE  WRK-JGN-NISSU(3)  =   WRK-JGN-NISSU(3)   -  1
           END-IF
           IF    (WRK-JGN-NISSU(4)     >=     1)   AND
                 (WRK-JGN-NISSU(4)     <=   99998)
               COMPUTE  WRK-JGN-NISSU(4)  =   WRK-JGN-NISSU(4)   -  1
           END-IF
           IF    (WRK-JGN-NISSU(5)     >=     1)   AND
                 (WRK-JGN-NISSU(5)     <=   99998)
               COMPUTE  WRK-JGN-NISSU(5)  =   WRK-JGN-NISSU(5)   -  1
           END-IF
      *
      *    ªªƒÍ∆√ƒÍ∆˛±°Œ¡§¨º£Œ≈ºº√±∞Ã∞ ≥∞§«§¢§ÎæÏπÁ°¢¥˚§ÀªªƒÍ∫—§ﬂ§Œ∆±∞Ï
      *    ∆√ƒÍ∆˛±°Œ¡§À§ƒ§§§∆ƒÃªª∆¸øÙ§ÚªªΩ–§π§Î
           IF   FLG-ROOM      =    0
             INITIALIZE                      ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
             IF      WRK-TOKNYUIN2-SRYCD(1)  NOT =   SPACE
                MOVE WRK-TOKNYUIN2-SRYCD(1)  TO  STUSAN-TOKUTEI-SRYCD(1)
             END-IF
             IF      WRK-TOKNYUIN2-SRYCD(2)  NOT =   SPACE
                MOVE WRK-TOKNYUIN2-SRYCD(2)  TO  STUSAN-TOKUTEI-SRYCD(2)
             END-IF
             IF      WRK-TOKNYUIN2-SRYCD(3)  NOT =   SPACE
                MOVE WRK-TOKNYUIN2-SRYCD(3)  TO  STUSAN-TOKUTEI-SRYCD(3)
             END-IF
             IF      WRK-TOKNYUIN2-SRYCD(4)  NOT =   SPACE
                MOVE WRK-TOKNYUIN2-SRYCD(4)  TO  STUSAN-TOKUTEI-SRYCD(4)
             END-IF
             IF      WRK-TOKNYUIN2-SRYCD(5)  NOT =   SPACE
                MOVE WRK-TOKNYUIN2-SRYCD(5)  TO  STUSAN-TOKUTEI-SRYCD(5)
             END-IF
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
             CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                             SPA-AREA
             MOVE    STUSAN-NISSU09          TO  LNK-TOKNYUIN2-NISSU08
           END-IF
      *
      *    ªªƒÍ∆√ƒÍ∆˛±°Œ¡§¨º£Œ≈ºº√±∞Ã§«§¢§ÎæÏπÁ°¢¥˚§ÀªªƒÍ∫—§ﬂ§Œº£Œ≈ºº
      *    √±∞Ã§Œ∆√ƒÍ∆˛±°Œ¡§À§ƒ§§§∆ƒÃªª∆¸øÙ§ÚªªΩ–§π§Î
           IF   FLG-ROOM      =    1
             PERFORM    201-TOKUTEI-ROOM-NISSU-SEC
           END-IF
      *
      *    ŒÚ∫Ó¿Æ§«≈–œø§∑§ø∆˛±°Õ˙ŒÚ§À§ƒ§§§∆°÷∆√ƒÍ∆˛±°°›ƒÃªª¬–æ›∆¸øÙ°◊§Œ
      *    ∆˛Œœ ¨∆¸øÙ§Ú≤√ªª§π§Î
           PERFORM    202-RRK-TOKUTEI-NISSU-SEC
      *
      *    ∞ ≤º£≤§ƒ§Œ∆√ƒÍ∆˛±°Œ¡§œΩÈ≤Û∆˛±°∆¸° ¬æ±° ¨§œ¥ﬁ§·§ §§°À§´§È§Œ
      *    ƒÃªª∆¸øÙ§À§Ë§ÍªªƒÍ§Úπ‘§¶
      *    (A314)«ß√Œæ…º£Œ≈…¬≈Ô∆˛±°Œ¡£±
      *    (A314)«ß√Œæ…º£Œ≈…¬≈Ô∆˛±°Œ¡£≤
           IF     LNK-TOKNYUIN2-SANTEIYMD   >=   "20160401"
              EVALUATE    LNK-TOKNYUIN2-SET-SRYCD
                WHEN      "190151610"
                WHEN      "190151710"
                INITIALIZE                      ORCSTUSANAREA
                MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
                MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
                MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
                CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                                SPA-AREA
                COMPUTE   LNK-TOKNYUIN2-NISSU08  =  STUSAN-NISSU03 +
                                                    STUSAN-TAINREL-NISSU
                IF      LNK-TOKNYUIN2-NISSU08   NOT =  ZERO
                     COMPUTE    LNK-TOKNYUIN2-NISSU08 =
                                LNK-TOKNYUIN2-NISSU08  -  1
                END-IF
             END-EVALUATE
           END-IF
      *
      *    ≤˛¿µ§À§Ë§Í∆√ƒÍ∆˛±°Œ¡§ŒªªƒÍ•≥°º•…§¨ —ππ§À§ §√§ø∆˛±°Œ¡§À§ƒ§§§∆
      *    Ωæ¡∞§ŒªªƒÍ•≥°º•…§Œ∆¸øÙ§‚ƒÃªª§π§Î…¨Õ◊§¨§¢§Î∞Ÿ°¢ƒÃªª∆¸øÙ§ŒºË∆¿
      *    §Ú —ππ°  ø¿Æ£≥£∞«Ø≤˛¿µ°À
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£±
              WHEN      "190209110"
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£±° ¿∏≥ËŒ≈Õ‹°À
              WHEN      "190209210"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£±
              WHEN      "190209310"
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£±° ¿∏≥ËŒ≈Õ‹°À
              WHEN      "190209410"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£≤
              WHEN      "190209510"
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£≤° ¿∏≥ËŒ≈Õ‹°À
              WHEN      "190209610"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≤
              WHEN      "190209710"
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≤° ¿∏≥ËŒ≈Õ‹°À
              WHEN      "190209810"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£±° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190210710"
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£±° ¿∏≥ËŒ≈Õ‹°À° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190210810"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£±° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190210910"
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£±° ¿∏≥ËŒ≈Õ‹°À° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190211010"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£≤° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190211110"
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£≤° ¿∏≥ËŒ≈Õ‹°À° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190211210"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≤° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190211310"
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≤° ¿∏≥ËŒ≈Õ‹°À° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190211410"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£≥
              WHEN      "190209910"
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£≥° ¿∏≥ËŒ≈Õ‹°À
              WHEN      "190210010"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≥
              WHEN      "190210110"
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≥° ¿∏≥ËŒ≈Õ‹°À
              WHEN      "190210210"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£¥
              WHEN      "190210310"
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£¥° ¿∏≥ËŒ≈Õ‹°À
              WHEN      "190210410"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£¥
              WHEN      "190210510"
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£¥° ¿∏≥ËŒ≈Õ‹°À
              WHEN      "190210610"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£≥° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190211510"
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£≥° ¿∏≥ËŒ≈Õ‹°À° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190211610"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≥° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190211710"
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≥° ¿∏≥ËŒ≈Õ‹°À° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190211810"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£¥° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190211910"
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£¥° ¿∏≥ËŒ≈Õ‹°À° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190212010"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£¥° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190212110"
      *      (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£¥° ¿∏≥ËŒ≈Õ‹°À° ∆√ƒÍ√œ∞Ë°À
              WHEN      "190212210"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A317)∆√ƒÍ∞Ï»Ã…¬≈Ô∆˛±°Œ¡° √œ∞Ë Ò≥Á•±•¢£±°À
              WHEN      "190213310"
             INITIALIZE                      ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
             MOVE    "190213310"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE    "190182310"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    "190182410"             TO  STUSAN-TOKUTEI-SRYCD(3)
             CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                             SPA-AREA
             MOVE    STUSAN-NISSU09          TO  LNK-TOKNYUIN2-NISSU08
             ADD     WRK-TOKU-TAISYONISSU    TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A317)∆√ƒÍ∞Ï»Ã…¬≈Ô∆˛±°Œ¡° √œ∞Ë Ò≥Á•±•¢£≤°À
              WHEN      "190213410"
             INITIALIZE                      ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
             MOVE    "190213410"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE    "190182310"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    "190182410"             TO  STUSAN-TOKUTEI-SRYCD(3)
             CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                             SPA-AREA
             MOVE    STUSAN-NISSU09          TO  LNK-TOKNYUIN2-NISSU08
             ADD     WRK-TOKU-TAISYONISSU    TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A317)∆√ƒÍ∞Ï»Ã…¬≈Ô∆˛±°Œ¡° √œ∞Ë Ò≥Á•±•¢£≥°À
              WHEN      "190213510"
             INITIALIZE                      ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
             MOVE    "190213510"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE    "190182310"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    "190182410"             TO  STUSAN-TOKUTEI-SRYCD(3)
             CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                             SPA-AREA
             MOVE    STUSAN-NISSU09          TO  LNK-TOKNYUIN2-NISSU08
             ADD     WRK-TOKU-TAISYONISSU    TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A317)∆√ƒÍ∞Ï»Ã…¬≈Ô∆˛±°Œ¡° √œ∞Ë Ò≥Á•±•¢£¥°À
              WHEN      "190213610"
             INITIALIZE                      ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
             MOVE    "190213610"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE    "190182310"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    "190182410"             TO  STUSAN-TOKUTEI-SRYCD(3)
             CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                             SPA-AREA
             MOVE    STUSAN-NISSU09          TO  LNK-TOKNYUIN2-NISSU08
             ADD     WRK-TOKU-TAISYONISSU    TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
      *    ª‹¿ﬂ¥Ω‡ —ππ§À§Ë§Í¥Àœ¬•±•¢…¬≈Ô∆˛±°Œ¡§Œ£±§»£≤§¨∫Æ∫ﬂ§π§ÎæÏπÁ§À
      *    ∆√ƒÍ∆˛±°Œ¡§ŒªªƒÍ•≥°º•…§¨ —ππ§À§ §√§ø∆˛±°Œ¡§À§ƒ§§§∆
      *    Ωæ¡∞§ŒªªƒÍ•≥°º•…§Œ∆¸øÙ§‚ƒÃªª§π§Î…¨Õ◊§¨§¢§Î∞Ÿ°¢ƒÃªª∆¸øÙ§ŒºË∆¿
      *    §Ú —ππ
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A310)¥Àœ¬•±•¢…¬≈Ô∆˛±°Œ¡£±
              WHEN      "190028910"
             INITIALIZE                      ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
             MOVE    "190028910"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE    "190150510"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    "190150610"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE    "190212710"             TO  STUSAN-TOKUTEI-SRYCD(4)
             MOVE    "190212810"             TO  STUSAN-TOKUTEI-SRYCD(5)
             MOVE    "190212910"             TO  STUSAN-TOKUTEI-SRYCD(6)
             CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                             SPA-AREA
             MOVE    STUSAN-NISSU09          TO  LNK-TOKNYUIN2-NISSU08
             ADD     WRK-TOKU-TAISYONISSU    TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A310)¥Àœ¬•±•¢…¬≈Ô∆˛±°Œ¡£≤
              WHEN      "190212710"
             INITIALIZE                      ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
             MOVE    "190028910"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE    "190150510"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    "190150610"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE    "190212710"             TO  STUSAN-TOKUTEI-SRYCD(4)
             MOVE    "190212810"             TO  STUSAN-TOKUTEI-SRYCD(5)
             MOVE    "190212910"             TO  STUSAN-TOKUTEI-SRYCD(6)
             CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                             SPA-AREA
             MOVE    STUSAN-NISSU09          TO  LNK-TOKNYUIN2-NISSU08
             ADD     WRK-TOKU-TAISYONISSU    TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
      *    ∆√ƒÍ∆˛±°Œ¡•≥°º•…§¨£±ºÔŒ‡§ŒæÏπÁ
           IF    WRK-SRYCD-COUNT       =   1
                 MOVE   ZERO           TO  TN-IDX
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(1)
                     ADD   1           TO  TN-IDX
      *              ∆√ƒÍ∆˛±°Œ¡•≥°º•…° £±°À
                     MOVE  WRK-TOKNYUIN2-SRYCD(1)
                                       TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *              ∆√ƒÍ∆˛±°≥´ªœ∆¸° £±°À
                     MOVE   LNK-TOKNYUIN2-SANTEIYMD
                                       TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
      *              ∆√ƒÍ∆˛±°Ω™Œª∆¸° £±°À
                   IF    WRK-JGN-NISSU(1) =   99999
                     MOVE  "99999999"   TO  LNK-TOKNYUIN2-EDYMD (TN-IDX)
                   ELSE
                     MOVE     ZERO     TO  WRK-TOKUTEI-DAY
                     COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(1)  -
                                                 LNK-TOKNYUIN2-NISSU08
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-TOKNYUIN2-SANTEIYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WRK-TOKUTEI-DAY   TO  LNK-DAY6-ZOGEN
                     CALL    "ORCSDAY"         USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
      *              ∞Ï»Ã∆˛±°≤Ò∑◊∫Ó¿Æ•’•È•∞§»∫Ó¿Æ≥´ªœ∆¸§Œ•ª•√•»
                     MOVE    "1"     TO   LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                     INITIALIZE                 STS-AREA-DAY
                     INITIALIZE                 LNK-DAY6-AREA
                     MOVE    "61"           TO  LNK-DAY6-IRAI
                     MOVE    LNK-TOKNYUIN2-EDYMD (TN-IDX)
                                            TO  LNK-DAY6-KIJUN
                     MOVE    "1"            TO  LNK-DAY6-ZOGENPTN
                     MOVE     1             TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                    LNK-DAY6-AREA
                     MOVE   LNK-DAY6-KEISAN TO
                                          LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                   END-IF
                 END-IF
      *
           END-IF
      *
      *    ∆√ƒÍ∆˛±°Œ¡•≥°º•…§¨£≤ºÔŒ‡§ŒæÏπÁ
           IF    WRK-SRYCD-COUNT       =   2
                 MOVE   ZERO           TO  TN-IDX
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(1)
                     ADD   1           TO  TN-IDX
      *              ∆√ƒÍ∆˛±°Œ¡•≥°º•…° £±°À
                     MOVE  WRK-TOKNYUIN2-SRYCD(1)
                                       TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *              ∆√ƒÍ∆˛±°≥´ªœ∆¸° £±°À
                     MOVE   LNK-TOKNYUIN2-SANTEIYMD
                                       TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
      *              ∆√ƒÍ∆˛±°Ω™Œª∆¸° £±°À
                     MOVE     ZERO     TO  WRK-TOKUTEI-DAY
                     COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(1)  -
                                                 LNK-TOKNYUIN2-NISSU08
                     COMPUTE  WRK-TOKUTEI-DAY1 = WRK-TOKUTEI-DAY + 1
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-TOKNYUIN2-SANTEIYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WRK-TOKUTEI-DAY   TO  LNK-DAY6-ZOGEN
                     CALL    "ORCSDAY"         USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
                 END-IF
      *
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(2)
      *
                   ADD    1   TO    TN-IDX
      *
      *            ∆√ƒÍ∆˛±°Œ¡•≥°º•…° £≤°À
                   MOVE   WRK-TOKNYUIN2-SRYCD(2)
                                        TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *            ∆√ƒÍ∆˛±°≥´ªœ∆¸° £≤°À
                   INITIALIZE                 STS-AREA-DAY
                   INITIALIZE                 LNK-DAY6-AREA
                   MOVE    "61"           TO  LNK-DAY6-IRAI
                   IF      TN-IDX         =   2
                       MOVE    LNK-TOKNYUIN2-EDYMD (1)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                   ELSE
                       MOVE    LNK-TOKNYUIN2-SANTEIYMD
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                   END-IF
      *            ∆√ƒÍ∆˛±°Ω™Œª∆¸° £≤°À
                   IF    WRK-JGN-NISSU(2) =   99999
                     MOVE  "99999999"   TO  LNK-TOKNYUIN2-EDYMD (TN-IDX)
                   ELSE
                     MOVE     ZERO      TO  WRK-TOKUTEI-DAY
                     COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(2)  -
                                                 LNK-TOKNYUIN2-NISSU08 -
                                                 WRK-TOKUTEI-DAY1
                     INITIALIZE              STS-AREA-DAY
                     INITIALIZE              LNK-DAY6-AREA
                     MOVE    "61"            TO  LNK-DAY6-IRAI
                     MOVE    LNK-TOKNYUIN2-STYMD (TN-IDX)
                                             TO  LNK-DAY6-KIJUN
                     MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                     MOVE    WRK-TOKUTEI-DAY TO  LNK-DAY6-ZOGEN
                     CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                     LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
      *              ∞Ï»Ã∆˛±°≤Ò∑◊∫Ó¿Æ•’•È•∞§»∫Ó¿Æ≥´ªœ∆¸§Œ•ª•√•»
                     MOVE    "1"     TO   LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                     INITIALIZE                 STS-AREA-DAY
                     INITIALIZE                 LNK-DAY6-AREA
                     MOVE    "61"           TO  LNK-DAY6-IRAI
                     MOVE    LNK-TOKNYUIN2-EDYMD (TN-IDX)
                                            TO  LNK-DAY6-KIJUN
                     MOVE    "1"            TO  LNK-DAY6-ZOGENPTN
                     MOVE     1             TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                    LNK-DAY6-AREA
                     MOVE   LNK-DAY6-KEISAN TO
                                          LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                   END-IF
                 END-IF
           END-IF
      *
      *    ∆√ƒÍ∆˛±°Œ¡•≥°º•…§¨£≥ºÔŒ‡§ŒæÏπÁ
           IF    WRK-SRYCD-COUNT       =   3
                 MOVE   ZERO           TO  TN-IDX
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(1)
                     ADD   1           TO  TN-IDX
      *              ∆√ƒÍ∆˛±°Œ¡•≥°º•…° £±°À
                     MOVE  WRK-TOKNYUIN2-SRYCD(1)
                                       TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *              ∆√ƒÍ∆˛±°≥´ªœ∆¸° £±°À
                     MOVE   LNK-TOKNYUIN2-SANTEIYMD
                                       TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
      *              ∆√ƒÍ∆˛±°Ω™Œª∆¸° £±°À
                     MOVE     ZERO     TO  WRK-TOKUTEI-DAY
                     COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(1)  -
                                                 LNK-TOKNYUIN2-NISSU08
                     COMPUTE  WRK-TOKUTEI-DAY1 = WRK-TOKUTEI-DAY + 1
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-TOKNYUIN2-SANTEIYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WRK-TOKUTEI-DAY   TO  LNK-DAY6-ZOGEN
                     CALL    "ORCSDAY"         USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
                 END-IF
      *
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(2)
      *
                   ADD    1   TO    TN-IDX
      *
      *            ∆√ƒÍ∆˛±°Œ¡•≥°º•…° £≤°À
                   MOVE   WRK-TOKNYUIN2-SRYCD(2)
                                        TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *            ∆√ƒÍ∆˛±°≥´ªœ∆¸° £≤°À
                   INITIALIZE                 STS-AREA-DAY
                   INITIALIZE                 LNK-DAY6-AREA
                   MOVE    "61"           TO  LNK-DAY6-IRAI
                   IF      TN-IDX         =   2
                       MOVE    LNK-TOKNYUIN2-EDYMD (1)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                   ELSE
                       MOVE    LNK-TOKNYUIN2-SANTEIYMD
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                   END-IF
      *            ∆√ƒÍ∆˛±°Ω™Œª∆¸° £≤°À
                   MOVE     ZERO      TO  WRK-TOKUTEI-DAY
                   COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(2)  -
                                               LNK-TOKNYUIN2-NISSU08 -
                                               WRK-TOKUTEI-DAY1
      **             DISPLAY "=== KAKUNIN DISPLAY ==="
      **             DISPLAY  "WRK-JGN-NISSU(2) = " WRK-JGN-NISSU(2)
      **             DISPLAY  "LNK-TOKNYUIN2-NISSU08 = " 
      **                       LNK-TOKNYUIN2-NISSU08
      **             DISPLAY  "WRK-TOKUTEI-DAY1 = " WRK-TOKUTEI-DAY1
      **             DISPLAY  "WRK-TOKUTEI-DAY = " WRK-TOKUTEI-DAY
      **             DISPLAY "=== KAKUNIN DISPLAY END ==="
                   COMPUTE  WRK-TOKUTEI-DAY2 = WRK-TOKUTEI-DAY + 1
                   INITIALIZE              STS-AREA-DAY
                   INITIALIZE              LNK-DAY6-AREA
                   MOVE    "61"            TO  LNK-DAY6-IRAI
                   MOVE    LNK-TOKNYUIN2-STYMD (TN-IDX)
                                           TO  LNK-DAY6-KIJUN
                   MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                   MOVE    WRK-TOKUTEI-DAY TO  LNK-DAY6-ZOGEN
                   CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN
                                        TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
                 END-IF
      *
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(3)
      *
                   ADD    1   TO    TN-IDX
      *
      *            ∆√ƒÍ∆˛±°Œ¡•≥°º•…° £≥°À
                   MOVE   WRK-TOKNYUIN2-SRYCD(3)
                                        TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *            ∆√ƒÍ∆˛±°≥´ªœ∆¸° £≥°À
                   INITIALIZE                 STS-AREA-DAY
                   INITIALIZE                 LNK-DAY6-AREA
                   MOVE    "61"           TO  LNK-DAY6-IRAI
                   EVALUATE  TN-IDX
                     WHEN  1
                       MOVE    LNK-TOKNYUIN2-SANTEIYMD
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                     WHEN  2
                       MOVE    LNK-TOKNYUIN2-EDYMD (1)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                     WHEN  3
                       MOVE    LNK-TOKNYUIN2-EDYMD (2)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                   END-EVALUATE
      *            ∆√ƒÍ∆˛±°Ω™Œª∆¸° £≥°À
                   IF    WRK-JGN-NISSU(3) =   99999
                     MOVE  "99999999"   TO  LNK-TOKNYUIN2-EDYMD (TN-IDX)
                   ELSE
                     MOVE     ZERO      TO  WRK-TOKUTEI-DAY
                     COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(3)  -
                                                 LNK-TOKNYUIN2-NISSU08 -
                                                 WRK-TOKUTEI-DAY1  -
                                                 WRK-TOKUTEI-DAY2
                     INITIALIZE              STS-AREA-DAY
                     INITIALIZE              LNK-DAY6-AREA
                     MOVE    "61"            TO  LNK-DAY6-IRAI
                     MOVE    LNK-TOKNYUIN2-STYMD (TN-IDX)
                                             TO  LNK-DAY6-KIJUN
                     MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                     MOVE    WRK-TOKUTEI-DAY TO  LNK-DAY6-ZOGEN
                     CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                     LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
      *              ∞Ï»Ã∆˛±°≤Ò∑◊∫Ó¿Æ•’•È•∞§»∫Ó¿Æ≥´ªœ∆¸§Œ•ª•√•»
                     MOVE    "1"     TO   LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                     INITIALIZE                 STS-AREA-DAY
                     INITIALIZE                 LNK-DAY6-AREA
                     MOVE    "61"           TO  LNK-DAY6-IRAI
                     MOVE    LNK-TOKNYUIN2-EDYMD (TN-IDX)
                                            TO  LNK-DAY6-KIJUN
                     MOVE    "1"            TO  LNK-DAY6-ZOGENPTN
                     MOVE     1             TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                    LNK-DAY6-AREA
                     MOVE   LNK-DAY6-KEISAN TO
                                          LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                   END-IF
                 END-IF
           END-IF
      *
      *    ∆√ƒÍ∆˛±°Œ¡•≥°º•…§¨£¥ºÔŒ‡§ŒæÏπÁ
           IF    WRK-SRYCD-COUNT       =   4
                 MOVE   ZERO           TO  TN-IDX
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(1)
                     ADD   1           TO  TN-IDX
      *              ∆√ƒÍ∆˛±°Œ¡•≥°º•…° £±°À
                     MOVE  WRK-TOKNYUIN2-SRYCD(1)
                                       TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *              ∆√ƒÍ∆˛±°≥´ªœ∆¸° £±°À
                     MOVE   LNK-TOKNYUIN2-SANTEIYMD
                                       TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
      *              ∆√ƒÍ∆˛±°Ω™Œª∆¸° £±°À
                     MOVE     ZERO     TO  WRK-TOKUTEI-DAY
                     COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(1)  -
                                                 LNK-TOKNYUIN2-NISSU08
                     COMPUTE  WRK-TOKUTEI-DAY1 = WRK-TOKUTEI-DAY + 1
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-TOKNYUIN2-SANTEIYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WRK-TOKUTEI-DAY   TO  LNK-DAY6-ZOGEN
                     CALL    "ORCSDAY"         USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
                 END-IF
      *
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(2)
      *
                   ADD    1   TO    TN-IDX
      *
      *            ∆√ƒÍ∆˛±°Œ¡•≥°º•…° £≤°À
                   MOVE   WRK-TOKNYUIN2-SRYCD(2)
                                        TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *            ∆√ƒÍ∆˛±°≥´ªœ∆¸° £≤°À
                   INITIALIZE                 STS-AREA-DAY
                   INITIALIZE                 LNK-DAY6-AREA
                   MOVE    "61"           TO  LNK-DAY6-IRAI
                   IF      TN-IDX         =   2
                       MOVE    LNK-TOKNYUIN2-EDYMD (1)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                   ELSE
                       MOVE    LNK-TOKNYUIN2-SANTEIYMD
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                   END-IF
      *            ∆√ƒÍ∆˛±°Ω™Œª∆¸° £≤°À
                   MOVE     ZERO      TO  WRK-TOKUTEI-DAY
                   COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(2)  -
                                               LNK-TOKNYUIN2-NISSU08 -
                                               WRK-TOKUTEI-DAY1
                   COMPUTE  WRK-TOKUTEI-DAY2 = WRK-TOKUTEI-DAY + 1
                   INITIALIZE              STS-AREA-DAY
                   INITIALIZE              LNK-DAY6-AREA
                   MOVE    "61"            TO  LNK-DAY6-IRAI
                   MOVE    LNK-TOKNYUIN2-STYMD (TN-IDX)
                                           TO  LNK-DAY6-KIJUN
                   MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                   MOVE    WRK-TOKUTEI-DAY TO  LNK-DAY6-ZOGEN
                   CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN
                                        TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
                 END-IF
      *
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(3)
      *
                   ADD    1   TO    TN-IDX
      *
      *            ∆√ƒÍ∆˛±°Œ¡•≥°º•…° £≥°À
                   MOVE   WRK-TOKNYUIN2-SRYCD(3)
                                        TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *            ∆√ƒÍ∆˛±°≥´ªœ∆¸° £≥°À
                   INITIALIZE                 STS-AREA-DAY
                   INITIALIZE                 LNK-DAY6-AREA
                   MOVE    "61"           TO  LNK-DAY6-IRAI
                   EVALUATE  TN-IDX
                     WHEN  1
                       MOVE    LNK-TOKNYUIN2-SANTEIYMD
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                     WHEN  2
                       MOVE    LNK-TOKNYUIN2-EDYMD (1)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                     WHEN  3
                       MOVE    LNK-TOKNYUIN2-EDYMD (2)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                   END-EVALUATE
      *            ∆√ƒÍ∆˛±°Ω™Œª∆¸° £≥°À
                   MOVE     ZERO      TO  WRK-TOKUTEI-DAY
                   COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(3)  -
                                               LNK-TOKNYUIN2-NISSU08 -
                                               WRK-TOKUTEI-DAY1  -
                                               WRK-TOKUTEI-DAY2
                   COMPUTE  WRK-TOKUTEI-DAY3 = WRK-TOKUTEI-DAY + 1
                   INITIALIZE              STS-AREA-DAY
                   INITIALIZE              LNK-DAY6-AREA
                   MOVE    "61"            TO  LNK-DAY6-IRAI
                   MOVE    LNK-TOKNYUIN2-STYMD (TN-IDX)
                                           TO  LNK-DAY6-KIJUN
                   MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                   MOVE    WRK-TOKUTEI-DAY TO  LNK-DAY6-ZOGEN
                   CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN
                                      TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
                 END-IF
      *
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(4)
      *
                   ADD    1   TO    TN-IDX
      *
      *            ∆√ƒÍ∆˛±°Œ¡•≥°º•…° £¥°À
                   MOVE   WRK-TOKNYUIN2-SRYCD(4)
                                        TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *            ∆√ƒÍ∆˛±°≥´ªœ∆¸° £¥°À
                   INITIALIZE                 STS-AREA-DAY
                   INITIALIZE                 LNK-DAY6-AREA
                   MOVE    "61"           TO  LNK-DAY6-IRAI
                   EVALUATE  TN-IDX
                     WHEN  1
                       MOVE    LNK-TOKNYUIN2-SANTEIYMD
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                     WHEN  2
                       MOVE    LNK-TOKNYUIN2-EDYMD (1)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                     WHEN  3
                       MOVE    LNK-TOKNYUIN2-EDYMD (2)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                     WHEN  4
                       MOVE    LNK-TOKNYUIN2-EDYMD (3)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                   END-EVALUATE
      *            ∆√ƒÍ∆˛±°Ω™Œª∆¸° £¥°À
                   IF    WRK-JGN-NISSU(4) =   99999
                     MOVE  "99999999"   TO  LNK-TOKNYUIN2-EDYMD (TN-IDX)
                   ELSE
                     MOVE     ZERO      TO  WRK-TOKUTEI-DAY
                     COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(4)  -
                                                 LNK-TOKNYUIN2-NISSU08 -
                                                 WRK-TOKUTEI-DAY1 -
                                                 WRK-TOKUTEI-DAY2 -
                                                 WRK-TOKUTEI-DAY3
                     INITIALIZE              STS-AREA-DAY
                     INITIALIZE              LNK-DAY6-AREA
                     MOVE    "61"            TO  LNK-DAY6-IRAI
                     MOVE    LNK-TOKNYUIN2-STYMD (TN-IDX)
                                             TO  LNK-DAY6-KIJUN
                     MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                     MOVE    WRK-TOKUTEI-DAY TO  LNK-DAY6-ZOGEN
                     CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                     LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
      *              ∞Ï»Ã∆˛±°≤Ò∑◊∫Ó¿Æ•’•È•∞§»∫Ó¿Æ≥´ªœ∆¸§Œ•ª•√•»
                     MOVE    "1"     TO   LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                     INITIALIZE                 STS-AREA-DAY
                     INITIALIZE                 LNK-DAY6-AREA
                     MOVE    "61"           TO  LNK-DAY6-IRAI
                     MOVE    LNK-TOKNYUIN2-EDYMD (TN-IDX)
                                            TO  LNK-DAY6-KIJUN
                     MOVE    "1"            TO  LNK-DAY6-ZOGENPTN
                     MOVE     1             TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                    LNK-DAY6-AREA
                     MOVE   LNK-DAY6-KEISAN TO
                                          LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                   END-IF
                 END-IF
           END-IF
      *
           DISPLAY   "TN-IDX = " TN-IDX
      *    ∆√ƒÍ∆˛±°¥¸¥÷§Ú∑–≤·§∑§ø∫›§Œ∞Ï»Ã∆˛±°≤Ò∑◊∫Ó¿ÆÕ—§Œ•’•È•∞¿ﬂƒÍ
           EVALUATE   LNK-TOKNYUIN2-IDOKBN
               WHEN   "1"
                   MOVE    LNK-TOKNYUIN2-SANTEIYMD
                                              TO WRK-IPN-HANTEI-YMD1
                   IF      WRK-IPN-HANTEI-M1   =   12
                         ADD    1     TO      WRK-IPN-HANTEI-Y1
                         MOVE   1     TO      WRK-IPN-HANTEI-M1
                   ELSE
                         ADD    1     TO      WRK-IPN-HANTEI-M1
                   END-IF
               WHEN   "2"
                   MOVE    LNK-TOKNYUIN2-SANTEIYMD
                                              TO WRK-IPN-HANTEI-YMD1
           END-EVALUATE
      *    ∑ÓÀˆ∆¸§ÚªªƒÍ
           INITIALIZE                STS-AREA-DAY
           INITIALIZE                LNK-DAY7-AREA
           MOVE    "71"              TO  LNK-DAY7-IRAI
           MOVE   WRK-IPN-HANTEI-YM1 TO  LNK-DAY7-YM
           CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                             LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI   TO      WRK-IPN-HANTEI-YMD1
      *    ∆√ƒÍ∆˛±°Œ¡§ŒΩ™Œª∆¸…’§Ú•ª•√•»
           IF     TN-IDX      NOT =     ZERO
               MOVE    LNK-TOKNYUIN2-EDYMD(TN-IDX)
                                     TO      WRK-IPN-HANTEI-YMD2
           ELSE
               MOVE    "00000000"    TO      WRK-IPN-HANTEI-YMD2
           END-IF
      *    ∞Ï»Ã∆˛±°≤Ò∑◊∫Ó¿ÆÕ—§Œ•’•È•∞¿ﬂƒÍ
           DISPLAY "WRK-IPN-HANTEI-YMD1 = " WRK-IPN-HANTEI-YMD1
           DISPLAY "WRK-IPN-HANTEI-YMD2 = " WRK-IPN-HANTEI-YMD2
           IF    WRK-IPN-HANTEI-YMD1    >   WRK-IPN-HANTEI-YMD2
               MOVE    "1"        TO     LNK-TOKNYUIN2-IPN-KAIKEI-FLG
               IF    WRK-IPN-HANTEI-YMD2   =   "00000000"
                   MOVE   LNK-TOKNYUIN2-SANTEIYMD
                                  TO     LNK-TOKNYUIN2-IPN-KAIKEI-YMD
               END-IF
           ELSE
               MOVE    SPACE      TO     LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                                         LNK-TOKNYUIN2-IPN-KAIKEI-YMD
           END-IF
      *
           IF        FLG-HIT    =    ZERO
               MOVE    SPACE      TO     LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                                         LNK-TOKNYUIN2-IPN-KAIKEI-YMD
           END-IF
      *
      *    Œ≈Õ‹…¬≈Ô° …¬æ≤°À§Œ∆˛±°ª˛§œ∆√ƒÍ∆˛±°Œ¡ªªƒÍ∏Â§Œ∞Ï»Ã∆˛±°≤Ò∑◊§œ
      *    ∫Ó¿Æ§∑§ §§
           IF     LNK-TOKNYUIN2-TOKUBETSU-FLG(2)    =    "1"
               MOVE    SPACE      TO     LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                                         LNK-TOKNYUIN2-IPN-KAIKEI-YMD
           END-IF
      *
           DISPLAY  "====  ORCSTOKNYUIN2 HENKYAKU  ===="
           DISPLAY  "LNK-TOKNYUIN2-SRYCD(1) = " LNK-TOKNYUIN2-SRYCD(1)
                 " " LNK-TOKNYUIN2-STYMD(1) " " LNK-TOKNYUIN2-EDYMD(1)
           DISPLAY  "LNK-TOKNYUIN2-SRYCD(2) = " LNK-TOKNYUIN2-SRYCD(2)
                 " " LNK-TOKNYUIN2-STYMD(2) " " LNK-TOKNYUIN2-EDYMD(2)
           DISPLAY  "LNK-TOKNYUIN2-SRYCD(3) = " LNK-TOKNYUIN2-SRYCD(3)
                 " " LNK-TOKNYUIN2-STYMD(3) " " LNK-TOKNYUIN2-EDYMD(3)
           DISPLAY  "LNK-TOKNYUIN2-SRYCD(4) = " LNK-TOKNYUIN2-SRYCD(4)
                 " " LNK-TOKNYUIN2-STYMD(4) " " LNK-TOKNYUIN2-EDYMD(4)
           DISPLAY  "LNK-TOKNYUIN2-SRYCD(5) = " LNK-TOKNYUIN2-SRYCD(5)
                 " " LNK-TOKNYUIN2-STYMD(5) " " LNK-TOKNYUIN2-EDYMD(5)
           DISPLAY  "LNK-TOKNYUIN2-IPN-KAIKEI-FLG = "
                     LNK-TOKNYUIN2-IPN-KAIKEI-FLG
           DISPLAY  "LNK-TOKNYUIN2-IPN-KAIKEI-YMD = "
                     LNK-TOKNYUIN2-IPN-KAIKEI-YMD
      *
           IF   LNK-TOKNYUIN2-SRYCD(1)  =      SPACE
               MOVE    1                TO     LNK-TOKNYUIN2-RC
           END-IF
           IF     FLG-HIT     =    ZERO
                  MOVE    2             TO     LNK-TOKNYUIN2-RC
           END-IF
      *
      *
           .
       200-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    º£Œ≈ºº√±∞Ã§Œ∆√ƒÍ∆˛±°Œ¡ƒÃªª∆¸øÙ°  ø¿Æ£≤£∂«Ø£¥∑Ó£±∆¸∞ πﬂ°À
      *****************************************************************
       201-TOKUTEI-ROOM-NISSU-SEC                    SECTION.
      *
           MOVE   ZERO                  TO  WRK-TOKUTEI-ROOM-NISSU
      *
           IF    LNK-TOKNYUIN2-NISSU08   NOT =    ZERO
      *     (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£±
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190024510"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190128610"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE   "190074510"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£≤
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190024310"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190128710"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE   "190024410"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£≥° µﬂÃøµﬂµﬁ∆˛±°Œ¡°À
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190138110"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190138210"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE   "190138310"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£≥° π≠»œ∞œ«ÆΩ˝∆√ƒÍΩ∏√Êº£Œ≈°À
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190138410"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190138510"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE   "190138610"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£¥° µﬂÃøµﬂµﬁ∆˛±°Œ¡°À
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190138710"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190138810"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE   "190138910"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A300)µﬂÃøµﬂµﬁ∆˛±°Œ¡£¥° π≠»œ∞œ«ÆΩ˝∆√ƒÍΩ∏√Êº£Œ≈°À
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190139010"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190139110"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE   "190139210"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE   "190139310"             TO  STUSAN-TOKUTEI-SRYCD(4)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A301)∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£±
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190174410"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190174510"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A301)∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£≤° ∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡°À
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190174610"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190174710"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A301)∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£≤° π≠»œ∞œ«ÆΩ˝∆√ƒÍΩ∏√Êº£Œ≈°À
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190174810"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190174910"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A301)∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£≥
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190116310"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190116410"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A301)∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£¥° ∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡°À
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190139810"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190139910"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A301)∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£¥° π≠»œ∞œ«ÆΩ˝∆√ƒÍΩ∏√Êº£Œ≈°À
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190140010"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190140110"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A301-2)•œ•§•±•¢•Ê•À•√•»∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£±
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190175010"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A301-2)•œ•§•±•¢•Ê•À•√•»∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≤
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190175110"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A301-2)•œ•§•±•¢•Ê•À•√•»∆˛±°∞ÂŒ≈¥…Õ˝Œ¡
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190117310"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A301-3)«æ¬¥√Ê•±•¢•Ê•À•√•»∆˛±°∞ÂŒ≈¥…Õ˝Œ¡
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190120810"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A301-4)æÆª˘∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190149910"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190150010"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A302)ø∑¿∏ª˘∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£±
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190024710"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A302)ø∑¿∏ª˘∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡£≤
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190140410"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A303)¡ÌπÁº˛ª∫¥¸∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡°  Ï¬Œ°¶¬€ª˘°À
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190066710"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A303)¡ÌπÁº˛ª∫¥¸∆√ƒÍΩ∏√Êº£Œ≈ºº¥…Õ˝Œ¡° ø∑¿∏ª˘°À
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190066810"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A303-2)ø∑¿∏ª˘º£Œ≈≤Û…¸ºº∆˛±°∞ÂŒ≈¥…Õ˝Œ¡
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190140510"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *     (A305)∞ÏŒ‡¥∂¿˜æ…¥µº‘∆˛±°∞ÂŒ≈¥…Õ˝Œ¡
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190075510"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190075610"             TO  STUSAN-TOKUTEI-SRYCD(2)
      *       ø¿Æ£≤£∏«Ø≈Ÿ≤˛¿µ§«ø«Œ≈π‘∞Ÿ•≥°º•… —ππ
             MOVE   "190194310"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE   "190194410"             TO  STUSAN-TOKUTEI-SRYCD(4)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
             MOVE    WRK-TOKUTEI-ROOM-NISSU TO  LNK-TOKNYUIN2-NISSU08
           END-IF
           .
       201-TOKUTEI-ROOM-NISSU-EXT.
           EXIT.
      *
      *****************************************************************
      *  °  ø¿Æ£≥£∞«Ø£¥∑Ó£±∆¸∞ πﬂ°À
      *    √œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°Œ¡°¢√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡§Œ
      *    ƒÃªª∆¸øÙºË∆¿
      *    ¢® ø¿Æ£≥£∞«Ø£≥∑Ó£≥£±∆¸§ﬁ§«§ŒªªƒÍ•≥°º•…§‚•´•¶•Û•»§π§Î§≥§»
      *****************************************************************
       201-CHIIKI-HOUKATSU-NISSU-SEC                    SECTION.
      *
           MOVE   ZERO                  TO  WRK-CHIIKI-HOUKATSU-NISSU
           MOVE   ZERO                  TO  FLG-KAIGO
      *
      *    ∫«Ω™§Œ∆˛±°Õ˙ŒÚ§¨≤∏Ó§´§È§Œ∞€∆∞° µﬁ¿≠¡˝∞≠°À§À§Ë§Î§‚§Œ§´
      *    »ΩƒÍÕ—§À∆˛±°Õ˙ŒÚ§Œ∆…§ﬂπ˛§ﬂ
           INITIALIZE                       PTNYUINRRK-REC
           MOVE    LNK-TOKNYUIN2-HOSPNUM
                                        TO  PTNYUINRRK-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID   TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
           MOVE    "key9"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE    "tbl_ptnyuinrrk" TO  MCP-TABLE
               MOVE    "key9"           TO  MCP-PATHNAME
               PERFORM 920-PTNYUINRRK-FETCH-SEC
               IF     FLG-PTNYUINRRK    =   ZERO
                   IF   PTNYUINRRK-KAIGOKBN   =   "2"
                       MOVE   1         TO  FLG-KAIGO
                   END-IF
               END-IF
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"       TO   MCP-TABLE
           MOVE    "key9"                TO   MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           DISPLAY "*******CHIIKI-HOUKATSU-NISSU****************"
           DISPLAY "PTNYUINRRK-RRKNUM = " PTNYUINRRK-RRKNUM
           DISPLAY "PTNYUINRRK-RRKEDANUM = " PTNYUINRRK-RRKEDANUM
           DISPLAY "PTNYUINRRK-KAIGOKBN = " PTNYUINRRK-KAIGOKBN
      *
      *    (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£±
      *    (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£±° ¿∏≥ËŒ≈Õ‹°À
      *    (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£≤
      *    (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£≤° ¿∏≥ËŒ≈Õ‹°À
           INITIALIZE                      ORCSTUSANAREA
           MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
           MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
      *    √œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£±
           MOVE    "190209110"             TO  STUSAN-TOKUTEI-SRYCD(1)
           MOVE    "190209210"             TO  STUSAN-TOKUTEI-SRYCD(2)
           MOVE    "190175310"             TO  STUSAN-TOKUTEI-SRYCD(3)
           MOVE    "190175410"             TO  STUSAN-TOKUTEI-SRYCD(4)
      *    √œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£≤
           MOVE    "190209510"             TO  STUSAN-TOKUTEI-SRYCD(5)
           MOVE    "190209610"             TO  STUSAN-TOKUTEI-SRYCD(6)
           MOVE    "190175710"             TO  STUSAN-TOKUTEI-SRYCD(7)
           MOVE    "190175810"             TO  STUSAN-TOKUTEI-SRYCD(8)
      *
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF      FLG-KAIGO               =   ZERO
               ADD     STUSAN-NISSU09      TO  WRK-CHIIKI-HOUKATSU-NISSU
           ELSE
               ADD     STUSAN-NISSU07      TO  WRK-CHIIKI-HOUKATSU-NISSU
           END-IF
      *
      *    (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£±
      *    (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£±° ¿∏≥ËŒ≈Õ‹°À
      *    (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≤
      *    (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≤° ¿∏≥ËŒ≈Õ‹°À
           INITIALIZE                      ORCSTUSANAREA
           MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
           MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
      *    √œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£±
           MOVE    "190209310"             TO  STUSAN-TOKUTEI-SRYCD(1)
           MOVE    "190209410"             TO  STUSAN-TOKUTEI-SRYCD(2)
           MOVE    "190175510"             TO  STUSAN-TOKUTEI-SRYCD(3)
           MOVE    "190175610"             TO  STUSAN-TOKUTEI-SRYCD(4)
      *    √œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≤
           MOVE    "190209710"             TO  STUSAN-TOKUTEI-SRYCD(5)
           MOVE    "190209810"             TO  STUSAN-TOKUTEI-SRYCD(6)
           MOVE    "190175910"             TO  STUSAN-TOKUTEI-SRYCD(7)
           MOVE    "190176010"             TO  STUSAN-TOKUTEI-SRYCD(8)
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF      FLG-KAIGO               =   ZERO
               ADD     STUSAN-NISSU09      TO  WRK-CHIIKI-HOUKATSU-NISSU
           ELSE
               ADD     STUSAN-NISSU07      TO  WRK-CHIIKI-HOUKATSU-NISSU
           END-IF
      *
      *    (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£±° ∆√ƒÍ√œ∞Ë°À
      *    (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£±° ¿∏≥ËŒ≈Õ‹°À° ∆√ƒÍ√œ∞Ë°À
           INITIALIZE                      ORCSTUSANAREA
           MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
           MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
           MOVE    "190210710"             TO  STUSAN-TOKUTEI-SRYCD(1)
           MOVE    "190210810"             TO  STUSAN-TOKUTEI-SRYCD(2)
           MOVE    "190176110"             TO  STUSAN-TOKUTEI-SRYCD(3)
           MOVE    "190176210"             TO  STUSAN-TOKUTEI-SRYCD(4)
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF      FLG-KAIGO               =   ZERO
               ADD     STUSAN-NISSU09      TO  WRK-CHIIKI-HOUKATSU-NISSU
           ELSE
               ADD     STUSAN-NISSU07      TO  WRK-CHIIKI-HOUKATSU-NISSU
           END-IF
      *
      *    (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£≤° ∆√ƒÍ√œ∞Ë°À
      *    (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£≤° ¿∏≥ËŒ≈Õ‹°À° ∆√ƒÍ√œ∞Ë°À
           INITIALIZE                      ORCSTUSANAREA
           MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
           MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
           MOVE    "190211110"             TO  STUSAN-TOKUTEI-SRYCD(1)
           MOVE    "190211210"             TO  STUSAN-TOKUTEI-SRYCD(2)
           MOVE    "190176510"             TO  STUSAN-TOKUTEI-SRYCD(3)
           MOVE    "190176610"             TO  STUSAN-TOKUTEI-SRYCD(4)
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF      FLG-KAIGO               =   ZERO
               ADD     STUSAN-NISSU09      TO  WRK-CHIIKI-HOUKATSU-NISSU
           ELSE
               ADD     STUSAN-NISSU07      TO  WRK-CHIIKI-HOUKATSU-NISSU
           END-IF
      *
      *    (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£±° ∆√ƒÍ√œ∞Ë°À
      *    (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£±° ¿∏≥ËŒ≈Õ‹°À° ∆√ƒÍ√œ∞Ë°À
           INITIALIZE                      ORCSTUSANAREA
           MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
           MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
           MOVE    "190210910"             TO  STUSAN-TOKUTEI-SRYCD(1)
           MOVE    "190211010"             TO  STUSAN-TOKUTEI-SRYCD(2)
           MOVE    "190176310"             TO  STUSAN-TOKUTEI-SRYCD(3)
           MOVE    "190176410"             TO  STUSAN-TOKUTEI-SRYCD(4)
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF      FLG-KAIGO               =   ZERO
               ADD     STUSAN-NISSU09      TO  WRK-CHIIKI-HOUKATSU-NISSU
           ELSE
               ADD     STUSAN-NISSU07      TO  WRK-CHIIKI-HOUKATSU-NISSU
           END-IF
      *
      *    (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≤° ∆√ƒÍ√œ∞Ë°À
      *    (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≤° ¿∏≥ËŒ≈Õ‹°À° ∆√ƒÍ√œ∞Ë°À
           INITIALIZE                      ORCSTUSANAREA
           MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
           MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
           MOVE    "190211310"             TO  STUSAN-TOKUTEI-SRYCD(1)
           MOVE    "190211410"             TO  STUSAN-TOKUTEI-SRYCD(2)
           MOVE    "190176710"             TO  STUSAN-TOKUTEI-SRYCD(3)
           MOVE    "190176810"             TO  STUSAN-TOKUTEI-SRYCD(4)
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF      FLG-KAIGO               =   ZERO
               ADD     STUSAN-NISSU09      TO  WRK-CHIIKI-HOUKATSU-NISSU
           ELSE
               ADD     STUSAN-NISSU07      TO  WRK-CHIIKI-HOUKATSU-NISSU
           END-IF
      *
      *    (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£≥
      *    (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£≥° ¿∏≥ËŒ≈Õ‹°À
      *    (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≥
      *    (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≥° ¿∏≥ËŒ≈Õ‹°À
      *    (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£¥
      *    (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£¥° ¿∏≥ËŒ≈Õ‹°À
      *    (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£¥
      *    (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£¥° ¿∏≥ËŒ≈Õ‹°À
           INITIALIZE                      ORCSTUSANAREA
           MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
           MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
           MOVE    "190209910"             TO  STUSAN-TOKUTEI-SRYCD(1)
           MOVE    "190210010"             TO  STUSAN-TOKUTEI-SRYCD(2)
           MOVE    "190210110"             TO  STUSAN-TOKUTEI-SRYCD(3)
           MOVE    "190210210"             TO  STUSAN-TOKUTEI-SRYCD(4)
           MOVE    "190210310"             TO  STUSAN-TOKUTEI-SRYCD(5)
           MOVE    "190210410"             TO  STUSAN-TOKUTEI-SRYCD(6)
           MOVE    "190210510"             TO  STUSAN-TOKUTEI-SRYCD(7)
           MOVE    "190210610"             TO  STUSAN-TOKUTEI-SRYCD(8)
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF      FLG-KAIGO               =   ZERO
               ADD     STUSAN-NISSU09      TO  WRK-CHIIKI-HOUKATSU-NISSU
           ELSE
               ADD     STUSAN-NISSU07      TO  WRK-CHIIKI-HOUKATSU-NISSU
           END-IF
      *
      *    (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£≥° ∆√ƒÍ√œ∞Ë°À
      *    (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£≥° ¿∏≥ËŒ≈Õ‹°À° ∆√ƒÍ√œ∞Ë°À
      *    (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≥° ∆√ƒÍ√œ∞Ë°À
      *    (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£≥° ¿∏≥ËŒ≈Õ‹°À° ∆√ƒÍ√œ∞Ë°À
      *    (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£¥° ∆√ƒÍ√œ∞Ë°À
      *    (A308-3)√œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°£¥° ¿∏≥ËŒ≈Õ‹°À° ∆√ƒÍ√œ∞Ë°À
      *    (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£¥° ∆√ƒÍ√œ∞Ë°À
      *    (A308-3)√œ∞Ë Ò≥Á•±•¢∆˛±°∞ÂŒ≈¥…Õ˝Œ¡£¥° ¿∏≥ËŒ≈Õ‹°À° ∆√ƒÍ√œ∞Ë°À
           INITIALIZE                      ORCSTUSANAREA
           MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
           MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
           MOVE    "190211510"             TO  STUSAN-TOKUTEI-SRYCD(1)
           MOVE    "190211610"             TO  STUSAN-TOKUTEI-SRYCD(2)
           MOVE    "190211710"             TO  STUSAN-TOKUTEI-SRYCD(3)
           MOVE    "190211810"             TO  STUSAN-TOKUTEI-SRYCD(4)
           MOVE    "190211910"             TO  STUSAN-TOKUTEI-SRYCD(5)
           MOVE    "190212010"             TO  STUSAN-TOKUTEI-SRYCD(6)
           MOVE    "190212110"             TO  STUSAN-TOKUTEI-SRYCD(7)
           MOVE    "190212210"             TO  STUSAN-TOKUTEI-SRYCD(8)
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF      FLG-KAIGO               =   ZERO
               ADD     STUSAN-NISSU09      TO  WRK-CHIIKI-HOUKATSU-NISSU
           ELSE
               ADD     STUSAN-NISSU07      TO  WRK-CHIIKI-HOUKATSU-NISSU
           END-IF
      *
           ADD     WRK-TOKU-TAISYONISSU    TO  WRK-CHIIKI-HOUKATSU-NISSU
      *
           .
       201-CHIIKI-HOUKATSU-NISSU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ŒÚ∫Ó¿Æ§«≈–œø§∑§ø∆˛±°Õ˙ŒÚ§À§ƒ§§§∆°÷∆√ƒÍ∆˛±°°›ƒÃªª¬–æ›∆¸øÙ°◊§Œ
      *    ∆˛Œœ ¨∆¸øÙ§Ú≤√ªª§π§Î
      *****************************************************************
       202-RRK-TOKUTEI-NISSU-SEC                    SECTION.
      *
           MOVE    ZERO                 TO  WRK-TOKU-TAISYONISSU
           INITIALIZE                       PTNYUINRRK-REC
           MOVE    LNK-TOKNYUIN2-HOSPNUM
                                        TO  PTNYUINRRK-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID   TO  PTNYUINRRK-PTID
           MOVE    LNK-TOKNYUIN2-SHONUM TO  PTNYUINRRK-SHONUM
           MOVE    PTNYUINRRK-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
           MOVE    "key65"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE    "tbl_ptnyuinrrk" TO  MCP-TABLE
               MOVE    "key65"          TO  MCP-PATHNAME
               PERFORM 920-PTNYUINRRK-FETCH-SEC
               PERFORM    UNTIL   FLG-PTNYUINRRK  =   1
      *
                  IF      PTNYUINRRK-TOKU-TAISYONISSU  NOT =  ZERO
                      ADD PTNYUINRRK-TOKU-TAISYONISSU  TO
                          LNK-TOKNYUIN2-NISSU08
                      ADD PTNYUINRRK-TOKU-TAISYONISSU  TO
                          WRK-TOKU-TAISYONISSU
                  END-IF
      *
                  MOVE    "tbl_ptnyuinrrk" TO  MCP-TABLE
                  MOVE    "key65"          TO  MCP-PATHNAME
                  PERFORM 920-PTNYUINRRK-FETCH-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"       TO   MCP-TABLE
           MOVE    "key65"                TO   MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       202-RRK-TOKUTEI-NISSU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥µº‘∏ƒ Ã¿ﬂƒÍ•∆°º•÷•Î∆…π˛ΩËÕ˝
      *****************************************************************
       900-PTCONF-READ-SEC         SECTION.
      *
           MOVE    PTCONF-REC          TO  MCPDATA-REC
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  PTCONF-REC
                   MOVE    ZERO        TO  FLG-PTCONF
               ELSE
                   INITIALIZE              PTCONF-REC
                   MOVE    1           TO  FLG-PTCONF
               END-IF
           ELSE
               INITIALIZE                  PTCONF-REC
               MOVE    1               TO  FLG-PTCONF
           END-IF
      *
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-PTCONF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    •∆°º•÷•Î∏°∫˜ΩËÕ˝
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
      *    £ƒ£¬∆…§ﬂπ˛§ﬂΩËÕ˝
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥µº‘∆˛±°Õ˙ŒÚ•ﬁ•π•ø°º∆…π˛
      *****************************************************************
       920-PTNYUINRRK-FETCH-SEC     SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
               MOVE    ZERO            TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF.
      *
       920-PTNYUINRRK-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    £ƒ£¬•Ø•Ì°º•πΩËÕ˝
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
