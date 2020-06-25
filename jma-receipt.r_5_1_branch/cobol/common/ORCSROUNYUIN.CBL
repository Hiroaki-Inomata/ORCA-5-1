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
       PROGRAM-ID.                 ORCSROUNYUIN.
      *****************************************************************
      *  •∑•π•∆•‡Ãæ        : £œ£“£√£¡
      *  •µ•÷•∑•π•∆•‡Ãæ    : ∆˛±°
      *  •≥•Û•›°º•Õ•Û•»Ãæ  : œ´∫“∆˛±°≤Ò∑◊∫Ó¿Æ•µ•÷
      *  ¥…Õ˝º‘            : 
      *  ∫Ó¿Æ∆¸…’    ∫Ó∂»º‘        µ≠Ω“
      *  03/07/11    NACL°›æÆ∆¶¬Ù  ø∑µ¨∫Ó¿Æ
      *****************************************************************
      *  •◊•Ì•∞•È•‡Ω§¿µÕ˙ŒÚ
      * Maj/Min/Rev  Ω§¿µº‘       ∆¸…’      ∆‚Õ∆
      *  01.00.01    NACL-ÃÁœ∆    04/02/27  Ã§º¬ª‹∏∫ªª∑Ó≈”√Ê —ππ¬–±˛
      *  01.00.02    NACL-æÆ∆¶¬Ù  04/03/11  Õ≠æ≤ø«Œ≈ΩÍ£±∑≤∆˛±°¥À‹Œ¡£±
      *                                     ° ∞Âª’«€√÷≈¨πÁ°À§Œ¬–±˛
      *  01.00.03    NACL-ÃÁœ∆    04/03/17  ¬æ∞ÂŒ≈µ°¥ÿºıø«§Œ»ΩƒÍÕ—•≥°º•…
      *                                     ° ∆˛±°¥À‹Œ¡≤√ªª∂Ë ¨°Àƒ…≤√
      *  01.00.04    NACL-ÃÁœ∆    04/03/29  ÍÛ·Ï¥µº‘¥…Õ˝≤√ªª§Œ¬–±˛
      *  01.00.05    NACL-æÆ∆¶¬Ù  04/09/10  œ´∫“æËøÙ§Ú•µ•÷•Î°º•¡•Û§´§È§Œ
      *                                     »ΩƒÍ§À —ππ
      *  01.00.05    NACL-æÆ∆¶¬Ù  05/09/16  ∞ ≤º§Œ∆√ƒÍ∆˛±°Œ¡§Œ≥∞«Ò∑◊ªª§ÚΩ§¿µ
      *                                     ø«Œ≈ΩÍœ∑øÕ∞ÂŒ≈¥…Õ˝Œ¡
      *                                     œ∑øÕ¿≠«ß√Œæ…º¿¥µº£Œ≈…¬≈Ô∆˛±°Œ¡£±
      *                                     œ∑øÕ¿≠«ß√Œæ…º¿¥µº£Œ≈…¬≈Ô∆˛±°Œ¡£≤
      *  01.00.06    NACL-ÃÁœ∆    06/01/20  …∏∑Á¬–±˛
      *  01.00.07    NACL-æÆ∆¶¬Ù  06/03/22  ∆√ƒÍ∆˛±°Œ¡ƒ…≤√ ¨° «æ¬¥√Ê•±•¢•Ê•À°À
      *                                     ≥∞«Ò√÷§≠¥π§®ΩËÕ˝
      *  02.09.01    NACL-ÃÁœ∆    06/04/13  £≤£∞£∞£∂«Ø£¥∑Ó≤˛¿µ¬–±˛
      *  02.09.02    NACL-ÃÁœ∆    06/05/17  Œ¨æŒ ‘Ω∏(190123250≈˘)
      *  02.09.03    NACL-ÃÁœ∆    06/05/23  ≥∞«Òª˛§Œ∆˛±°¥À‹Œ¡ºË∆¿Ω§¿µ
      *  02.09.04    NACL-ÃÁœ∆    06/06/02  ÍÛ·Ï¥µº‘¥…Õ˝≤√ªª ‘Ω∏¬–±˛
      *  02.09.05    NACL-ÃÁœ∆    06/06/05   £øÙ∞Âª’µ⁄§”¥«∏Ó«€√÷≈¨πÁ≤√ªª§»
      *                                     ¥«∏Ó«€√÷≈¨πÁ≤√ªª§Œ ‘Ω∏¬–±˛
      *  02.09.06    NACL-ÃÁœ∆    06/06/23  «ß√Œµ°«Ωæ„≥≤≤√ªª§Œ ‘Ω∏¬–±˛
      *  03.01.01    NACL-ÃÁœ∆    06/06/30  Œ≈Õ‹…¬≈Ô§Œ¬æ∞ÂŒ≈µ°¥ÿºıø«¬–±˛
      *  03.01.02    NACL-æÆ∆¶¬Ù  06.07.04  ∆˛±°Œ¡§À¥ﬁ§·§Î≤√ªª§¨•≥•·•Û•»
      *                                     §»∆±ª˛∆˛Œœ§µ§Ï§øæÏπÁ§Œ¬–±˛
      *  03.04.01    NACL-ÃÁœ∆    06/12/01  º´«Â¿’∑Ú ›Ω‡µÚ¬–±˛
      *  03.04.02    NACL-ÃÁœ∆    07/03/12   Ò≥Áø«Œ≈¬–±˛
      *  03.05.00    NACL-ÃÁœ∆    07/05/08  •∞•Î°º•◊ø«Œ≈¬–±˛
      *  04.01.01    NACL-ÃÁœ∆    07/10/25  ∏¯Ã≥∫“≥≤¬–±˛
      *  04.01.02    NACL-ÃÁœ∆    07/12/19  ∆˛±°Œ¡Ã§ªªƒÍ∆¸¬–±˛
      *  04.01.03    NACL-ÃÁœ∆    08/02/04  …∏∑Á≥∞«Ò∑◊ªªΩ§¿µ
      *  04.02.01    NACL-ÃÁœ∆    08/02/29   ø¿Æ£≤£∞«Ø£¥∑Ó≤˛¿µ¬–±˛
      *  04.02.02    NACL-æÆ∆¶¬Ù  08/04/30  Õ≠æ≤ø«Œ≈ΩÍ§Œ≤√ªª§Úœ´∫“∆˛±°≤√ªª§À
      *                                     ¥ﬁ§·§Î° ∆˛±°≤Ò∑◊§Ë§Í°À
      *                                     °¶ÃÎ¥÷∂€µﬁ¬Œ¿©≥Œ ›≤√ªª
      *                                     °¶∞Âª’«€√÷≈¨πÁ≤√ªª
      *                                     °¶¥«∏Ó«€√÷≤√ªª£±
      *                                     °¶¥«∏Ó«€√÷≤√ªª£≤
      *                                     °¶ÃÎ¥÷¥«∏Ó«€√÷≤√ªª£±
      *                                     °¶ÃÎ¥÷¥«∏Ó«€√÷≤√ªª£≤
      *                                     ∆√ƒÍµ°«Ω…¬±°°›¿∫ø¿…¬≈Ô§Œ≤√ªª§Úœ´∫“∆˛±°≤√ªª§À
      *                                     ¥ﬁ§·§Î° ø«Œ≈≤Ò∑◊§Ë§Í°À
      *                                     °¶Ω≈≈Ÿ«ß√Œæ…≤√ªª° ∆√ƒÍµ°«Ω…¬±°¿∫ø¿…¬≈Ô°À
      *                                     ≤Û…¸¥¸•Í•œ•”•Í•∆°º•∑•Á•Û…¬≈Ô∆˛±°Œ¡£±§Œ≤√ªª§Ú
      *                                     œ´∫“∆˛±°≤√ªª§À¥ﬁ§·§Î° ∆˛±°≤Ò∑◊§Ë§Í°À
      *                                     °¶Ω≈æ…¥µº‘≤Û…¸…¬≈Ô≤√ªª
      *  04.02.03    NACL-ÃÁœ∆    08/09/24  ∆˛±°Œ¨æŒ ‘Ω∏ƒ…≤√
      *  04.02.04    NACL-ÃÁœ∆    08/10/07  ∆˛±°¥À‹Œ¡Õ≠∏˙¥¸¥÷•¡•ß•√•Ø
      *  04.04.01    NACL-ÃÁœ∆    09/09/04  æÆ∆˛¥…Œ¨æŒΩ§¿µ
      *  04.04.02    NACL-ÃÁœ∆    10/05/07  ∆˛±°Œ¡∫ﬁ≈¿•¡•ß•√•Ø
      *  04.04.03    NACL-ÃÁœ∆    10/06/01  ¬æ∞ÂŒ≈µ°¥ÿºıø«¬–±˛
      *  04.04.04    NACL-ÃÁœ∆    10/08/09  ¬æ∞Âºıø«§Œ•¡•ß•√•Ø
      *
      *  04.05.01    NACL-ÃÁœ∆    10/01/18  º£∏≥¬–±˛
      *  04.05.02    NACL-ÃÁœ∆    11/08/16  œ´∫“…¬∞·¬ﬂÕøŒ¡¬–±˛
      *  04.05.03    NACL-ÃÁœ∆    12/03/13  ≤˛¿µ¬–±˛° ≥∞«Ò∂Ë ¨£∂°§£∑§Œ¬–±˛°À
      *  04.05.04    NACL-ÃÁœ∆    12/04/09  ≤˛¿µ¬–±˛° ∆˛±°Œ¨æŒ§Œ ∏ª˙øÙ≥»ƒ•°À
      *  04.05.05    NACL-ÃÁœ∆    12/09/18  ≤˛¿µ¬–±˛° ≥∞«Ò∂Ë ¨£∏°§£π§Œ¬–±˛°À
      *
      *  04.06.01    NACL-ÃÁœ∆    13/02/18  œ´∫“•Ï•ª≈≈¬–±˛
      *                                     ° ø∑ø«Œ≈π‘∞Ÿ•≥°º•…¬–±˛°À
      *
      *  04.07.01    NACL-ÃÁœ∆    11/11/22  ∆±∆¸∫∆∆˛±°¬–±˛
      *  04.07.02    NACL-ÃÁœ∆    13/10/02  º´«Â¿’° ¬Ëª∞º‘π‘∞Ÿ°À¬–±˛
      *  04.07.03    NACL-ÃÁœ∆    14/03/14   ø¿Æ£≤£∂«Ø£¥∑Ó≤˛¿µ¬–±˛
      *  04.07.04    NACL-ÃÁœ∆    14/04/07   ø¿Æ£≤£∂«Ø£¥∑Ó≤˛¿µ¬–±˛
      *                                     ° œ´∫“•ﬁ•π•ø§À√÷§≠¥π§®°À
      *  04.07.05    NACL-ÃÁœ∆    14/04/08   ø¿Æ£≤£∂«Ø£¥∑Ó≤˛¿µ¬–±˛
      *  04.07.06    NACL-ÃÁœ∆    14/06/17  ±…Õ‹¥…Õ˝¬Œ¿©∏∫ªªµ¨ƒÍ≥∫≈ˆ¬–±˛
      *  04.07.07    NACL-ÃÁœ∆    14/07/31  ±…Õ‹¥…Õ˝¬Œ¿©∏∫ªªµ¨ƒÍ≥∫≈ˆ¬–±˛
      *
      *  04.08.01    NACL-ÃÁœ∆    15/11/09  ¬æ∞Âºıø«•¡•ß•√•Øƒ…≤√
      *  04.08.02    NACL-ÃÁœ∆    16/03/08   ø¿Æ£≤£∏«Ø£¥∑Ó≤˛¿µ¬–±˛
      *  04.08.03    NACL-ÃÁœ∆    17/12/13  ∆˛±°Œ¡≤√ªª§Œœ´∫“•ﬁ•π•ø√÷§≠¥π§®Ω§¿µ
      *  04.08.04    NACL-ÃÁœ∆    18/02/01  ¬Ëª∞º‘π‘∞Ÿ§Œ¡™ƒÍ∆˛±°Œ¡¬–±˛
      *  04.08.05    NACL-ÃÁœ∆    18/03/01   ø¿Æ£≥£∞«Ø£¥∑Ó≤˛¿µ¬–±˛
      *  04.08.06    NACL-ÃÁœ∆    18/03/27   ø¿Æ£≥£∞«Ø£¥∑Ó≤˛¿µ¬–±˛
      *
      *  05.00.01    NACL-ÃÁœ∆    20/03/09  Œ·œ¬£≤«Ø£¥∑Ó≤˛¿µ
      *                                     ∆˛±°¥À‹Œ¡≤√ªª∂Ë ¨ ‘Ω∏
      *  05.00.02    NACL-ÃÁœ∆    20/03/16  ¬æ∞ÂŒ≈µ°¥ÿºıø«°‹…∏∑Á§Œ∑◊ªªΩ§¿µ
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
           03  FLG-NYUINACCT       PIC 9(01).
           03  FLG-NYUINACT        PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-RJN             PIC 9(01).
           03  FLG-ROUSAI          PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-RYAKU           PIC 9(01).
           03  FLG-NYURYAKUSHO     PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-KSN             PIC 9(01).
           03  FLG-KENPOJYUNKYO    PIC 9(01).
           03  FLG-SENTEICDCHG     PIC 9(01).
      *    ≈∫ª˙ŒŒ∞Ë
       01  IDX-AREA.
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  ID1                 PIC 9(04).
           03  KSN-IDX             PIC 9(04).
           03  GENSAN-IDX          PIC 9(04).
           03  GENSAN-IDX2         PIC 9(04).
           03  IDV                 PIC 9(02).
           03  SRYACT-IDX          PIC 9(04).
      *
      *    •≥•Û•π•ø•Û•»√Õ
       01  CONSTANT-AREA.
      *    ∆˛±°ø«Œ≈∑◊≤ËÃ§º¬ª‹∏∫ªª°  ø¿Æ£±£∏«Ø£≥∑Ó§ﬁ§«°À
           03  CONS-KEIKAKU-GENSAN1        PIC X(09) VALUE "190080470".
           03  CONS-KEIKAKU-GENSAN2        PIC X(09) VALUE "190799670".
      *    ±°∆‚¥∂¿˜À…ªﬂ¬–∫ˆÃ§º¬ª‹∏∫ªª°  ø¿Æ£±£∏«Ø£≥∑Ó§ﬁ§«°À
           03  CONS-INNAI-GENSAN1          PIC X(09) VALUE "190080570".
           03  CONS-INNAI-GENSAN2          PIC X(09) VALUE "190799770".
      *    ∞ÂŒ≈∞¬¡¥¥…Õ˝¬Œ¿©Ã§¿∞»˜∏∫ªª°  ø¿Æ£±£∏«Ø£≥∑Ó§ﬁ§«°À
           03  CONS-ANZEN-GENSAN1          PIC X(09) VALUE "190111690".
           03  CONS-ANZEN-GENSAN2          PIC X(09) VALUE "190809090".
      *    ÍÛ·Ï¬–∫ˆÃ§º¬ª‹∏∫ªª°  ø¿Æ£±£∏«Ø£≥∑Ó§ﬁ§«°À
           03  CONS-JYOKU-GENSAN1          PIC X(09) VALUE "190111790".
           03  CONS-JYOKU-GENSAN2          PIC X(09) VALUE "190809190".
      *    ∆¸æÔ¿∏≥Ëæ„≥≤≤√ªª°  ø¿Æ£±£∏«Ø£∂∑Ó§ﬁ§«°À
           03  CONS-NICHIJOU-KSN1          PIC X(09) VALUE "190109170".
           03  CONS-NICHIJOU-KSN2          PIC X(09) VALUE "190807570".
      *    «ß√Œæ…≤√ªª°  ø¿Æ£±£∏«Ø£∂∑Ó§ﬁ§«°À
           03  CONS-CHIHOU-KSN1            PIC X(09) VALUE "190109270".
           03  CONS-CHIHOU-KSN2            PIC X(09) VALUE "190808670".
      *    ÍÛ·Ï¥µº‘¥…Õ˝≤√ªª
           03  CONS-JYOKU-KSN1             PIC X(09) VALUE "190116990".
           03  CONS-JYOKU-KSN2             PIC X(09) VALUE "190813390".
      *     £øÙ∞Âª’µ⁄§”¥«∏Ó«€√÷≈¨πÁ≤√ªª°  ø¿Æ£≤£∞«Ø£≥∑Ó§ﬁ§«°À
           03  CONS-KANGO-KSN1             PIC X(09) VALUE "190117070".
      *    ¥«∏Ó«€√÷≈¨πÁ≤√ªª°  ø¿Æ£≤£∞«Ø£≥∑Ó§ﬁ§«°À
           03  CONS-KANGO-KSN2             PIC X(09) VALUE "190109670".
      *    «ß√Œµ°«Ωæ„≥≤≤√ªª° Œ≈Õ‹…¬≈Ô°À°  ø¿Æ£≤£∞«Ø£≥∑Ó§ﬁ§«°À
           03  CONS-NINSYO-KSN1            PIC X(09) VALUE "190121970".
      *    «ß√Œµ°«Ωæ„≥≤≤√ªª° Õ≠æ≤ø«Œ≈ΩÍ°À°  ø¿Æ£≤£∞«Ø£≥∑Ó§ﬁ§«°À
           03  CONS-NINSYO-KSN2            PIC X(09) VALUE "190122670".
      *
      *-- ø¿Æ£≤£∞«Ø£¥∑Ó∞ πﬂ§À¬–ΩË§Úƒ…≤√§∑§ø≤√ªª------------------------
      *    ÃÎ¥÷∂€µﬁ¬Œ¿©≥Œ ›≤√ªª
           03  CONS-200804-KSN1            PIC X(09) VALUE "190126470".
      *    ∞Âª’«€√÷≈¨πÁ≤√ªª° £≤£≤«Ø£¥∑Ó∞ πﬂ°¢∞Âª’«€√÷≤√ªª£≤°À
           03  CONS-200804-KSN2            PIC X(09) VALUE "190117070".
      *    ¥«∏Ó«€√÷≤√ªª£±° £≤£≤«Ø£¥∑Ó∞ πﬂ°¢¥«∏Ó«€√÷≤√ªª£≤°À
           03  CONS-200804-KSN3            PIC X(09) VALUE "190126570".
      *    ¥«∏Ó«€√÷≤√ªª£≤° £≤£≤«Ø£¥∑Ó∞ πﬂ°¢¥«∏Ó«€√÷≤√ªª£±°À
           03  CONS-200804-KSN4            PIC X(09) VALUE "190109670".
      *    ÃÎ¥÷¥«∏Ó«€√÷≤√ªª£±° £≤£≤«Ø£¥∑Ó∞ πﬂ°¢ÃÎ¥÷¥«∏Ó«€√÷≤√ªª£≤°À
           03  CONS-200804-KSN5            PIC X(09) VALUE "190126670".
      *    ÃÎ¥÷¥«∏Ó«€√÷≤√ªª£≤° £≤£≤«Ø£¥∑Ó∞ πﬂ°¢ÃÎ¥÷¥«∏Ó«€√÷≤√ªª£±°À
           03  CONS-200804-KSN6            PIC X(09) VALUE "190126770".
      *    Ω≈≈Ÿ«ß√Œæ…≤√ªª° ∆√ƒÍµ°«Ω…¬±°¿∫ø¿…¬≈Ô°À
           03  CONS-200804-KSN7            PIC X(09) VALUE "190119210".
      *    ÍÛ·Ï…æ≤¡º¬ª‹≤√ªª° Œ≈Õ‹…¬≈Ô°À
           03  CONS-200804-KSN8            PIC X(09) VALUE "190121970".
      *    ÍÛ·Ï…æ≤¡º¬ª‹≤√ªª° Õ≠æ≤ø«Œ≈ΩÍ°À
           03  CONS-200804-KSN9            PIC X(09) VALUE "190122670".
      *    Ω≈æ…¥µº‘≤Û…¸…¬≈Ô≤√ªª
           03  CONS-200804-KSN10           PIC X(09) VALUE "190129870".
      *
      *-- ø¿Æ£≤£≤«Ø£¥∑Ó∞ πﬂ§À¬–ΩË§Úƒ…≤√§∑§ø≤√ªª------------------------
      *    ∞Âª’«€√÷≤√ªª£±
           03  CONS-201004-KSN12           PIC X(09) VALUE "190135570".
      *    ∞Ï»Ã…¬≈Ô¥«∏Ó…¨Õ◊≈Ÿ…æ≤¡≤√ªª
           03  CONS-201004-KSN13           PIC X(09) VALUE "190131570".
      *    Ω≈≈Ÿ«ß√Œæ…≤√ªª° ¿∫ø¿…¬≈Ô∆˛±°¥À‹Œ¡°À
           03  CONS-201004-KSN15           PIC X(09) VALUE "190118570".
      *    ∞Ï»Ã…¬≈Ô¥«∏Ó…¨Õ◊≈Ÿ…æ≤¡≤√ªª° ∆√ƒÍµ°«Ω…¬±°°À
           03  CONS-201004-KSN16           PIC X(09) VALUE "190134870".
      *    ∞Ï»Ã…¬≈Ô¥«∏Ó…¨Õ◊≈Ÿ…æ≤¡≤√ªª° ¿ÏÃÁ…¬±°°À
           03  CONS-201004-KSN17           PIC X(09) VALUE "190134970".
      *
      *-- ø¿Æ£≤£¥«Ø£¥∑Ó∞ πﬂ§À¬–ΩË§Úƒ…≤√§∑§ø≤√ªª------------------------
      *    ¥«∏Ó…¨Õ◊≈Ÿ≤√ªª£±° ∞Ï»Ã…¬≈Ô∆˛±°¥À‹Œ¡°À
           03  CONS-201204-KSN4            PIC X(09) VALUE "190143070".
      *    ¥«∏Ó…¨Õ◊≈Ÿ≤√ªª£≤° ∞Ï»Ã…¬≈Ô∆˛±°¥À‹Œ¡°À
           03  CONS-201204-KSN5            PIC X(09) VALUE "190143170".
      *    ¥«∏Ó…¨Õ◊≈Ÿ≤√ªª£±° ∆√ƒÍµ°«Ω…¬±°∆˛±°¥À‹Œ¡°À
           03  CONS-201204-KSN8            PIC X(09) VALUE "190143970".
      *    ¥«∏Ó…¨Õ◊≈Ÿ≤√ªª£≤° ∆√ƒÍµ°«Ω…¬±°∆˛±°¥À‹Œ¡°À
           03  CONS-201204-KSN9            PIC X(09) VALUE "190144070".
      *    ¥«∏Ó…¨Õ◊≈Ÿ≤√ªª£±° ¿ÏÃÁ…¬±°∆˛±°¥À‹Œ¡°À
           03  CONS-201204-KSN10           PIC X(09) VALUE "190144570".
      *    ¥«∏Ó…¨Õ◊≈Ÿ≤√ªª£≤° ¿ÏÃÁ…¬±°∆˛±°¥À‹Œ¡°À
           03  CONS-201204-KSN11           PIC X(09) VALUE "190144670".
      *
      *-- ø¿Æ£≤£µ«Ø£∑∑Ó∞ πﬂ§À¬–ΩË§Úƒ…≤√§∑§ø≤√ªª------------------------
      *    ∞Ï»Ã…¬≈Ô¥«∏Ó…¨Õ◊≈Ÿ…æ≤¡≤√ªª° ∞Ï»Ã…¬≈Ô∆˛±°¥À‹Œ¡°À° œ´∫“Õ—°À
           03  CONS-201307-KSN1            PIC X(09) VALUE "101900100".
      *    Ω≈≈Ÿ«ß√Œæ…≤√ªª° ¿∫ø¿…¬≈Ô∆˛±°¥À‹Œ¡°À° œ´∫“Õ—°À
           03  CONS-201307-KSN2            PIC X(09) VALUE "101900110".
      *    ∞Ï»Ã…¬≈Ô¥«∏Ó…¨Õ◊≈Ÿ…æ≤¡≤√ªª° ¿ÏÃÁ…¬±°∆˛±°¥À‹Œ¡°À° œ´∫“Õ—°À
           03  CONS-201307-KSN3            PIC X(09) VALUE "101900120".
      *
      *-- ø¿Æ£≤£∂«Ø£¥∑Ó∞ πﬂ§À¬–ΩË§Úƒ…≤√§∑§ø≤√ªª------------------------
      *    À˝¿≠∞›ª˝∆©¿œ¥…Õ˝≤√ªª° Œ≈Õ‹…¬≈Ô∆˛±°¥À‹Œ¡°À
           03  CONS-201404-KSN1            PIC X(09) VALUE "190167970".
      *    ∫ﬂ¬…¸µ¢µ°«Ω∂Ø≤Ω≤√ªª° Œ≈Õ‹…¬≈Ô∆˛±°¥À‹Œ¡°À
           03  CONS-201404-KSN2            PIC X(09) VALUE "190168070".
      *    ¿∫ø¿ ›∑Ú °ª„ªŒ«€√÷≤√ªª° ¿∫ø¿…¬≈Ô∆˛±°¥À‹Œ¡°À
           03  CONS-201404-KSN3            PIC X(09) VALUE "190168970".
      *    ¥«∏Ó ‰Ωı«€√÷≤√ªª£±° Õ≠æ≤ø«Œ≈ΩÍ∆˛±°¥À‹Œ¡°À
           03  CONS-201404-KSN4            PIC X(09) VALUE "190170570".
      *    ¥«∏Ó ‰Ωı«€√÷≤√ªª£≤° Õ≠æ≤ø«Œ≈ΩÍ∆˛±°¥À‹Œ¡°À
           03  CONS-201404-KSN5            PIC X(09) VALUE "190170670".
      *    ±…Õ‹¥…Õ˝º¬ª‹≤√ªª° Õ≠æ≤ø«Œ≈ΩÍ∆˛±°¥À‹Œ¡°À
           03  CONS-201404-KSN6            PIC X(09) VALUE "190170770".
      *    ±…Õ‹¥…Õ˝º¬ª‹≤√ªª° Õ≠æ≤ø«Œ≈ΩÍŒ≈Õ‹…¬æ≤∆˛±°¥À‹Œ¡°À
           03  CONS-201404-KSN7            PIC X(09) VALUE "190171770".
      *
      *    À˝¿≠∞›ª˝∆©¿œ¥…Õ˝≤√ªª° Œ≈Õ‹…¬≈Ô∆˛±°¥À‹Œ¡°À° œ´∫“Õ—°À
           03  CONS-201404-KSN8            PIC X(09) VALUE "101900130".
      *    ∫ﬂ¬…¸µ¢µ°«Ω∂Ø≤Ω≤√ªª° Œ≈Õ‹…¬≈Ô∆˛±°¥À‹Œ¡°À° œ´∫“Õ—°À
           03  CONS-201404-KSN9            PIC X(09) VALUE "101900140".
      *
      *-- ø¿Æ£≤£∂«Ø£∑∑Ó∞ πﬂ§À¬–ΩË§Úƒ…≤√§∑§ø≤√ªª------------------------
      *    ±…Õ‹¥…Õ˝¬Œ¿©∏∫ªªµ¨ƒÍ≥∫≈ˆ° ∆˛±°¥À‹Œ¡°À
           03  CONS-201407-KSN1            PIC X(09) VALUE "190182590".
      *    ±…Õ‹¥…Õ˝¬Œ¿©∏∫ªªµ¨ƒÍ≥∫≈ˆ° ∆√ƒÍ∆˛±°Œ¡µ⁄§”√ª¥¸¬⁄∫ﬂºÍΩ—≈˘¥À‹Œ¡£≤°À
           03  CONS-201407-KSN2            PIC X(09) VALUE "190182690".
      *
      *    ±…Õ‹¥…Õ˝¬Œ¿©∏∫ªªµ¨ƒÍ≥∫≈ˆ° ∆˛±°¥À‹Œ¡°À° œ´∫“Õ—°À
           03  CONS-201407-KSN3            PIC X(09) VALUE "101900150".
      *    ±…Õ‹¥…Õ˝¬Œ¿©∏∫ªªµ¨ƒÍ≥∫≈ˆ° ∆√ƒÍ∆˛±°Œ¡√ª¥¸¬⁄∫ﬂºÍΩ—¥£≤°À° œ´∫“Õ—°À
           03  CONS-201407-KSN4            PIC X(09) VALUE "101900160".
      *
      *-- ø¿Æ£≤£∏«Ø£¥∑Ó∞ πﬂ§À¬–ΩË§Úƒ…≤√§∑§ø≤√ªª------------------------
      *    ¥«∏Ó…¨Õ◊≈Ÿ≤√ªª£±° ∞Ï»Ã…¬≈Ô∆˛±°¥À‹Œ¡°À
           03  CONS-201604-KSN1            PIC X(09) VALUE "190183070".
      *    ¥«∏Ó…¨Õ◊≈Ÿ≤√ªª£≤° ∞Ï»Ã…¬≈Ô∆˛±°¥À‹Œ¡°À
           03  CONS-201604-KSN2            PIC X(09) VALUE "190183170".
      *    ¥«∏Ó…¨Õ◊≈Ÿ≤√ªª£≥° ∞Ï»Ã…¬≈Ô∆˛±°¥À‹Œ¡°À
           03  CONS-201604-KSN3            PIC X(09) VALUE "190183270".
      *    ¥«∏Ó…¨Õ◊≈Ÿ≤√ªª£±° ∆√ƒÍµ°«Ω…¬±°∆˛±°¥À‹Œ¡°À
           03  CONS-201604-KSN4            PIC X(09) VALUE "190188870".
      *    ¥«∏Ó…¨Õ◊≈Ÿ≤√ªª£≤° ∆√ƒÍµ°«Ω…¬±°∆˛±°¥À‹Œ¡°À
           03  CONS-201604-KSN5            PIC X(09) VALUE "190188970".
      *    ¥«∏Ó…¨Õ◊≈Ÿ≤√ªª£≥° ∆√ƒÍµ°«Ω…¬±°∆˛±°¥À‹Œ¡°À
           03  CONS-201604-KSN6            PIC X(09) VALUE "190189070".
      *    ¥«∏Ó…¨Õ◊≈Ÿ≤√ªª£±° ¿ÏÃÁ…¬±°∆˛±°¥À‹Œ¡°À
           03  CONS-201604-KSN7            PIC X(09) VALUE "190189170".
      *    ¥«∏Ó…¨Õ◊≈Ÿ≤√ªª£≤° ¿ÏÃÁ…¬±°∆˛±°¥À‹Œ¡°À
           03  CONS-201604-KSN8            PIC X(09) VALUE "190189270".
      *    ¥«∏Ó…¨Õ◊≈Ÿ≤√ªª£≥° ¿ÏÃÁ…¬±°∆˛±°¥À‹Œ¡°À
           03  CONS-201604-KSN9            PIC X(09) VALUE "190189370".
      *    Õ≠æ≤ø«Œ≈ΩÍ∫ﬂ¬…¸µ¢µ°«Ω∂Ø≤Ω≤√ªª° Õ≠æ≤ø«Œ≈ΩÍ∆˛±°¥À‹Œ¡°À
           03  CONS-201604-KSN10           PIC X(09) VALUE "190190270".
      *    Õ≠æ≤ø«Œ≈ΩÍŒ≈Õ‹…¬æ≤∫ﬂ¬…¸µ¢µ°«Ω∂Ø≤Ω≤√ªª
           03  CONS-201604-KSN11           PIC X(09) VALUE "190190370".
      *
      *-- ø¿Æ£≥£∞«Ø£¥∑Ó∞ πﬂ§À¬–ΩË§Úƒ…≤√§∑§ø≤√ªª------------------------
      *    ÍÛ·Ï¬–∫ˆ≤√ªª£≤° Œ≈Õ‹…¬≈Ô°À
           03  CONS-201804-KSN1            PIC X(09) VALUE "190201670".
      *    ÍÛ·Ï¬–∫ˆ≤√ªª£≤° Õ≠æ≤ø«Œ≈ΩÍŒ≈Õ‹…¬æ≤∆˛±°¥À‹Œ¡°À
           03  CONS-201804-KSN4            PIC X(09) VALUE "190206170".
      *    ÃÎ¥÷¥«∏Ó≤√ªª
           03  CONS-201804-KSN5            PIC X(09) VALUE "190204570".
      *    ÃÎ¥÷¥«∏Ó≤√ªª° œ´∫“Õ—°À
           03  CONS-201804-KSN6            PIC X(09) VALUE "101900170".
      *----------------------------------------------------------------
      *
      *    œ´∫“º£Œ≈∑◊≤Ë≤√ªª
           03  CONS-ROSAICHIRYO            PIC X(09) VALUE "101910050".
      *    …¬∞·¬ﬂÕøŒ¡
           03  CONS-BYOUI                  PIC X(09) VALUE "101910060".
      *    ∆√ Ã¥∆ªÎŒ¡
           03  CONS-TOKUKANSI              PIC X(09) VALUE "101920010".
      *
      *    ∞Ïª˛ŒŒ∞Ë
       01  WRK-AREA.
      *
           03  WRK-GAIHAKU-SRYCD           PIC X(09).
           03  WRK-MEISAISU                PIC 9(03).
           03  WRK-RYAKU-SRYCD             PIC X(09).
           03  WRK-RYAKUSHO                PIC X(20).
           03  WRK-SRYKBN                  PIC X(02).
           03  WRK-KSNKBN                  PIC 9(03).
           03  WRK-CDKBN-KBNNUM            PIC 9(03).
           03  WRK-CHUKSNCD-XXX            PIC X(04).
           03  WRK-NYUKHNKBN-XXX           PIC 9(03).
           03  WRK-ZAISKBKBN               PIC X(01).
           03  WRK-SANTEIED-DDX            PIC X(02).
           03  WRK-SANTEIED-DD9            REDEFINES  WRK-SANTEIED-DDX
                                           PIC 9(02).
      *
           03  WRK-SYS5000-COUNT           PIC 9(02).
      *
           03  WRK-GENSAN2-MIJISI          PIC 9(01).
           03  WRK-GENSAN3-MIJISI          PIC 9(01).
           03  WRK-GENSAN4-MIJISI          PIC 9(01).
      *
           03  WRK-SHOKIKSN-TAIHI          PIC X(09).
           03  WRK-HYOKETU-TAIHI           PIC X(09).
           03  WRK-HYOKETU-TEN             PIC 9(09)V99.
           03  WRK-ZAITEN-HYOKETU          PIC 9(08).
           03  WRK-ZAITEN-HYOKETU2         PIC 9(08)V9(5).
      *
           03  WRK-NYUINRYO-ERR            PIC 9(01).
           03  WRK-NYUINRYO-ERR2           PIC 9(01).
           03  WRK-TAIJYUSIN-ERR           PIC 9(01).
           03  WRK-GENSAN-ERR              PIC 9(01).
           03  WRK-GENSAN-ERR2             PIC 9(01).
      *
           03  WRK-KHNTEN                  PIC 9(08). 
           03  WRK-KSNTEN                  PIC S9(08).
           03  WRK-HYOTEN                  PIC 9(09)V99.
           03  WRK-HIKAKU-ZAITEN           PIC 9(08).
      *
           03  WRK-SENTEI-SRYCD1           PIC X(09).
           03  WRK-SENTEI-SRYCD2           PIC X(09).
      *
           03  WRK-HOSPSBT                 PIC 9(01).
      *
           03  WRK-TOKUREI-SRYCD           PIC X(09).
           03  WRK-TOKUREI-SRYCD2          PIC X(09).
           03  WRK-TOKUREI-FLG             PIC 9(01).
      *
           03  WRK-TOKUREI-RYAKUSHO        PIC X(20).
      *
           03  WRK-KEISAN                  PIC 9(08).
      *
      *    œ´∫“∆˛±°≤Ò∑◊•Ô°º•Ø
           03  WRK-ROUSAI-TABLE.
               05  WRK-ROUSAI-TBL          OCCURS  31.
                   07  WRK-NYUINKHN        PIC X(09).
                   07  WRK-SHOKIKSN        PIC X(09).
                   07  WRK-HYOKETU         PIC X(09).
                   07  WRK-GAIHAKU         PIC X(09).
                   07  WRK-GAIHAKU2        PIC X(09).
                   07  WRK-NYUKHNKSNKBN    PIC 9(03).
                   07  WRK-CHUKSNCD        PIC X(04).
                   07  WRK-NYUKHNKBN       PIC 9(03).
                   07  WRK-DAY-SENTEI      PIC 9(04).
                   07  WRK-SENTEI-KHN      PIC X(09).
                   07  WRK-SENTEI-KHNGEN   PIC X(09).
                   07  WRK-KANGOGEN        PIC X(09).
      *
      *    ∆˛±°Œ¡≤√ªª¬‡»ÚŒŒ∞Ë
           03  WRK-NYUKSN-TABLE.
               05  WRK-NYUKSN-TBL          OCCURS  20.
                   07  WRK-NYUKSN          PIC X(09).
                   07  WRK-NYUKSN-ZAITEN   PIC 9(08).
                   07  WRK-NYUKSN-DAY      PIC 9(03)  OCCURS 31.
      *
      *    Ã§º¬ª‹∏∫ªª¬‡»ÚÕ—•’•È•∞•∆°º•÷•Î
           03  WRK-GENSAN-TABLE.
               05  WRK-GENSAN-INF          OCCURS  31.
                   07  WRK-GENSAN-INNAI    PIC X(01).
                   07  WRK-GENSAN-IRYO     PIC X(01).
                   07  WRK-GENSAN-JYOKU    PIC X(01).
      *
      *    ±°∆‚¥∂¿˜À…ªﬂ¬–∫ˆÃ§º¬ª‹∏∫ªª•Ô°º•Ø
           03  WRK-GENSAN2                 PIC X(09).
      *    ∞ÂŒ≈∞¬¡¥¥…Õ˝¬Œ¿©Ã§¿∞»˜∏∫ªª•Ô°º•Ø
           03  WRK-GENSAN3                 PIC X(09).
      *    ÍÛ·Ï¬–∫ˆÃ§º¬ª‹∏∫ªª•Ô°º•Ø
           03  WRK-GENSAN4                 PIC X(09).
      *
      *    ∆˛±°ø«Œ≈∑◊≤ËÃ§º¬ª‹∏∫ªª•Ô°º•Ø
           03  WRK-KEIKAKU-GENSAN1         PIC 9(03) OCCURS 31.
           03  WRK-KEIKAKU-GENSAN2         PIC 9(03) OCCURS 31.
      *    ±°∆‚¥∂¿˜À…ªﬂ¬–∫ˆÃ§º¬ª‹∏∫ªª•Ô°º•Ø
           03  WRK-INNAI-GENSAN1           PIC 9(03) OCCURS 31.
           03  WRK-INNAI-GENSAN2           PIC 9(03) OCCURS 31.
      *    ∞ÂŒ≈∞¬¡¥¥…Õ˝¬Œ¿©Ã§¿∞»˜∏∫ªª•Ô°º•Ø
           03  WRK-ANZEN-GENSAN1           PIC 9(03) OCCURS 31.
           03  WRK-ANZEN-GENSAN2           PIC 9(03) OCCURS 31.
      *    ÍÛ·Ï¬–∫ˆÃ§º¬ª‹∏∫ªª•Ô°º•Ø
           03  WRK-JYOKU-GENSAN1           PIC 9(03) OCCURS 31.
           03  WRK-JYOKU-GENSAN2           PIC 9(03) OCCURS 31.
      *
      *    ∆¸æÔ¿∏≥Ëæ„≥≤≤√ªª¬‡»Ú
           03  WRK-NICHIJOU-KSN1           PIC 9(03) OCCURS 31.
           03  WRK-NICHIJOU-KSN2           PIC 9(03) OCCURS 31.
      *
      *    √‘ Ú≤√ªª¬‡»Ú
           03  WRK-CHIHOU-KSN1             PIC 9(03) OCCURS 31.
           03  WRK-CHIHOU-KSN2             PIC 9(03) OCCURS 31.
      *
      *    ÍÛ·Ï¥µº‘¥…Õ˝≤√ªª
           03  WRK-JYOKU-KSN1              PIC 9(03) OCCURS 31.
           03  WRK-JYOKU-KSN2              PIC 9(03) OCCURS 31.
      *
      *     £øÙ∞Âª’µ⁄§”¥«∏Ó«€√÷≈¨πÁ≤√ªª
           03  WRK-KANGO-KSN1              PIC 9(03) OCCURS 31.
      *    ¥«∏Ó«€√÷≈¨πÁ≤√ªª
           03  WRK-KANGO-KSN2              PIC 9(03) OCCURS 31.
      *
      *    «ß√Œµ°«Ωæ„≥≤≤√ªª° Œ≈Õ‹…¬≈Ô°À
           03  WRK-NINSYO-KSN1             PIC 9(03) OCCURS 31.
      *    «ß√Œµ°«Ωæ„≥≤≤√ªª° Õ≠æ≤ø«Œ≈ΩÍ°À
           03  WRK-NINSYO-KSN2             PIC 9(03) OCCURS 31.
      *
      *-- ø¿Æ£≤£∞«Ø£¥∑Ó∞ πﬂ§À¬–ΩË§Úƒ…≤√§∑§ø≤√ªª§Œ¬‡»ÚŒŒ∞Ë--------------
           03  WRK-200804-KSN1             PIC 9(03) OCCURS 31.
           03  WRK-200804-KSN2             PIC 9(03) OCCURS 31.
           03  WRK-200804-KSN3             PIC 9(03) OCCURS 31.
           03  WRK-200804-KSN4             PIC 9(03) OCCURS 31.
           03  WRK-200804-KSN5             PIC 9(03) OCCURS 31.
           03  WRK-200804-KSN6             PIC 9(03) OCCURS 31.
           03  WRK-200804-KSN7             PIC 9(03) OCCURS 31.
           03  WRK-200804-KSN8             PIC 9(03) OCCURS 31.
           03  WRK-200804-KSN9             PIC 9(03) OCCURS 31.
           03  WRK-200804-KSN10            PIC 9(03) OCCURS 31.
      *-- ø¿Æ£≤£≤«Ø£¥∑Ó∞ πﬂ§Àø∑¿ﬂ§µ§Ï§ø≤√ªª§Œ¬‡»ÚŒŒ∞Ë--------------
           03  WRK-201004-KSN11            PIC 9(03) OCCURS 31.
           03  WRK-201004-KSN12            PIC 9(03) OCCURS 31.
           03  WRK-201004-KSN13            PIC 9(03) OCCURS 31.
           03  WRK-201004-KSN14            PIC 9(03) OCCURS 31.
           03  WRK-201004-KSN15            PIC 9(03) OCCURS 31.
           03  WRK-201004-KSN16            PIC 9(03) OCCURS 31.
           03  WRK-201004-KSN17            PIC 9(03) OCCURS 31.
           03  WRK-201004-KSN18            PIC 9(03) OCCURS 31.
           03  WRK-201004-KSN19            PIC 9(03) OCCURS 31.
      *-- ø¿Æ£≤£¥«Ø£¥∑Ó∞ πﬂ§Àø∑¿ﬂ§µ§Ï§ø≤√ªª§Œ¬‡»ÚŒŒ∞Ë--------------
           03  WRK-201204-KSN4             PIC 9(03) OCCURS 31.
           03  WRK-201204-KSN5             PIC 9(03) OCCURS 31.
           03  WRK-201204-KSN8             PIC 9(03) OCCURS 31.
           03  WRK-201204-KSN9             PIC 9(03) OCCURS 31.
           03  WRK-201204-KSN10            PIC 9(03) OCCURS 31.
           03  WRK-201204-KSN11            PIC 9(03) OCCURS 31.
      *-- ø¿Æ£≤£∂«Ø£¥∑Ó∞ πﬂ§Àø∑¿ﬂ§µ§Ï§ø≤√ªª§Œ¬‡»ÚŒŒ∞Ë--------------
           03  WRK-201404-KSN1             PIC 9(03) OCCURS 31.
           03  WRK-201404-KSN2             PIC 9(03) OCCURS 31.
           03  WRK-201404-KSN3             PIC 9(03) OCCURS 31.
           03  WRK-201404-KSN4             PIC 9(03) OCCURS 31.
           03  WRK-201404-KSN5             PIC 9(03) OCCURS 31.
           03  WRK-201404-KSN6             PIC 9(03) OCCURS 31.
           03  WRK-201404-KSN7             PIC 9(03) OCCURS 31.
      *-- ø¿Æ£≤£∂«Ø£∑∑Ó∞ πﬂ§Àø∑¿ﬂ§µ§Ï§ø≤√ªª§Œ¬‡»ÚŒŒ∞Ë--------------
           03  WRK-201407-KSN1             PIC 9(03) OCCURS 31.
           03  WRK-201407-KSN2             PIC 9(03) OCCURS 31.
      *-- ø¿Æ£≤£∏«Ø£¥∑Ó∞ πﬂ§Àø∑¿ﬂ§µ§Ï§ø≤√ªª§Œ¬‡»ÚŒŒ∞Ë--------------
           03  WRK-201604-KSN1             PIC 9(03) OCCURS 31.
           03  WRK-201604-KSN2             PIC 9(03) OCCURS 31.
           03  WRK-201604-KSN3             PIC 9(03) OCCURS 31.
           03  WRK-201604-KSN4             PIC 9(03) OCCURS 31.
           03  WRK-201604-KSN5             PIC 9(03) OCCURS 31.
           03  WRK-201604-KSN6             PIC 9(03) OCCURS 31.
           03  WRK-201604-KSN7             PIC 9(03) OCCURS 31.
           03  WRK-201604-KSN8             PIC 9(03) OCCURS 31.
           03  WRK-201604-KSN9             PIC 9(03) OCCURS 31.
           03  WRK-201604-KSN10            PIC 9(03) OCCURS 31.
           03  WRK-201604-KSN11            PIC 9(03) OCCURS 31.
      *-- ø¿Æ£≥£∞«Ø£¥∑Ó∞ πﬂ§Àø∑¿ﬂ§µ§Ï§ø≤√ªª§Œ¬‡»ÚŒŒ∞Ë--------------
           03  WRK-201804-KSN1             PIC 9(03) OCCURS 31.
           03  WRK-201804-KSN4             PIC 9(03) OCCURS 31.
           03  WRK-201804-KSN5             PIC 9(03) OCCURS 31.
      *----------------------------------------------------------------
      *
      *    ø«Œ≈•≥°º•…∑◊Ω∏∑◊Õ—
           03  WRK-SRYCD-X                 PIC X(09).
           03  WRK-SRYCD-9    REDEFINES    WRK-SRYCD-X
                                           PIC 9(09).
      *    ∆˛±°Œ¡Ã§ªªƒÍ∂Ë ¨
           03  WRK-NYUINDAY-TBL.
               05  WRK-NYUINDAY-KBN        PIC X(01) OCCURS  31.
      *
      *****************************************************************
      *    •’•°•§•Î•Ï•§•¢•¶•»
      *****************************************************************
      *
      *    •∑•π•∆•‡¥…Õ˝•ﬁ•π•ø
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1001.INC".
      *
           COPY    "CPSK5000.INC".
      *
      *    ∆˛±°ø«Œ≈≤Ò∑◊•ﬁ•π•ø°›
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    ∆˛±°ø«Œ≈π‘∞Ÿ•ﬁ•π•ø°º
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    ø«Œ≈≤Ò∑◊
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    ø«Œ≈π‘∞Ÿ
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    ≈¿øÙ•ﬁ•π•ø°›
           COPY    "CPTENSU.INC".
      *
      *    ¥µº‘•ﬁ•π•ø°›
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *     ›∏±¡»§ﬂπÁ§Ô§ª
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    ∆˛±°Œ¨æŒ
       01  NYURYAKUSHO-REC.
           COPY    "CPNYUINRYAKUSHO.INC".
      *
      *    ¡™ƒÍ∆˛±°Œ¡
       01  SENTEICDCHG-REC.
           COPY    "CPSENTEICDCHG.INC".
      *
      *    £ƒ£¬∏°∫˜
           COPY    "MCPDATA.INC".
      ***  COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    •µ•÷•◊•ÌÕ—°°ŒŒ∞Ë
      *****************************************************************
      *
      *   ∆¸…’ —¥π•µ•÷
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *     ›∏±ªªƒÍÕ—«ØŒ°¶≥‰πÁ∑◊ªª•µ•÷
           COPY    "CPORCSAGECHK.INC".
      *
      *    ƒÃªª∆¸øÙºË∆¿•µ•÷° œ´∫“ ›∏±Õ—°À
           COPY    "CPORCSRSITUSAN.INC".
      *
      *    ∆˛±°Œ¡Ã§ªªƒÍ∆¸ºË∆¿•µ•÷
           COPY    "CPORCSNYUINDAY.INC".
      *
      *    ±…Õ‹¥…Õ˝∑–≤·¡º√÷∆˛±°Œ¡•≥°º•…ºË∆¿•µ•÷
           COPY    "CPORCSEIYOUKKS.INC".
      *
      *    ÃÎ¥÷¥«∏Ó¬Œ¿©∆√ƒÍ∆¸∏∫ªªºË∆¿•µ•÷
           COPY    "CPORCSKANGOGEN.INC".
      *
      *    ∆˛±°≈¿øÙ•ﬁ•π•ø…’≤√æ Û•µ•÷
           COPY    "CPORCSNTENSU.INC".
      *
      *****************************************************************
      *    œ¢ÕÌ°°ŒŒ∞Ë
      *****************************************************************
        LINKAGE                     SECTION.
           COPY    "MCPAREA".
      *    œ´∫“∆˛±°≤Ò∑◊∫Ó¿ÆÕ—•—•È•·•ø
           COPY    "CPORCSROUNYUIN.INC".
      *    •π•—ŒŒ∞Ë
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           ORCSROUNYUIN-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    ºÁ°°°°ΩËÕ˝
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  LNK-SROUNYUIN-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-SROUNYUIN-RC
      *
      *    •—•È•·°º•ø•¡•ß•√•Ø
           PERFORM 100-PRM-CHK-SEC
           IF    ( LNK-SROUNYUIN-RC       NOT =   ZERO )
               GO  TO  000-PROC-EXT
           END-IF
      *
      *    ºÁΩËÕ˝
           IF      LNK-SROUNYUIN-RC      =    ZERO
               PERFORM 200-MAIN-SEC
           END-IF
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    •—•È•·°º•ø•¡•ß•√•Ø
      *****************************************************************
       100-PRM-CHK-SEC                     SECTION.
      *
           IF    ( LNK-SROUNYUIN-SANTEIYM    =   SPACE )
               MOVE    1               TO  LNK-SROUNYUIN-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    ¥µº‘•ﬁ•π•ø°º∏°∫˜
           INITIALIZE                      PTINF-REC
           MOVE    LNK-SROUNYUIN-HOSPNUM   TO  PTINF-HOSPNUM
           MOVE    LNK-SROUNYUIN-PTID      TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           NOT =   ZERO
               MOVE    2               TO  LNK-SROUNYUIN-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    œ´∫“∆˛±°√Ê§Œ•¡•ß•√•Ø° ∆˛±°≤Ò∑◊•ﬁ•π•ø°º∏°∫˜°À
           INITIALIZE                      NYUINACCT-REC
           MOVE    LNK-SROUNYUIN-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-SROUNYUIN-PTID      TO  NACCT-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO  NACCT-SRYYM
           MOVE    "5"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           NOT =   ZERO
               MOVE    3               TO  LNK-SROUNYUIN-RC
           ELSE
               MOVE   "tbl_nyuinacct"      TO  MCP-TABLE
               MOVE   "key17"              TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT          NOT =   ZERO
                    MOVE    3               TO  LNK-SROUNYUIN-RC
               ELSE
      *            œ´∫“ ›∏±§Œ»ΩƒÍ
                   MOVE      ZERO              TO  FLG-ROUSAI
                   PERFORM   VARYING   IDX1   FROM  1   BY  1
                             UNTIL    IDX1   >   31
                        IF   LNK-SROUNYUIN-HKNCOMBI  =
                                               NACCT-DAY(IDX1)
                             MOVE    1         TO  FLG-ROUSAI
                             MOVE   "1"        TO  
                                          LNK-SROUNYUIN-NYUIN-FLG(IDX1)
                        END-IF
                   END-PERFORM
      **>          IF   FLG-ROUSAI             =   ZERO
      **>               MOVE    3              TO  LNK-SROUNYUIN-RC
      **>          END-IF
               END-IF
           END-IF
      *
      *    ∆˛±°≤Ò∑◊•Ø•Ì°º•∫
           MOVE     "tbl_nyuinacct"      TO  MCP-TABLE
           MOVE     "key17"              TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           IF   LNK-SROUNYUIN-RC    NOT =   ZERO
                   GO  TO  100-PRM-CHK-EXT
           END-IF
      *
           PERFORM   110-RJN-TAISYO-CHK-SEC
      *
           IF   LNK-SROUNYUIN-RC    NOT =   ZERO
                   GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    ∆˛±°Œ¡Ã§ªªƒÍ∆¸ºË∆¿
           INITIALIZE                          ORCSNYUINDAY-AREA
           MOVE    LNK-SROUNYUIN-HOSPNUM   TO  LNK-NYUINDAY-HOSPNUM
           MOVE    LNK-SROUNYUIN-PTID      TO  LNK-NYUINDAY-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO  LNK-NYUINDAY-SANTEIYM
           CALL    "ORCSNYUINDAY"          USING   ORCSNYUINDAY-AREA
                                                   SPA-AREA
           IF      LNK-NYUINDAY-RC     =   ZERO
              PERFORM    VARYING   IDX1    FROM   1   BY   1
                          UNTIL    IDX1     >    31
                IF   LNK-SROUNYUIN-NYUIN-FLG(IDX1) = "1"
                   MOVE  LNK-NYUINDAY-KBN(IDX1)
                                           TO  WRK-NYUINDAY-KBN(IDX1)
                END-IF
              END-PERFORM
           END-IF
      *
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    œ∑øÕ¬–æ›¥µº‘•¡•ß•√•Ø≈˘
      *****************************************************************
       110-RJN-TAISYO-CHK-SEC         SECTION.
      *
           MOVE    ZERO                 TO   FLG-RJN
      *
           MOVE    ZERO                 TO   FLG-HKNCOMBI
           INITIALIZE                        HKNCOMBI-REC
           MOVE    LNK-SROUNYUIN-HOSPNUM
                                        TO   COMB-HOSPNUM
           MOVE    LNK-SROUNYUIN-PTID   TO   COMB-PTID
           MOVE    LNK-SROUNYUIN-HKNCOMBI
                                        TO   COMB-HKNCOMBINUM
           MOVE    "tbl_hkncombi"       TO   MCP-TABLE
           MOVE    "key"                TO   MCP-PATHNAME
           PERFORM 910-HKNCOMBI-SELECT-SEC
           IF      FLG-HKNCOMBI         =    ZERO
               MOVE     "tbl_hkncombi"  TO   MCP-TABLE
               MOVE     "key"           TO   MCP-PATHNAME
               PERFORM   910-HKNCOMBI-READ-SEC
           END-IF
           MOVE     "tbl_hkncombi"      TO   MCP-TABLE
           MOVE     "key"               TO   MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *    ∆˛±°¥µº‘§Œ«ØŒ∑◊ªª
           INITIALIZE                       ORCSAGECHKAREA
           MOVE    LNK-SROUNYUIN-HOSPNUM
                                        TO  AGECHK-HOSPNUM
           MOVE    LNK-SROUNYUIN-PTID   TO  AGECHK-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM
                                        TO  AGECHK-SRYYMD(1:6)
           MOVE    "01"                 TO  AGECHK-SRYYMD(7:2)
           MOVE    PTINF-BIRTHDAY       TO  AGECHK-BIRTHDAY
           MOVE    LNK-SROUNYUIN-HKNCOMBI
                                        TO  AGECHK-HKNCOMBINUM
           MOVE    "1"                  TO  AGECHK-NYUGAIKBN
           CALL    "ORCSAGECHK"         USING
                                            ORCSAGECHKAREA
                                            SPA-AREA
           IF      AGECHK-RC            =   ZERO
               CONTINUE
           ELSE
               INITIALIZE    ORCSAGECHKAREA
           END-IF
      *
      *    œ∑øÕ¬–æ›¥µº‘»ΩƒÍ
           IF    ((FLG-HKNCOMBI        =   ZERO      ) AND
                  ((COMB-KOH1HKNNUM    =   "027"    ) OR
                   (COMB-HKNNUM        =   "039"    )))
           OR    ( AGECHK-NENREI      >=    75        )
           OR    ((AGECHK-NENREI      >=    70       ) AND
                  (PTINF-BIRTHDAY     <=   "19320930"))
               MOVE    1               TO  FLG-RJN
           END-IF
      *
      *    ∑Ú ›Ω‡µÚ»ΩƒÍ
           MOVE      ZERO       TO    FLG-KENPOJYUNKYO
           IF      AGECHK-HKNKBN =   4  OR  5  OR  7
              MOVE      1       TO    FLG-KENPOJYUNKYO
           END-IF
      *
           IF      FLG-HKNCOMBI    NOT =   ZERO
                  MOVE    4     TO    LNK-SROUNYUIN-RC
           END-IF
      *
           .
       110-RJN-TAISYO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ºÁºË∆¿
      *****************************************************************
       200-MAIN-SEC            SECTION.
      *
           IF      LNK-SROUNYUIN-SANTEIYM   <=   "200803"
               PERFORM 210-ROUNYUIN-SET1-SEC
           ELSE
               IF      LNK-SROUNYUIN-SANTEIYM   <=   "201003"
                   PERFORM 210-ROUNYUIN-SET2-SEC
               ELSE
                 IF    LNK-SROUNYUIN-SANTEIYM   <=   "201203"
                   PERFORM 210-ROUNYUIN-SET3-SEC
                 ELSE
                   PERFORM 210-ROUNYUIN-SET4-SEC
                 END-IF
               END-IF
           END-IF
      *
           IF  LNK-SROUNYUIN-RC  =  ZERO
             IF   WRK-TAIJYUSIN-ERR =  1
                MOVE     9     TO    LNK-SROUNYUIN-RC
             END-IF
           END-IF
           IF  LNK-SROUNYUIN-RC  =  ZERO
             IF   WRK-GENSAN-ERR    =  1
                MOVE    10     TO    LNK-SROUNYUIN-RC
             END-IF
           END-IF
           IF  LNK-SROUNYUIN-RC  =  ZERO
             IF   WRK-GENSAN-ERR2   =  1
                MOVE    11     TO    LNK-SROUNYUIN-RC
             END-IF
           END-IF
           IF  LNK-SROUNYUIN-RC  =  ZERO
             IF   WRK-NYUINRYO-ERR  =  1
                MOVE     7     TO    LNK-SROUNYUIN-RC
             END-IF
           END-IF
           IF  LNK-SROUNYUIN-RC  =  ZERO
             IF   WRK-NYUINRYO-ERR2 =  1
                MOVE     8     TO    LNK-SROUNYUIN-RC
             END-IF
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    œ´∫“∆˛±°•µ•÷§Œ ÷µ—√Õ•ª•√•»°  ø¿Æ£≤£∞«Ø£≥∑Ó§ﬁ§«°À
      *****************************************************************
       210-ROUNYUIN-SET1-SEC            SECTION.
      *
      *œ´∫“æËøÙ§Œ£≤Ωµ¥÷∞ ∆‚§Œ∫«Ω™∆¸§ÚºË∆¿
           INITIALIZE                      ORCSRSITUSANAREA
           MOVE    LNK-SROUNYUIN-HOSPNUM
                                       TO  RSITUSAN-HOSPNUM
           MOVE    LNK-SROUNYUIN-PTID
                                       TO  RSITUSAN-PTID
           MOVE    LNK-SROUNYUIN-HKNCOMBI
                                       TO  RSITUSAN-HKNCOMBI
           MOVE    LNK-SROUNYUIN-SANTEIYM
                                       TO  RSITUSAN-SRYYM
           CALL    "ORCSRSITUSAN"      USING
                                           ORCSRSITUSANAREA
                                           SPA-AREA
           IF    ( RSITUSAN-RC          =   ZERO )
               CONTINUE
           ELSE
               INITIALIZE  ORCSRSITUSANAREA
           END-IF
           PERFORM   VARYING   IDX1   FROM 1  BY  1
                     UNTIL     IDX1   >    31
               MOVE  RSITUSAN-DAY(IDX1) TO LNK-SROUNYUIN-JOUSU-KBN(IDX1)
           END-PERFORM
      *œ´∫“∆˛±°Œ¡§ŒºË∆¿
           MOVE    ZERO                    TO  FLG-OK
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-SROUNYUIN-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-SROUNYUIN-PTID      TO  NACCT-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO  NACCT-SRYYM
           MOVE    "1"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key17"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                   MOVE      ZERO     TO      FLG-RYAKU
                                              WRK-KSNKBN
                                              WRK-CDKBN-KBNNUM
                   MOVE      SPACE    TO      WRK-CHUKSNCD-XXX
                   MOVE      ZERO     TO      WRK-NYUKHNKBN-XXX
                   MOVE      SPACE    TO      WRK-RYAKUSHO
                                              WRK-SRYKBN
                                              WRK-SHOKIKSN-TAIHI
                                              WRK-HYOKETU-TAIHI
                   IF    NACCT-SRYCDTOTAL     =    99999916
                       CONTINUE
                   ELSE
      *                ≥∫≈ˆ∆˛±°π‘∞Ÿ•∆°º•÷•Î§Œ∆…§ﬂπ˛§ﬂ
                       PERFORM    210-NYUINACT-GET-SEC
      *                ∆˛±°¥À‹Œ¡§»ΩÈ¥¸≤√ªª° ƒπ¥¸∏∫ªª°À§Œ¬‡»Ú
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF    (LNK-SROUNYUIN-NYUIN-FLG(IDX1) = "1")
                           AND   (NACCT-DAY(IDX1)    NOT =  ZERO) 
                               MOVE    NSRY-SRYCD(1)   TO  
                                                   WRK-NYUINKHN(IDX1)
      *
                               IF   FLG-RYAKU   =   ZERO
      *
                                  IF   NSRY-SRYCD(1)  NOT =   SPACE
                                      INITIALIZE    TNS-TENSU-REC
                                      MOVE NSRY-SRYCD(1)
                                                TO  TNS-SRYCD
                                      MOVE LNK-SROUNYUIN-HOSPNUM
                                                TO  TNS-HOSPNUM
                                      MOVE LNK-SROUNYUIN-SANTEIYM
                                                TO  TNS-YUKOSTYMD(1:6)
                                      MOVE "01" TO  TNS-YUKOSTYMD(7:2)
                                      MOVE LNK-SROUNYUIN-SANTEIYM
                                                TO  TNS-YUKOEDYMD(1:6)
                                      MOVE "01" TO  TNS-YUKOEDYMD(7:2)
                                      PERFORM 910-TENSU-READ-SEC
                                      IF  FLG-TENSU  NOT = ZERO
                                        MOVE  1 TO  WRK-NYUINRYO-ERR
                                      END-IF
                                  END-IF
      *
                                  PERFORM  VARYING  IDX3  FROM  2  BY  1
                                            UNTIL   IDX3   >    3
                                    IF   NSRY-SRYCD(IDX3)  NOT =   SPACE
                                      INITIALIZE    TNS-TENSU-REC
                                      MOVE NSRY-SRYCD(IDX3)
                                                TO  TNS-SRYCD
                                      MOVE LNK-SROUNYUIN-HOSPNUM
                                                TO  TNS-HOSPNUM
                                      MOVE LNK-SROUNYUIN-SANTEIYM
                                                TO  TNS-YUKOSTYMD(1:6)
                                      MOVE "01" TO  TNS-YUKOSTYMD(7:2)
                                      MOVE LNK-SROUNYUIN-SANTEIYM
                                                TO  TNS-YUKOEDYMD(1:6)
                                      MOVE "01" TO  TNS-YUKOEDYMD(7:2)
                                      PERFORM 910-TENSU-READ-SEC
                                      EVALUATE  TNS-TENSIKIBETU
                                         WHEN     3
                                         WHEN     8
                                             MOVE   NSRY-SRYCD(IDX3)  TO
                                                      WRK-SHOKIKSN-TAIHI
                                         WHEN     6
                                             MOVE   NSRY-SRYCD(IDX3)  TO
                                                      WRK-HYOKETU-TAIHI
                                      END-EVALUATE
                                    END-IF
                                  END-PERFORM
      *
                                  MOVE    NACCT-SRYKBN
                                                TO  WRK-SRYKBN
      *                           ≥∞«Ò•≥°º•… ‘Ω∏Õ—§À∆˛±°¥À‹Œ¡≤√ªª∂Ë ¨
      *                           §Œ¬‡»Ú
                                  PERFORM   210-GAIHAKU-MAE-SEC
      *
                                  MOVE   NSRY-SRYCD(1)
                                                TO  WRK-RYAKU-SRYCD
                                  MOVE   NACCT-ZAISKBKBN
                                                TO  WRK-ZAISKBKBN
                                  PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                                  MOVE    1     TO    FLG-RYAKU
                               END-IF
      *
                               IF    WRK-SHOKIKSN-TAIHI  NOT = SPACE
                                  MOVE  WRK-SHOKIKSN-TAIHI
                                               TO  WRK-SHOKIKSN(IDX1)
                               END-IF
                               IF    WRK-HYOKETU-TAIHI   NOT = SPACE
                                  MOVE  WRK-HYOKETU-TAIHI
                                               TO  WRK-HYOKETU(IDX1)
                               END-IF
      *
                               MOVE  WRK-SRYKBN
                                         TO  LNK-SROUNYUIN-SRYKBN(IDX1)
                               MOVE  WRK-KSNKBN
                                         TO  WRK-NYUKHNKSNKBN(IDX1)
                               MOVE  WRK-CHUKSNCD-XXX
                                         TO  WRK-CHUKSNCD(IDX1)
                               MOVE  WRK-NYUKHNKBN-XXX
                                         TO  WRK-NYUKHNKBN(IDX1)
      *
                               IF    WRK-RYAKUSHO   NOT =  SPACE
                                  MOVE  WRK-RYAKUSHO
                                      TO  LNK-SROUNYUIN-RYAKUSYO(IDX1)
                               END-IF
      *
                               MOVE    1    TO   FLG-OK
                           END-IF
                       END-PERFORM
                   END-IF
                   MOVE   "tbl_nyuinacct"   TO  MCP-TABLE
                   MOVE   "key17"           TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
      ***      IF   FLG-OK     =   ZERO
      ***          MOVE     6      TO      LNK-SROUNYUIN-RC
      ***      END-IF
           ELSE
               MOVE     6      TO      LNK-SROUNYUIN-RC
           END-IF
      *    ∆˛±°≤Ò∑◊•Ø•Ì°º•∫
           MOVE     "tbl_nyuinacct"   TO  MCP-TABLE
           MOVE     "key17"           TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           IF   LNK-SROUNYUIN-RC    NOT =   ZERO
                   GO   TO    200-MAIN-EXT
           END-IF
      *
      *œ´∫“∆˛±°§Œœ´∫“æËøÙ° ¥¸¥÷≤√ªª°À§À¥ﬁ§·§Î√Ì≤√ªª§ŒºË∆¿
           PERFORM    210-CHUKSN-GET-SEC
      *
      *œ´∫“∆˛±°ª˛§Œ≥∞«ÒºË∆¿
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-SROUNYUIN-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-SROUNYUIN-PTID      TO  NACCT-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO  NACCT-SRYYM
           MOVE    "2"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key17"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
      *            ≥∞«Ò∂Ë ¨§Œ¬‡»Ú
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                              UNTIL     IDX1   >   31
      ****             IF    (WRK-NYUINKHN(IDX1) NOT = SPACE) 
                       IF     NACCT-DAY(IDX1)    NOT =  ZERO
                              PERFORM     210-GAIHAKU-SET-SEC
                       END-IF
                   END-PERFORM
                   MOVE   "tbl_nyuinacct"  TO  MCP-TABLE
                   MOVE   "key17"          TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    ∆˛±°≤Ò∑◊•Ø•Ì°º•∫
           MOVE     "tbl_nyuinacct"        TO  MCP-TABLE
           MOVE     "key17"                TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *∑Ó∆‚§À £øÙ§Œ∆˛±°¥À‹æ Û° £µ£∞£∞£∞°À§¨§ §§§´∏°∫˜
           PERFORM   210-SYS-5000-CHK-SEC
      *
      *∞ÂŒ≈µ°¥ÿ∏∫ªªæ Û§ŒºË∆¿° •∑•π•∆•‡¥…Õ˝•∆°º•÷•Î§Ë§Í°À
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    LNK-SROUNYUIN-HOSPNUM
                                       TO  SYS-HOSPNUM
           MOVE    LNK-SROUNYUIN-SANTEIYM
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    LNK-SROUNYUIN-SANTEIYM
                                       TO  SYS-EDYUKYMD(1:6)
           MOVE    "01"                TO  SYS-EDYUKYMD(7:2)
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               PERFORM  VARYING   GENSAN-IDX  FROM  1  BY  1
                        UNTIL     GENSAN-IDX   >   4
                    IF    SYS-5000-GENKBN(GENSAN-IDX)   =   "2"
                        EVALUATE  GENSAN-IDX
                            WHEN  2
                                MOVE  "190080570"  TO  WRK-GENSAN2
      **                          IF   FLG-RJN    =    ZERO
      **                              MOVE  "190080570"  TO  WRK-GENSAN2
      **                          ELSE
      **                              MOVE  "190799770"  TO  WRK-GENSAN2
      **                          END-IF
                                PERFORM VARYING GENSAN-IDX2 FROM 1 BY 1
                                    UNTIL     GENSAN-IDX2   >   31
                                  MOVE   "1"  TO
                                          WRK-GENSAN-INNAI(GENSAN-IDX2)
                                END-PERFORM
                            WHEN  3
                                MOVE  "190111690"  TO  WRK-GENSAN3
      **                          IF   FLG-RJN    =    ZERO
      **                              MOVE  "190111690"  TO  WRK-GENSAN3
      **                          ELSE
      **                              MOVE  "190809090"  TO  WRK-GENSAN3
      **                          END-IF
                                PERFORM VARYING GENSAN-IDX2 FROM 1 BY 1
                                    UNTIL     GENSAN-IDX2   >   31
                                  MOVE   "1"  TO
                                          WRK-GENSAN-IRYO(GENSAN-IDX2)
                                END-PERFORM
                            WHEN  4
                                MOVE  "190111790"  TO  WRK-GENSAN4
      **                          IF   FLG-RJN    =    ZERO
      **                              MOVE  "190111790"  TO  WRK-GENSAN4
      **                          ELSE
      **                              MOVE  "190809190"  TO  WRK-GENSAN4
      **                          END-IF
                                PERFORM VARYING GENSAN-IDX2 FROM 1 BY 1
                                    UNTIL     GENSAN-IDX2   >   31
                                  MOVE   "1"  TO
                                          WRK-GENSAN-JYOKU(GENSAN-IDX2)
                                END-PERFORM
                        END-EVALUATE
                    END-IF
               END-PERFORM
           END-IF
      *    •∑•π•∆•‡¥…Õ˝•Ø•Ì°º•∫
           MOVE     "tbl_syskanri"      TO  MCP-TABLE
           MOVE     "key10"             TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           IF        WRK-SYS5000-COUNT    =     ZERO
                 CONTINUE
           ELSE
                 PERFORM  210-SYS-5000-GET-SEC
           END-IF
      *
      *
      *œ´∫“∆˛±°ª˛§Œ∆˛±°Œ¡≤√ªªºË∆¿° ∆˛±°≤Ò∑◊§Ë§Í°À
           MOVE    ZERO                    TO  KSN-IDX
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-SROUNYUIN-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-SROUNYUIN-PTID      TO  NACCT-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO  NACCT-SRYYM
           MOVE    "6"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key17"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                   MOVE      ZERO     TO      FLG-RYAKU
                   MOVE      ZERO     TO      FLG-KSN
      *            ∆˛±°Œ¡≤√ªª§Œ¬‡»Ú
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                              UNTIL     IDX1   >   31
                       IF    ((WRK-NYUINKHN(IDX1) NOT = SPACE) OR
                              (WRK-NYUINDAY-KBN(IDX1) =  "1" ))
                       AND   ((LNK-SROUNYUIN-GAIHAKU(IDX1)
                                                      = ZERO ) OR
                              (LNK-SROUNYUIN-GAIHAKU(IDX1)
                                                     >=   4  ))
                       AND   ( NACCT-DAY(IDX1)    NOT =  ZERO )
      *
                         IF  (NACCT-SRYCDTOTAL(6:9) = CONS-KANGO-KSN1
                                                   OR CONS-KANGO-KSN2)
                         AND (LNK-SROUNYUIN-SANTEIYM  >= "200604"    )
                          IF    (WRK-NYUINKHN(IDX1)      NOT = SPACE)
                          AND   (LNK-SROUNYUIN-GAIHAKU(IDX1) =  ZERO)
                           EVALUATE   NACCT-SRYCDTOTAL(6:9)
                             WHEN  CONS-KANGO-KSN1
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-KANGO-KSN1(IDX1)
                             WHEN  CONS-KANGO-KSN2
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-KANGO-KSN2(IDX1)
                           END-EVALUATE
      *
                           IF   FLG-RYAKU   =   ZERO
                              MOVE   NACCT-SRYCDTOTAL(6:9)
                                            TO  WRK-RYAKU-SRYCD
                              MOVE   NACCT-ZAISKBKBN
                                            TO  WRK-ZAISKBKBN
                              PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                              MOVE    1     TO    FLG-RYAKU
                           END-IF
                          END-IF
                         ELSE
                           IF   FLG-KSN   =  ZERO
                               ADD     1    TO    KSN-IDX
                               MOVE    1    TO    FLG-KSN
                           END-IF
                           MOVE   NACCT-SRYCDTOTAL(6:9)
                                        TO  WRK-NYUKSN(KSN-IDX)
                           MOVE   NACCT-ZAITEN  
                                        TO  WRK-NYUKSN-ZAITEN(KSN-IDX)
                           MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-NYUKSN-DAY(KSN-IDX IDX1)
      *
                           IF   FLG-RYAKU   =   ZERO
                              MOVE   NACCT-SRYCDTOTAL(6:9)
                                            TO  WRK-RYAKU-SRYCD
                              MOVE   NACCT-ZAISKBKBN
                                            TO  WRK-ZAISKBKBN
                              PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                              MOVE    1     TO    FLG-RYAKU
                           END-IF
                         END-IF
      *
                       END-IF
                   END-PERFORM
                   MOVE   "tbl_nyuinacct"   TO  MCP-TABLE
                   MOVE   "key17"           TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    ∆˛±°≤Ò∑◊•Ø•Ì°º•∫
           MOVE     "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE     "key17"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *
      *œ´∫“∆˛±°ª˛§Œ∆˛±°Œ¡≤√ªªºË∆¿° ø«Œ≈≤Ò∑◊§Ë§Í°À
           INITIALIZE                           SRYACCT-REC
           MOVE    LNK-SROUNYUIN-HOSPNUM   TO   ACCT-HOSPNUM
           MOVE    "1"                     TO   ACCT-NYUGAIKBN
           MOVE    LNK-SROUNYUIN-PTID      TO   ACCT-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO   ACCT-SRYYM
           MOVE    LNK-SROUNYUIN-HKNCOMBI  TO   ACCT-HKNCOMBI
           MOVE    "tbl_sryacct"           TO   MCP-TABLE
           MOVE    "key22"                 TO   MCP-PATHNAME
           PERFORM 910-SRYACCT-SELECT-SEC
           IF      FLG-SRYACCT           =   ZERO
               MOVE     "tbl_sryacct"      TO   MCP-TABLE
               MOVE     "key22"            TO   MCP-PATHNAME
               PERFORM   910-SRYACCT-READ-SEC
               PERFORM   UNTIL   (FLG-SRYACCT    =   1)
                 MOVE      ZERO     TO      FLG-RYAKU
                 IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                      (ACCT-ZAIKAISU  NOT =    ZERO)    AND
                      (ACCT-JIHOKBN       =   SPACE)
                   IF   ACCT-SRYKBN  =  "90" OR "92"
      *
                     INITIALIZE                         SRYACT-REC
                     MOVE    ACCT-HOSPNUM       TO      SRY-HOSPNUM
                     MOVE    ACCT-NYUGAIKBN     TO      SRY-NYUGAIKBN
                     MOVE    ACCT-PTID          TO      SRY-PTID
                     MOVE    ACCT-SRYYM         TO      SRY-SRYYM
                     MOVE    ACCT-ZAINUM        TO      SRY-ZAINUM
                     MOVE    SRYACT-REC         TO      MCPDATA-REC
                     MOVE    "DBSELECT"         TO      MCP-FUNC
                     MOVE    "tbl_sryact"       TO      MCP-TABLE
                     MOVE    "key9"             TO      MCP-PATHNAME
                     CALL    "ORCDBMAIN"        USING   MCPAREA
                                                        MCPDATA-REC
                                                        SPA-AREA
                     IF      MCP-RC               =       ZERO
                         MOVE    "tbl_sryact"   TO      MCP-TABLE
                         MOVE    "key9"         TO      MCP-PATHNAME
                         PERFORM  910-SRYACT-READ-SEC
                         IF   FLG-SRYACT    =    0
                            IF (SRY-SRYCD(1)  =  CONS-KEIKAKU-GENSAN1
                                              OR CONS-KEIKAKU-GENSAN2
                                              OR CONS-INNAI-GENSAN1
                                              OR CONS-INNAI-GENSAN2
                                              OR CONS-ANZEN-GENSAN1
                                              OR CONS-ANZEN-GENSAN2
                                              OR CONS-JYOKU-GENSAN1
                                              OR CONS-JYOKU-GENSAN2
                                              OR CONS-NICHIJOU-KSN1
                                              OR CONS-NICHIJOU-KSN2
                                              OR CONS-CHIHOU-KSN1
                                              OR CONS-CHIHOU-KSN2  )
                           OR ((SRY-SRYCD(1)  =  CONS-JYOKU-KSN1
                                               OR  CONS-JYOKU-KSN2) AND
                              (LNK-SROUNYUIN-SANTEIYM  < "200604"  ))
                           OR ((SRY-SRYCD(1)    =  CONS-KANGO-KSN1
                                               OR  CONS-KANGO-KSN2) AND
                              (LNK-SROUNYUIN-SANTEIYM >= "200604"  ))
                           OR ((SRY-SRYCD(1)    = CONS-NINSYO-KSN1
                                               OR CONS-NINSYO-KSN2) AND
                              (LNK-SROUNYUIN-SANTEIYM >= "200607"  ))
      *                                        
                                MOVE  SRY-SRYCD(1)   TO
                                               ACCT-SRYCDTOTAL(6:9)
                            END-IF
                         END-IF
                     END-IF
      *              ø«Œ≈π‘∞Ÿ•Ø•Ì°º•∫
                     MOVE     "tbl_sryact"    TO    MCP-TABLE
                     MOVE     "key9"          TO    MCP-PATHNAME
                     PERFORM   910-DB-CLOSE-SEC
      *
                     IF ( ACCT-SRYCDTOTAL(6:9)  =
                                            CONS-KEIKAKU-GENSAN1
                                        OR  CONS-KEIKAKU-GENSAN2
                                        OR  CONS-INNAI-GENSAN1
                                        OR  CONS-INNAI-GENSAN2
                                        OR  CONS-ANZEN-GENSAN1
                                        OR  CONS-ANZEN-GENSAN2
                                        OR  CONS-JYOKU-GENSAN1
                                        OR  CONS-JYOKU-GENSAN2
                                        OR  CONS-NICHIJOU-KSN1
                                        OR  CONS-NICHIJOU-KSN2
                                        OR  CONS-CHIHOU-KSN1
                                        OR  CONS-CHIHOU-KSN2       )
                     OR ((ACCT-SRYCDTOTAL(6:9)  =  CONS-JYOKU-KSN1
                                               OR  CONS-JYOKU-KSN2) AND
                         (LNK-SROUNYUIN-SANTEIYM  < "200604"      ))
                     OR ((ACCT-SRYCDTOTAL(6:9)  =  CONS-KANGO-KSN1
                                               OR  CONS-KANGO-KSN2) AND
                         (LNK-SROUNYUIN-SANTEIYM >= "200604"      ))
                     OR ((ACCT-SRYCDTOTAL(6:9)  = CONS-NINSYO-KSN1
                                               OR CONS-NINSYO-KSN2) AND
                         (LNK-SROUNYUIN-SANTEIYM >= "200607"      ))
      *                ∆˛±°Œ¡≤√ªª§Œ¬‡»Ú
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF    (WRK-NYUINKHN(IDX1) NOT = SPACE)
                           AND   (LNK-SROUNYUIN-GAIHAKU(IDX1)  
                                                        =  ZERO)
                           AND   (ACCT-DAY(1 IDX1)   NOT =  ZERO)
                              EVALUATE   ACCT-SRYCDTOTAL(6:9)
      *                         ∆˛±°ø«Œ≈∑◊≤ËÃ§º¬ª‹∏∫ªª
                                WHEN  CONS-KEIKAKU-GENSAN1
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-KEIKAKU-GENSAN1(IDX1)
                                WHEN  CONS-KEIKAKU-GENSAN2
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-KEIKAKU-GENSAN2(IDX1)
      *                         ±°∆‚¥∂¿˜À…ªﬂ¬–∫ˆÃ§º¬ª‹∏∫ªª
                                WHEN  CONS-INNAI-GENSAN1
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-INNAI-GENSAN1(IDX1)
                                WHEN  CONS-INNAI-GENSAN2
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-INNAI-GENSAN2(IDX1)
      *                         ∞ÂŒ≈∞¬¡¥¥…Õ˝¬Œ¿©Ã§¿∞»˜∏∫ªª
                                WHEN  CONS-ANZEN-GENSAN1
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-ANZEN-GENSAN1(IDX1)
                                WHEN  CONS-ANZEN-GENSAN2
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-ANZEN-GENSAN2(IDX1)
      *                         ÍÛ·Ï¬–∫ˆÃ§º¬ª‹∏∫ªª
                                WHEN  CONS-JYOKU-GENSAN1
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-JYOKU-GENSAN1(IDX1)
                                WHEN  CONS-JYOKU-GENSAN2
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-JYOKU-GENSAN2(IDX1)
      *                         ∆¸æÔ¿∏≥Ëæ„≥≤≤√ªª
                                WHEN  CONS-NICHIJOU-KSN1
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-NICHIJOU-KSN1(IDX1)
                                WHEN  CONS-NICHIJOU-KSN2
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-NICHIJOU-KSN2(IDX1)
      *                         √‘ Ú≤√ªª
                                WHEN  CONS-CHIHOU-KSN1
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-CHIHOU-KSN1(IDX1)
                                WHEN  CONS-CHIHOU-KSN2
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-CHIHOU-KSN2(IDX1)
      *                         ÍÛ·Ï¥µº‘¥…Õ˝≤√ªª
                                WHEN  CONS-JYOKU-KSN1
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-JYOKU-KSN1(IDX1)
                                WHEN  CONS-JYOKU-KSN2
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-JYOKU-KSN2(IDX1)
      *                          £øÙ∞Âª’µ⁄§”¥«∏Ó«€√÷≈¨πÁ≤√ªª
                                WHEN  CONS-KANGO-KSN1
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-KANGO-KSN1(IDX1)
      *                         ¥«∏Ó«€√÷≈¨πÁ≤√ªª
                                WHEN  CONS-KANGO-KSN2
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-KANGO-KSN2(IDX1)
      *                         «ß√Œµ°«Ωæ„≥≤≤√ªª° Œ≈Õ‹…¬≈Ô°À
                                WHEN  CONS-NINSYO-KSN1
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-NINSYO-KSN1(IDX1)
      *                         «ß√Œµ°«Ωæ„≥≤≤√ªª° Õ≠æ≤ø«Œ≈ΩÍ°À
                                WHEN  CONS-NINSYO-KSN2
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-NINSYO-KSN2(IDX1)
                              END-EVALUATE
      *
                              IF   FLG-RYAKU   =   ZERO
                                 MOVE   ACCT-SRYCDTOTAL(6:9)
                                               TO  WRK-RYAKU-SRYCD
                                 MOVE   "6"    TO  WRK-ZAISKBKBN
                                 PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                                 MOVE    1     TO    FLG-RYAKU
                              END-IF
      *
                           END-IF
                       END-PERFORM
                     ELSE
                       IF  (ACCT-SRYCDTOTAL(6:9)  =  CONS-ROSAICHIRYO
                                                 OR  CONS-BYOUI
                                                 OR  CONS-TOKUKANSI)
                       OR  (ACCT-ZAIKBN           =    2           )
      *                  ∆˛±°Œ¡≤√ªª§Œ¬‡»Ú
                        IF  ACCT-ZAIKBN  =  2
                         PERFORM    VARYING   IDX1   FROM   1   BY   1
                                    UNTIL     IDX1   >   31
                             IF    ((WRK-NYUINKHN(IDX1) NOT = SPACE) OR
                                    (WRK-NYUINDAY-KBN(IDX1) =  "1" ))
                             AND   ( ACCT-DAY(1 IDX1)   NOT =  ZERO )
                               COMPUTE LNK-SROUNYUIN-GENTEN(IDX1)
                                    =  LNK-SROUNYUIN-GENTEN(IDX1)
                                    + (ACCT-ZAITEN * ACCT-DAY(1 IDX1))
                             END-IF
                         END-PERFORM
                        ELSE
                         PERFORM    VARYING   IDX1   FROM   1   BY   1
                                    UNTIL     IDX1   >   31
      **>                    IF    ((WRK-NYUINKHN(IDX1) NOT = SPACE) OR
      **>                           (WRK-NYUINDAY-KBN(IDX1) =  "1" ))
      **>                    AND   ( ACCT-DAY(1 IDX1)   NOT =  ZERO )
                             IF    ( ACCT-DAY(1 IDX1)   NOT =  ZERO )
                               COMPUTE LNK-SROUNYUIN-ZAITEN-ACCTETC
                                                             (IDX1)
                                    =  LNK-SROUNYUIN-ZAITEN-ACCTETC
                                                             (IDX1)
                                    + (ACCT-ZAITEN * ACCT-DAY(1 IDX1))
                               COMPUTE LNK-SROUNYUIN-ZAITEN-ACCTETC2
                                                             (IDX1)
                                    =  LNK-SROUNYUIN-ZAITEN-ACCTETC2
                                                             (IDX1)
                                    + (ACCT-ZAITEN * ACCT-DAY(2 IDX1))
                               COMPUTE LNK-SROUNYUIN-ZAITEN-ACCTETC3
                                                             (IDX1)
                                    =  LNK-SROUNYUIN-ZAITEN-ACCTETC3
                                                             (IDX1)
                                    + (ACCT-ZAITEN * ACCT-DAY(3 IDX1))
      *
                               IF   FLG-RYAKU   =   ZERO
                                  MOVE   ACCT-SRYCDTOTAL(6:9)
                                                TO  WRK-RYAKU-SRYCD
                                  MOVE   "6"    TO  WRK-ZAISKBKBN
                                  PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                                  MOVE    1     TO    FLG-RYAKU
                               END-IF
      *
                             END-IF
                         END-PERFORM
                        END-IF
                       ELSE
      *                  ∆˛±°Œ¡≤√ªª§Œ¬‡»Ú
                         PERFORM    VARYING   IDX1   FROM   1   BY   1
                                    UNTIL     IDX1   >   31
                             IF    ((WRK-NYUINKHN(IDX1) NOT = SPACE) OR
                                    (WRK-NYUINDAY-KBN(IDX1) =  "1" ))
                             AND   ( ACCT-DAY(1 IDX1)   NOT =  ZERO )
                               COMPUTE LNK-SROUNYUIN-ZAITEN-ACCT (IDX1)
                                    =  LNK-SROUNYUIN-ZAITEN-ACCT (IDX1)
                                    + (ACCT-ZAITEN * ACCT-DAY(1 IDX1))
                               COMPUTE LNK-SROUNYUIN-ZAITEN-ACCT2(IDX1)
                                    =  LNK-SROUNYUIN-ZAITEN-ACCT2(IDX1)
                                    + (ACCT-ZAITEN * ACCT-DAY(2 IDX1))
                               COMPUTE LNK-SROUNYUIN-ZAITEN-ACCT3(IDX1)
                                    =  LNK-SROUNYUIN-ZAITEN-ACCT3(IDX1)
                                    + (ACCT-ZAITEN * ACCT-DAY(3 IDX1))
      *
                               IF   FLG-RYAKU   =   ZERO
                                  PERFORM   210-SRYACT-HENSYU-SEC
      *
                                  MOVE    1     TO    FLG-RYAKU
                               END-IF
      *
                             END-IF
                         END-PERFORM
                       END-IF
                     END-IF
                   END-IF
                 END-IF
                 MOVE     "tbl_sryacct"         TO    MCP-TABLE
                 MOVE     "key22"               TO    MCP-PATHNAME
                 PERFORM   910-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *    ø«Œ≈≤Ò∑◊•Ø•Ì°º•∫
           MOVE     "tbl_sryacct"         TO    MCP-TABLE
           MOVE     "key22"               TO    MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *
      *    ºË∆¿§∑§øœ´∫“∆˛±°æ Û§´§È∆¸√±∞Ã§Œ∆˛±°≤Ò∑◊æ Û§Ú∫∆πΩ√€
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
               MOVE     ZERO                TO  WRK-MEISAISU
      *        ∆˛±°¥À‹Œ¡
               IF         WRK-NYUINKHN(IDX1)  NOT =        SPACE
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-NYUINKHN(IDX1) TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "1"           TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ≥∞«Ò
               IF        (WRK-NYUINKHN(IDX1)  NOT =        SPACE)
               AND       (WRK-GAIHAKU(IDX1)   NOT =        SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-GAIHAKU(IDX1)  TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "2"           TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ∆¸æÔ¿∏≥Ëæ„≥≤≤√ªª
               IF         WRK-NICHIJOU-KSN1(IDX1)   NOT =   ZERO
                   ADD      1                 TO  WRK-MEISAISU
                   MOVE   CONS-NICHIJOU-KSN1  TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"            TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
               IF         WRK-NICHIJOU-KSN2(IDX1)   NOT =   ZERO
                   ADD      1                 TO  WRK-MEISAISU
                   MOVE   CONS-NICHIJOU-KSN2  TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"            TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        √‘ Ú≤√ªª
               IF         WRK-CHIHOU-KSN1(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-CHIHOU-KSN1     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
               IF         WRK-CHIHOU-KSN2(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-CHIHOU-KSN2     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *         £øÙ∞Âª’µ⁄§”¥«∏Ó«€√÷≈¨πÁ≤√ªª
               IF         WRK-KANGO-KSN1(IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-KANGO-KSN1      TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ¥«∏Ó«€√÷≈¨πÁ≤√ªª
               IF         WRK-KANGO-KSN2(IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-KANGO-KSN2      TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        «ß√Œµ°«Ωæ„≥≤≤√ªª° Œ≈Õ‹…¬≈Ô°À
               IF         WRK-NINSYO-KSN1(IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-NINSYO-KSN1     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        «ß√Œµ°«Ωæ„≥≤≤√ªª° Õ≠æ≤ø«Œ≈ΩÍ°À
               IF         WRK-NINSYO-KSN2(IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-NINSYO-KSN2     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ÍÛ·Ï¥µº‘¥…Õ˝≤√ªª
               IF         WRK-JYOKU-KSN1(IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-JYOKU-KSN1      TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
               IF         WRK-JYOKU-KSN2(IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-JYOKU-KSN2      TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ∆˛±°ø«Œ≈∑◊≤ËÃ§º¬ª‹∏∫ªª
               IF         WRK-KEIKAKU-GENSAN1(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-KEIKAKU-GENSAN1 TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
               IF         WRK-KEIKAKU-GENSAN2(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-KEIKAKU-GENSAN2 TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ±°∆‚¥∂¿˜À…ªﬂ¬–∫ˆÃ§º¬ª‹∏∫ªª
               IF         WRK-INNAI-GENSAN1(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-INNAI-GENSAN1   TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
               IF         WRK-INNAI-GENSAN2(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-INNAI-GENSAN2   TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ∞ÂŒ≈∞¬¡¥¥…Õ˝¬Œ¿©Ã§¿∞»˜∏∫ªª
               IF         WRK-ANZEN-GENSAN1(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-ANZEN-GENSAN1   TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
               IF         WRK-ANZEN-GENSAN2(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-ANZEN-GENSAN2   TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ÍÛ·Ï¬–∫ˆÃ§º¬ª‹∏∫ªª
               IF         WRK-JYOKU-GENSAN1(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-JYOKU-GENSAN1   TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
               IF         WRK-JYOKU-GENSAN2(IDX1) NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-JYOKU-GENSAN2   TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ∏∫ªªæ Û° •∑•π•∆•‡¥…Õ˝°À
               IF      (WRK-NYUINKHN(IDX1)   NOT =         SPACE)  AND
                       (WRK-GAIHAKU(IDX1)        =         SPACE)  AND
                       (WRK-GENSAN-INNAI(IDX1)   NOT =     SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-GENSAN2       TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"          TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                   MOVE     1               TO  WRK-GENSAN2-MIJISI
               END-IF
               IF      (WRK-NYUINKHN(IDX1)   NOT =         SPACE)  AND
                       (WRK-GAIHAKU(IDX1)        =         SPACE)  AND
                       (WRK-GENSAN-IRYO(IDX1)   NOT =      SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-GENSAN3       TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"          TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                   MOVE     1               TO  WRK-GENSAN3-MIJISI
               END-IF
               IF      (WRK-NYUINKHN(IDX1)   NOT =         SPACE)  AND
                       (WRK-GAIHAKU(IDX1)        =         SPACE)  AND
                       (WRK-GENSAN-JYOKU(IDX1)   NOT =     SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-GENSAN4       TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"          TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                   MOVE     1               TO  WRK-GENSAN4-MIJISI
               END-IF
      *        œ´∫“æËøÙ° ∆√ƒÍ∆˛±°Œ¡§ŒæÏπÁ§œœ´∫“æËøÙ§ §∑°À
               IF       LNK-SROUNYUIN-SRYKBN(IDX1)    =    "90"
                 IF     FLG-KENPOJYUNKYO  =  1
                   CONTINUE
                 ELSE
                   EVALUATE  LNK-SROUNYUIN-JOUSU-KBN(IDX1)
                       WHEN  "1"
                           ADD      1           TO  WRK-MEISAISU
                           MOVE   "101900010"   TO  LNK-SROUNYUIN-SRYCD
                                               (IDX1 WRK-MEISAISU)
                           MOVE    "4"      TO  LNK-SROUNYUIN-SRYCD-SK
                                               (IDX1 WRK-MEISAISU)
                       WHEN  "2"
                           ADD      1           TO  WRK-MEISAISU
                           MOVE   "101900020"   TO  LNK-SROUNYUIN-SRYCD
                                               (IDX1 WRK-MEISAISU)
                           MOVE    "4"      TO  LNK-SROUNYUIN-SRYCD-SK
                                               (IDX1 WRK-MEISAISU)
                   END-EVALUATE
                 END-IF
               END-IF
      *        ΩÈ¥¸≤√ªª° ƒπ¥¸∏∫ªª°À
               IF      (WRK-GAIHAKU(IDX1)        =         SPACE)  AND
                       (WRK-SHOKIKSN(IDX1)   NOT =         SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-SHOKIKSN(IDX1) TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "5"           TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        …∏∑Á
               IF       WRK-HYOKETU(IDX1)    NOT =         SPACE
                 IF    (WRK-GAIHAKU(IDX1)        =
                                     SPACE      OR
                                    "190108790" OR  "190108890" OR
                                    "190109090"                 )
                 AND   (WRK-NYUINKHN(IDX1)   NOT =       SPACE  )
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-HYOKETU(IDX1)  TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "6"           TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                 ELSE
                   MOVE   SPACE             TO  WRK-HYOKETU(IDX1)
                 END-IF
               END-IF
      *        ∆˛±°Œ¡≤√ªª° ∆˛±°≤Ò∑◊°À
      *         PERFORM   VARYING   KSN-IDX   FROM  1  BY  1
      *                   UNTIL    (WRK-NYUKSN(KSN-IDX)  =  SPACE)  OR
      *                            (KSN-IDX             >  20)
      *            IF    WRK-NYUKSN-DAY(KSN-IDX IDX1)  =  ZERO
      *                CONTINUE
      *            ELSE
      *                ADD     1           TO  WRK-MEISAISU
      *                MOVE WRK-NYUKSN(KSN-IDX)
      *                                    TO  LNK-SROUNYUIN-SRYCD
      *                                   (IDX1 WRK-MEISAISU)
      *            END-IF
      *         END-PERFORM
           END-PERFORM
      *
      *    ∫∆πΩ√€§∑§øœ´∫“∆˛±°≤Ò∑◊æ Û§´§È≈¿øÙ§ŒªªƒÍ
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
      *
               MOVE     ZERO       TO       WRK-HYOKETU-TEN
                                            WRK-ZAITEN-HYOKETU
                                            WRK-ZAITEN-HYOKETU2
               IF   WRK-HYOKETU(IDX1)   =   SPACE
      *          ≈¿øÙ•ﬁ•π•ø§Œ∆…§ﬂπ˛§ﬂ
                 PERFORM   VARYING   IDX2    FROM   1  BY   1
                      UNTIL (LNK-SROUNYUIN-SRYCD(IDX1 IDX2) = SPACE)
                         OR (IDX2    >      20)
                     PERFORM    210-TENSU-KEISAN-SEC
                 END-PERFORM
               ELSE
      *          ≈¿øÙ•ﬁ•π•ø§Œ∆…§ﬂπ˛§ﬂ
                 INITIALIZE                          TNS-TENSU-REC
                 MOVE    WRK-HYOKETU(IDX1)       TO  TNS-SRYCD
                 MOVE    LNK-SROUNYUIN-HOSPNUM   TO  TNS-HOSPNUM
                 MOVE    LNK-SROUNYUIN-SANTEIYM  TO  TNS-YUKOSTYMD(1:6)
                 MOVE    "01"                    TO  TNS-YUKOSTYMD(7:2)
                 MOVE    LNK-SROUNYUIN-SANTEIYM  TO  TNS-YUKOEDYMD(1:6)
                 MOVE    "01"                    TO  TNS-YUKOEDYMD(7:2)
                 PERFORM 910-TENSU-READ-SEC
                 MOVE    TNS-TEN                 TO  WRK-HYOKETU-TEN
      *
                 PERFORM   VARYING   IDX2    FROM   1  BY   1
                      UNTIL (LNK-SROUNYUIN-SRYCD(IDX1 IDX2) = SPACE)
                         OR (IDX2    >      20)
                     PERFORM    210-TENSU-KEISAN-HYOKETU-SEC
                 END-PERFORM
               END-IF
      *
               PERFORM   VARYING   KSN-IDX   FROM   1  BY   1
                         UNTIL (WRK-NYUKSN(KSN-IDX)  =  SPACE)  OR
                               (KSN-IDX    >      20)
                  IF    WRK-NYUKSN-DAY(KSN-IDX IDX1)  =  ZERO
                      CONTINUE
                  ELSE
                      ADD  WRK-NYUKSN-ZAITEN(KSN-IDX)
                                 TO  LNK-SROUNYUIN-ZAITEN-NACCT(IDX1)
                  END-IF
               END-PERFORM
      *
      ***      IF    LNK-SROUNYUIN-NYUIN-FLG(IDX1)      =   "1"
      ***      AND   LNK-SROUNYUIN-ZAITEN(IDX1)         =  ZERO
      ***         MOVE     SPACE   TO   LNK-SROUNYUIN-NYUIN-FLG(IDX1)
      ***      END-IF
           END-PERFORM
      *
      *
           IF  WRK-GENSAN2-MIJISI  =  1
                 MOVE    WRK-GENSAN2       TO  WRK-RYAKU-SRYCD
                 MOVE    "6"               TO  WRK-ZAISKBKBN
                 PERFORM   210-RYAKUSYO-HENSYU-SEC
           END-IF
      *
           IF  WRK-GENSAN3-MIJISI  =  1
                 MOVE    WRK-GENSAN3       TO  WRK-RYAKU-SRYCD
                 MOVE    "6"               TO  WRK-ZAISKBKBN
                 PERFORM   210-RYAKUSYO-HENSYU-SEC
           END-IF
      *
           IF  WRK-GENSAN4-MIJISI  =  1
                 MOVE    WRK-GENSAN4       TO  WRK-RYAKU-SRYCD
                 MOVE    "6"               TO  WRK-ZAISKBKBN
                 PERFORM   210-RYAKUSYO-HENSYU-SEC
           END-IF
      *
           .
       210-ROUNYUIN-SET1-EXT.
           EXIT.
      *
      *****************************************************************
      *    œ´∫“∆˛±°•µ•÷§Œ ÷µ—√Õ•ª•√•»°  ø¿Æ£≤£∞«Ø£¥∑Ó∞ πﬂ°À
      *****************************************************************
       210-ROUNYUIN-SET2-SEC            SECTION.
      *
      *œ´∫“æËøÙ§Œ£≤Ωµ¥÷∞ ∆‚§Œ∫«Ω™∆¸§ÚºË∆¿
           INITIALIZE                      ORCSRSITUSANAREA
           MOVE    LNK-SROUNYUIN-HOSPNUM
                                       TO  RSITUSAN-HOSPNUM
           MOVE    LNK-SROUNYUIN-PTID
                                       TO  RSITUSAN-PTID
           MOVE    LNK-SROUNYUIN-HKNCOMBI
                                       TO  RSITUSAN-HKNCOMBI
           MOVE    LNK-SROUNYUIN-SANTEIYM
                                       TO  RSITUSAN-SRYYM
           CALL    "ORCSRSITUSAN"      USING
                                           ORCSRSITUSANAREA
                                           SPA-AREA
           IF    ( RSITUSAN-RC          =   ZERO )
               CONTINUE
           ELSE
               INITIALIZE  ORCSRSITUSANAREA
           END-IF
           PERFORM   VARYING   IDX1   FROM 1  BY  1
                     UNTIL     IDX1   >    31
               MOVE  RSITUSAN-DAY(IDX1) TO LNK-SROUNYUIN-JOUSU-KBN(IDX1)
           END-PERFORM
      *
      *œ´∫“∆˛±°Œ¡§ŒºË∆¿
           MOVE    ZERO                    TO  FLG-OK
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-SROUNYUIN-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-SROUNYUIN-PTID      TO  NACCT-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO  NACCT-SRYYM
           MOVE    "1"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key17"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                   MOVE      ZERO     TO      FLG-RYAKU
                                              WRK-KSNKBN
                                              WRK-CDKBN-KBNNUM
                   MOVE      SPACE    TO      WRK-CHUKSNCD-XXX
                   MOVE      ZERO     TO      WRK-NYUKHNKBN-XXX
                   MOVE      SPACE    TO      WRK-RYAKUSHO
                                              WRK-SRYKBN
                                              WRK-SHOKIKSN-TAIHI
                                              WRK-HYOKETU-TAIHI
                   IF    NACCT-SRYCDTOTAL     =    99999916
                       CONTINUE
                   ELSE
      *                ≥∫≈ˆ∆˛±°π‘∞Ÿ•∆°º•÷•Î§Œ∆…§ﬂπ˛§ﬂ
                       PERFORM    210-NYUINACT-GET-SEC
      *                ∆˛±°¥À‹Œ¡§»ΩÈ¥¸≤√ªª° ƒπ¥¸∏∫ªª°À§Œ¬‡»Ú
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF    (LNK-SROUNYUIN-NYUIN-FLG(IDX1) = "1")
                           AND   (NACCT-DAY(IDX1)    NOT =  ZERO) 
                               MOVE    NSRY-SRYCD(1)   TO  
                                                   WRK-NYUINKHN(IDX1)
      *
                               IF   FLG-RYAKU   =   ZERO
      *
                                  IF   NSRY-SRYCD(1)  NOT =   SPACE
                                      INITIALIZE    TNS-TENSU-REC
                                      MOVE NSRY-SRYCD(1)
                                                TO  TNS-SRYCD
                                      MOVE LNK-SROUNYUIN-HOSPNUM
                                                TO  TNS-HOSPNUM
                                      MOVE LNK-SROUNYUIN-SANTEIYM
                                                TO  TNS-YUKOSTYMD(1:6)
                                      MOVE "01" TO  TNS-YUKOSTYMD(7:2)
                                      MOVE LNK-SROUNYUIN-SANTEIYM
                                                TO  TNS-YUKOEDYMD(1:6)
                                      MOVE "01" TO  TNS-YUKOEDYMD(7:2)
                                      PERFORM 910-TENSU-READ-SEC
                                      IF  FLG-TENSU  NOT = ZERO
                                        MOVE  1 TO  WRK-NYUINRYO-ERR
                                      END-IF
                                  END-IF
      *
                                  PERFORM  VARYING  IDX3  FROM  2  BY  1
                                            UNTIL   IDX3   >    3
                                    IF   NSRY-SRYCD(IDX3)  NOT =   SPACE
                                      INITIALIZE    TNS-TENSU-REC
                                      MOVE NSRY-SRYCD(IDX3)
                                                TO  TNS-SRYCD
                                      MOVE LNK-SROUNYUIN-HOSPNUM
                                                TO  TNS-HOSPNUM
                                      MOVE LNK-SROUNYUIN-SANTEIYM
                                                TO  TNS-YUKOSTYMD(1:6)
                                      MOVE "01" TO  TNS-YUKOSTYMD(7:2)
                                      MOVE LNK-SROUNYUIN-SANTEIYM
                                                TO  TNS-YUKOEDYMD(1:6)
                                      MOVE "01" TO  TNS-YUKOEDYMD(7:2)
                                      PERFORM 910-TENSU-READ-SEC
                                      EVALUATE  TNS-TENSIKIBETU
                                         WHEN     3
                                         WHEN     8
                                             MOVE   NSRY-SRYCD(IDX3)  TO
                                                      WRK-SHOKIKSN-TAIHI
                                         WHEN     6
                                             MOVE   NSRY-SRYCD(IDX3)  TO
                                                      WRK-HYOKETU-TAIHI
                                      END-EVALUATE
                                    END-IF
                                  END-PERFORM
      *
                                  MOVE    NACCT-SRYKBN
                                                TO  WRK-SRYKBN
      *                           ≥∞«Ò•≥°º•… ‘Ω∏Õ—§À∆˛±°¥À‹Œ¡≤√ªª∂Ë ¨
      *                           §Œ¬‡»Ú
                                  PERFORM   210-GAIHAKU-MAE-SEC
      *
                                  MOVE   NSRY-SRYCD(1)
                                                TO  WRK-RYAKU-SRYCD
                                  MOVE   NACCT-ZAISKBKBN
                                                TO  WRK-ZAISKBKBN
                                  PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                                  MOVE    1     TO    FLG-RYAKU
                               END-IF
      *
                               IF    WRK-SHOKIKSN-TAIHI  NOT = SPACE
                                  MOVE  WRK-SHOKIKSN-TAIHI
                                               TO  WRK-SHOKIKSN(IDX1)
                               END-IF
                               IF    WRK-HYOKETU-TAIHI   NOT = SPACE
                                  MOVE  WRK-HYOKETU-TAIHI
                                               TO  WRK-HYOKETU(IDX1)
                               END-IF
      *
                               MOVE  WRK-SRYKBN
                                         TO  LNK-SROUNYUIN-SRYKBN(IDX1)
                               MOVE  WRK-KSNKBN
                                         TO  WRK-NYUKHNKSNKBN(IDX1)
                               MOVE  WRK-CHUKSNCD-XXX
                                         TO  WRK-CHUKSNCD(IDX1)
                               MOVE  WRK-NYUKHNKBN-XXX
                                         TO  WRK-NYUKHNKBN(IDX1)
      *
                               IF    WRK-RYAKUSHO   NOT =  SPACE
                                  MOVE  WRK-RYAKUSHO
                                      TO  LNK-SROUNYUIN-RYAKUSYO(IDX1)
                               END-IF
      *
                               MOVE    1    TO   FLG-OK
                           END-IF
                       END-PERFORM
                   END-IF
                   MOVE   "tbl_nyuinacct"   TO  MCP-TABLE
                   MOVE   "key17"           TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
      ***      IF   FLG-OK     =   ZERO
      ***          MOVE     6      TO      LNK-SROUNYUIN-RC
      ***      END-IF
           ELSE
               MOVE     6      TO      LNK-SROUNYUIN-RC
           END-IF
      *    ∆˛±°≤Ò∑◊•Ø•Ì°º•∫
           MOVE     "tbl_nyuinacct"   TO  MCP-TABLE
           MOVE     "key17"           TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           IF   LNK-SROUNYUIN-RC    NOT =   ZERO
                   GO   TO    200-MAIN-EXT
           END-IF
      *
      *œ´∫“∆˛±°§Œœ´∫“æËøÙ° ¥¸¥÷≤√ªª°À§À¥ﬁ§·§Î√Ì≤√ªª§ŒºË∆¿
           PERFORM    210-CHUKSN-GET-SEC
      *
      *œ´∫“∆˛±°ª˛§Œ≥∞«ÒºË∆¿
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-SROUNYUIN-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-SROUNYUIN-PTID      TO  NACCT-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO  NACCT-SRYYM
           MOVE    "2"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key17"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
      *            ≥∞«Ò∂Ë ¨§Œ¬‡»Ú
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                              UNTIL     IDX1   >   31
      ****             IF    (WRK-NYUINKHN(IDX1) NOT = SPACE) 
                       IF     NACCT-DAY(IDX1)    NOT =  ZERO
                              PERFORM     210-GAIHAKU-SET-SEC
                       END-IF
                   END-PERFORM
                   MOVE   "tbl_nyuinacct"  TO  MCP-TABLE
                   MOVE   "key17"          TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    ∆˛±°≤Ò∑◊•Ø•Ì°º•∫
           MOVE     "tbl_nyuinacct"        TO  MCP-TABLE
           MOVE     "key17"                TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *œ´∫“∆˛±°ª˛§Œ∆˛±°Œ¡≤√ªªºË∆¿° ∆˛±°≤Ò∑◊§Ë§Í°À
           MOVE    ZERO                    TO  KSN-IDX
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-SROUNYUIN-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-SROUNYUIN-PTID      TO  NACCT-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO  NACCT-SRYYM
           MOVE    "6"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key17"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                   MOVE      ZERO     TO      FLG-RYAKU
                   MOVE      ZERO     TO      FLG-KSN
      *            ∆˛±°Œ¡≤√ªª§Œ¬‡»Ú
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                              UNTIL     IDX1   >   31
                       IF    ((WRK-NYUINKHN(IDX1) NOT = SPACE) OR
                              (WRK-NYUINDAY-KBN(IDX1) =  "1" ))
                       AND   ((LNK-SROUNYUIN-GAIHAKU(IDX1)
                                                      = ZERO ) OR
                              (LNK-SROUNYUIN-GAIHAKU(IDX1)
                                                     >=   4  ))
                       AND   ( NACCT-DAY(IDX1)    NOT =  ZERO )
      *
      *                  KSN1°ßÃÎ¥÷∂€µﬁ¬Œ¿©≥Œ ›≤√ªª
      *                  KSN2°ß∞Âª’«€√÷≈¨πÁ≤√ªª
      *                  KSN3°ß¥«∏Ó«€√÷≤√ªª£±
      *                  KSN4°ß¥«∏Ó«€√÷≤√ªª£≤
      *                  KSN5°ßÃÎ¥÷¥«∏Ó«€√÷≤√ªª£±
      *                  KSN6°ßÃÎ¥÷¥«∏Ó«€√÷≤√ªª£≤
      *                  KSN10°ßΩ≈æ…¥µº‘≤Û…¸…¬≈Ô≤√ªª
                         IF (NACCT-SRYCDTOTAL(6:9) = CONS-200804-KSN1
                                                  OR CONS-200804-KSN2
                                                  OR CONS-200804-KSN3
                                                  OR CONS-200804-KSN4
                                                  OR CONS-200804-KSN5
                                                  OR CONS-200804-KSN6
                                                  OR CONS-200804-KSN10)
                          IF  (WRK-NYUINKHN(IDX1)      NOT = SPACE)
                          AND ((LNK-SROUNYUIN-GAIHAKU(IDX1)
                                                           = ZERO) OR
                               (LNK-SROUNYUIN-GAIHAKU(IDX1)
                                                          >=   4 ))
                           EVALUATE   NACCT-SRYCDTOTAL(6:9)
                             WHEN  CONS-200804-KSN1
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-200804-KSN1(IDX1)
                             WHEN  CONS-200804-KSN2
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-200804-KSN2(IDX1)
                             WHEN  CONS-200804-KSN3
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-200804-KSN3(IDX1)
                             WHEN  CONS-200804-KSN4
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-200804-KSN4(IDX1)
                             WHEN  CONS-200804-KSN5
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-200804-KSN5(IDX1)
                             WHEN  CONS-200804-KSN6
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-200804-KSN6(IDX1)
                             WHEN  CONS-200804-KSN10
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-200804-KSN10(IDX1)
                           END-EVALUATE
      *
                           IF   FLG-RYAKU   =   ZERO
                              MOVE   NACCT-SRYCDTOTAL(6:9)
                                            TO  WRK-RYAKU-SRYCD
                              MOVE   NACCT-ZAISKBKBN
                                            TO  WRK-ZAISKBKBN
                              PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                              MOVE    1     TO    FLG-RYAKU
                           END-IF
                          END-IF
                         ELSE
                           IF   FLG-KSN   =  ZERO
                               ADD     1    TO    KSN-IDX
                               MOVE    1    TO    FLG-KSN
                           END-IF
                           MOVE   NACCT-SRYCDTOTAL(6:9)
                                        TO  WRK-NYUKSN(KSN-IDX)
                           MOVE   NACCT-ZAITEN  
                                        TO  WRK-NYUKSN-ZAITEN(KSN-IDX)
                           MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-NYUKSN-DAY(KSN-IDX IDX1)
      *
                           IF   FLG-RYAKU   =   ZERO
                              MOVE   NACCT-SRYCDTOTAL(6:9)
                                            TO  WRK-RYAKU-SRYCD
                              MOVE   NACCT-ZAISKBKBN
                                            TO  WRK-ZAISKBKBN
                              PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                              MOVE    1     TO    FLG-RYAKU
                           END-IF
                         END-IF
      *
                       END-IF
                   END-PERFORM
                   MOVE   "tbl_nyuinacct"   TO  MCP-TABLE
                   MOVE   "key17"           TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    ∆˛±°≤Ò∑◊•Ø•Ì°º•∫
           MOVE     "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE     "key17"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *
      *œ´∫“∆˛±°ª˛§Œ∆˛±°Œ¡≤√ªªºË∆¿° ø«Œ≈≤Ò∑◊§Ë§Í°À
           INITIALIZE                           SRYACCT-REC
           MOVE    LNK-SROUNYUIN-HOSPNUM   TO   ACCT-HOSPNUM
           MOVE    "1"                     TO   ACCT-NYUGAIKBN
           MOVE    LNK-SROUNYUIN-PTID      TO   ACCT-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO   ACCT-SRYYM
           MOVE    LNK-SROUNYUIN-HKNCOMBI  TO   ACCT-HKNCOMBI
           MOVE    "tbl_sryacct"           TO   MCP-TABLE
           MOVE    "key22"                 TO   MCP-PATHNAME
           PERFORM 910-SRYACCT-SELECT-SEC
           IF      FLG-SRYACCT           =   ZERO
               MOVE     "tbl_sryacct"      TO   MCP-TABLE
               MOVE     "key22"            TO   MCP-PATHNAME
               PERFORM   910-SRYACCT-READ-SEC
               PERFORM   UNTIL   (FLG-SRYACCT    =   1)
                 MOVE      ZERO     TO      FLG-RYAKU
                 IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                      (ACCT-ZAIKAISU  NOT =    ZERO)    AND
                      (ACCT-JIHOKBN       =   SPACE)
                   IF   ACCT-SRYKBN  =  "90" OR "92"
      *
                     INITIALIZE                         SRYACT-REC
                     MOVE    ACCT-HOSPNUM       TO      SRY-HOSPNUM
                     MOVE    ACCT-NYUGAIKBN     TO      SRY-NYUGAIKBN
                     MOVE    ACCT-PTID          TO      SRY-PTID
                     MOVE    ACCT-SRYYM         TO      SRY-SRYYM
                     MOVE    ACCT-ZAINUM        TO      SRY-ZAINUM
                     MOVE    SRYACT-REC         TO      MCPDATA-REC
                     MOVE    "DBSELECT"         TO      MCP-FUNC
                     MOVE    "tbl_sryact"       TO      MCP-TABLE
                     MOVE    "key9"             TO      MCP-PATHNAME
                     CALL    "ORCDBMAIN"        USING   MCPAREA
                                                        MCPDATA-REC
                                                        SPA-AREA
                     IF      MCP-RC               =       ZERO
                         MOVE    "tbl_sryact"   TO      MCP-TABLE
                         MOVE    "key9"         TO      MCP-PATHNAME
                         PERFORM  910-SRYACT-READ-SEC
                         IF   FLG-SRYACT    =    0
      *                     KSN7°ßΩ≈≈Ÿ«ß√Œæ…≤√ªª
      *                     KSN8°ßÍÛ·Ï…æ≤¡º¬ª‹≤√ªª° Œ≈Õ‹…¬≈Ô°À
      *                     KSN9°ßÍÛ·Ï…æ≤¡º¬ª‹≤√ªª° Õ≠æ≤ø«Œ≈ΩÍ°À
                            IF (SRY-SRYCD(1)  =  CONS-200804-KSN7
                                              OR CONS-200804-KSN8
                                              OR CONS-200804-KSN9 )
      *                                        
                                MOVE  SRY-SRYCD(1)   TO
                                               ACCT-SRYCDTOTAL(6:9)
                            END-IF
                         END-IF
                     END-IF
      *              ø«Œ≈π‘∞Ÿ•Ø•Ì°º•∫
                     MOVE     "tbl_sryact"    TO    MCP-TABLE
                     MOVE     "key9"          TO    MCP-PATHNAME
                     PERFORM   910-DB-CLOSE-SEC
      *
                     IF ( ACCT-SRYCDTOTAL(6:9)  =
                                            CONS-200804-KSN7
                                        OR  CONS-200804-KSN8
                                        OR  CONS-200804-KSN9 )
      *                ∆˛±°Œ¡≤√ªª§Œ¬‡»Ú
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF    (WRK-NYUINKHN(IDX1) NOT = SPACE)
                           AND   ((LNK-SROUNYUIN-GAIHAKU(IDX1)
                                                         = ZERO) OR
                                  (LNK-SROUNYUIN-GAIHAKU(IDX1)
                                                        >=   4 ))
                           AND   (ACCT-DAY(1 IDX1)   NOT =  ZERO)
                              EVALUATE   ACCT-SRYCDTOTAL(6:9)
      *                         Ω≈≈Ÿ«ß√Œæ…≤√ªª
                                WHEN  CONS-200804-KSN7
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-200804-KSN7(IDX1)
      *                         ÍÛ·Ï…æ≤¡º¬ª‹≤√ªª° Œ≈Õ‹…¬≈Ô°À
                                WHEN  CONS-200804-KSN8
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-200804-KSN8(IDX1)
      *                         ÍÛ·Ï…æ≤¡º¬ª‹≤√ªª° Õ≠æ≤ø«Œ≈ΩÍ°À
                                WHEN  CONS-200804-KSN9
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-200804-KSN9(IDX1)
                              END-EVALUATE
      *
                              IF   FLG-RYAKU   =   ZERO
                                 MOVE   ACCT-SRYCDTOTAL(6:9)
                                               TO  WRK-RYAKU-SRYCD
                                 MOVE   "6"    TO  WRK-ZAISKBKBN
                                 PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                                 MOVE    1     TO    FLG-RYAKU
                              END-IF
      *
                           END-IF
                       END-PERFORM
                     ELSE
                       IF  (ACCT-SRYCDTOTAL(6:9)  =  CONS-ROSAICHIRYO
                                                 OR  CONS-BYOUI
                                                 OR  CONS-TOKUKANSI)
                       OR  (ACCT-ZAIKBN           =    2           )
      *                  ∆˛±°Œ¡≤√ªª§Œ¬‡»Ú
                        IF  ACCT-ZAIKBN  =  2
                         PERFORM    VARYING   IDX1   FROM   1   BY   1
                                    UNTIL     IDX1   >   31
                             IF    ((WRK-NYUINKHN(IDX1) NOT = SPACE) OR
                                    (WRK-NYUINDAY-KBN(IDX1) =  "1" ))
                             AND   ( ACCT-DAY(1 IDX1)   NOT =  ZERO )
                               COMPUTE LNK-SROUNYUIN-GENTEN(IDX1)
                                    =  LNK-SROUNYUIN-GENTEN(IDX1)
                                    + (ACCT-ZAITEN * ACCT-DAY(1 IDX1))
                             END-IF
                         END-PERFORM
                        ELSE
                         PERFORM    VARYING   IDX1   FROM   1   BY   1
                                    UNTIL     IDX1   >   31
      **>                    IF    ((WRK-NYUINKHN(IDX1) NOT = SPACE) OR
      **>                           (WRK-NYUINDAY-KBN(IDX1) =  "1" ))
      **>                    AND   ( ACCT-DAY(1 IDX1)   NOT =  ZERO )
                             IF    ( ACCT-DAY(1 IDX1)   NOT =  ZERO )
                               COMPUTE LNK-SROUNYUIN-ZAITEN-ACCTETC
                                                             (IDX1)
                                    =  LNK-SROUNYUIN-ZAITEN-ACCTETC
                                                             (IDX1)
                                    + (ACCT-ZAITEN * ACCT-DAY(1 IDX1))
                               COMPUTE LNK-SROUNYUIN-ZAITEN-ACCTETC2
                                                             (IDX1)
                                    =  LNK-SROUNYUIN-ZAITEN-ACCTETC2
                                                             (IDX1)
                                    + (ACCT-ZAITEN * ACCT-DAY(2 IDX1))
                               COMPUTE LNK-SROUNYUIN-ZAITEN-ACCTETC3
                                                             (IDX1)
                                    =  LNK-SROUNYUIN-ZAITEN-ACCTETC3
                                                             (IDX1)
                                    + (ACCT-ZAITEN * ACCT-DAY(3 IDX1))
      *
                               IF   FLG-RYAKU   =   ZERO
                                  MOVE   ACCT-SRYCDTOTAL(6:9)
                                                TO  WRK-RYAKU-SRYCD
                                  MOVE   "6"    TO  WRK-ZAISKBKBN
                                  PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                                  MOVE    1     TO    FLG-RYAKU
                               END-IF
      *
                             END-IF
                         END-PERFORM
                        END-IF
                       ELSE
      *                  ∆˛±°Œ¡≤√ªª§Œ¬‡»Ú
                         PERFORM    VARYING   IDX1   FROM   1   BY   1
                                    UNTIL     IDX1   >   31
                             IF    ((WRK-NYUINKHN(IDX1) NOT = SPACE) OR
                                    (WRK-NYUINDAY-KBN(IDX1) =  "1" ))
                             AND   ( ACCT-DAY(1 IDX1)   NOT =  ZERO )
                               COMPUTE LNK-SROUNYUIN-ZAITEN-ACCT (IDX1)
                                    =  LNK-SROUNYUIN-ZAITEN-ACCT (IDX1)
                                    + (ACCT-ZAITEN * ACCT-DAY(1 IDX1))
                               COMPUTE LNK-SROUNYUIN-ZAITEN-ACCT2(IDX1)
                                    =  LNK-SROUNYUIN-ZAITEN-ACCT2(IDX1)
                                    + (ACCT-ZAITEN * ACCT-DAY(2 IDX1))
                               COMPUTE LNK-SROUNYUIN-ZAITEN-ACCT3(IDX1)
                                    =  LNK-SROUNYUIN-ZAITEN-ACCT3(IDX1)
                                    + (ACCT-ZAITEN * ACCT-DAY(3 IDX1))
      *
                               IF   FLG-RYAKU   =   ZERO
                                  PERFORM   210-SRYACT-HENSYU-SEC
      *
                                  MOVE    1     TO    FLG-RYAKU
                               END-IF
      *
                             END-IF
                         END-PERFORM
                       END-IF
                     END-IF
                   END-IF
                 END-IF
                 MOVE     "tbl_sryacct"         TO    MCP-TABLE
                 MOVE     "key22"               TO    MCP-PATHNAME
                 PERFORM   910-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *    ø«Œ≈≤Ò∑◊•Ø•Ì°º•∫
           MOVE     "tbl_sryacct"         TO    MCP-TABLE
           MOVE     "key22"               TO    MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *
      *    ºË∆¿§∑§øœ´∫“∆˛±°æ Û§´§È∆¸√±∞Ã§Œ∆˛±°≤Ò∑◊æ Û§Ú∫∆πΩ√€
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
               MOVE     ZERO                TO  WRK-MEISAISU
      *        ∆˛±°¥À‹Œ¡
               IF         WRK-NYUINKHN(IDX1)  NOT =        SPACE
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-NYUINKHN(IDX1) TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "1"           TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ≥∞«Ò
               IF        (WRK-NYUINKHN(IDX1)  NOT =        SPACE)
               AND       (WRK-GAIHAKU(IDX1)   NOT =        SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-GAIHAKU(IDX1)  TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "2"           TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ÃÎ¥÷∂€µﬁ¬Œ¿©≥Œ ›≤√ªª
               IF         WRK-200804-KSN1(IDX1)   NOT =   ZERO
                   ADD      1                 TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN1  TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"            TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ∞Âª’«€√÷≈¨πÁ≤√ªª
               IF         WRK-200804-KSN2(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN2     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ¥«∏Ó«€√÷≤√ªª£±
               IF         WRK-200804-KSN3(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN3     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ¥«∏Ó«€√÷≤√ªª£≤
               IF         WRK-200804-KSN4(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN4     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ÃÎ¥÷¥«∏Ó«€√÷≤√ªª£±
               IF         WRK-200804-KSN5(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN5     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ÃÎ¥÷¥«∏Ó«€√÷≤√ªª£≤
               IF         WRK-200804-KSN6(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN6     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        Ω≈≈Ÿ«ß√Œæ…≤√ªª° ∆√ƒÍµ°«Ω…¬±°¿∫ø¿…¬≈Ô°À
               IF         WRK-200804-KSN7(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN7     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ÍÛ·Ï…æ≤¡º¬ª‹≤√ªª° Œ≈Õ‹…¬≈Ô°À
               IF         WRK-200804-KSN8(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN8     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ÍÛ·Ï…æ≤¡º¬ª‹≤√ªª° Õ≠æ≤ø«Œ≈ΩÍ°À
               IF         WRK-200804-KSN9(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN9     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        Ω≈æ…¥µº‘≤Û…¸…¬≈Ô≤√ªª
               IF         WRK-200804-KSN10(IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN10    TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *
               IF      (WRK-NYUINKHN(IDX1)   NOT =         SPACE)  AND
                       (WRK-GAIHAKU(IDX1)        =         SPACE)  AND
                       (WRK-GENSAN-IRYO(IDX1)   NOT =      SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-GENSAN3       TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"          TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                   MOVE     1               TO  WRK-GENSAN3-MIJISI
               END-IF
               IF      (WRK-NYUINKHN(IDX1)   NOT =         SPACE)  AND
                       (WRK-GAIHAKU(IDX1)        =         SPACE)  AND
                       (WRK-GENSAN-JYOKU(IDX1)   NOT =     SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-GENSAN4       TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"          TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                   MOVE     1               TO  WRK-GENSAN4-MIJISI
               END-IF
      *        œ´∫“æËøÙ° ∆√ƒÍ∆˛±°Œ¡§ŒæÏπÁ§œœ´∫“æËøÙ§ §∑°À
               IF       LNK-SROUNYUIN-SRYKBN(IDX1)    =    "90"
                 IF     FLG-KENPOJYUNKYO  =  1
                   CONTINUE
                 ELSE
                   EVALUATE  LNK-SROUNYUIN-JOUSU-KBN(IDX1)
                       WHEN  "1"
                           ADD      1           TO  WRK-MEISAISU
                           MOVE   "101900010"   TO  LNK-SROUNYUIN-SRYCD
                                               (IDX1 WRK-MEISAISU)
                           MOVE    "4"      TO  LNK-SROUNYUIN-SRYCD-SK
                                               (IDX1 WRK-MEISAISU)
                       WHEN  "2"
                           ADD      1           TO  WRK-MEISAISU
                           MOVE   "101900020"   TO  LNK-SROUNYUIN-SRYCD
                                               (IDX1 WRK-MEISAISU)
                           MOVE    "4"      TO  LNK-SROUNYUIN-SRYCD-SK
                                               (IDX1 WRK-MEISAISU)
                   END-EVALUATE
                 END-IF
               END-IF
      *        ΩÈ¥¸≤√ªª° ƒπ¥¸∏∫ªª°À
               IF      (WRK-GAIHAKU(IDX1)        =         SPACE)  AND
                       (WRK-SHOKIKSN(IDX1)   NOT =         SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-SHOKIKSN(IDX1) TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "5"           TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        …∏∑Á
               IF       WRK-HYOKETU(IDX1)    NOT =         SPACE
                 IF    (WRK-GAIHAKU(IDX1)        =
                                     SPACE      OR
                                    "190108790" OR  "190108890" OR
                                    "190109090"                 )
                 AND   (WRK-NYUINKHN(IDX1)   NOT =       SPACE  )
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-HYOKETU(IDX1)  TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "6"           TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                 ELSE
                   MOVE   SPACE             TO  WRK-HYOKETU(IDX1)
                 END-IF
               END-IF
      *        ∆˛±°Œ¡≤√ªª° ∆˛±°≤Ò∑◊°À
      *         PERFORM   VARYING   KSN-IDX   FROM  1  BY  1
      *                   UNTIL    (WRK-NYUKSN(KSN-IDX)  =  SPACE)  OR
      *                            (KSN-IDX             >  20)
      *            IF    WRK-NYUKSN-DAY(KSN-IDX IDX1)  =  ZERO
      *                CONTINUE
      *            ELSE
      *                ADD     1           TO  WRK-MEISAISU
      *                MOVE WRK-NYUKSN(KSN-IDX)
      *                                    TO  LNK-SROUNYUIN-SRYCD
      *                                   (IDX1 WRK-MEISAISU)
      *            END-IF
      *         END-PERFORM
           END-PERFORM
      *
      *    ∫∆πΩ√€§∑§øœ´∫“∆˛±°≤Ò∑◊æ Û§´§È≈¿øÙ§ŒªªƒÍ
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
      *
               MOVE     ZERO       TO       WRK-HYOKETU-TEN
                                            WRK-ZAITEN-HYOKETU
                                            WRK-ZAITEN-HYOKETU2
               IF   WRK-HYOKETU(IDX1)   =   SPACE
      *          ≈¿øÙ•ﬁ•π•ø§Œ∆…§ﬂπ˛§ﬂ
                 PERFORM   VARYING   IDX2    FROM   1  BY   1
                      UNTIL (LNK-SROUNYUIN-SRYCD(IDX1 IDX2) = SPACE)
                         OR (IDX2    >      20)
                     PERFORM    210-TENSU-KEISAN-SEC
                 END-PERFORM
               ELSE
      *          ≈¿øÙ•ﬁ•π•ø§Œ∆…§ﬂπ˛§ﬂ
                 INITIALIZE                          TNS-TENSU-REC
                 MOVE    WRK-HYOKETU(IDX1)       TO  TNS-SRYCD
                 MOVE    LNK-SROUNYUIN-HOSPNUM   TO  TNS-HOSPNUM
                 MOVE    LNK-SROUNYUIN-SANTEIYM  TO  TNS-YUKOSTYMD(1:6)
                 MOVE    "01"                    TO  TNS-YUKOSTYMD(7:2)
                 MOVE    LNK-SROUNYUIN-SANTEIYM  TO  TNS-YUKOEDYMD(1:6)
                 MOVE    "01"                    TO  TNS-YUKOEDYMD(7:2)
                 PERFORM 910-TENSU-READ-SEC
                 MOVE    TNS-TEN                 TO  WRK-HYOKETU-TEN
      *
                 PERFORM   VARYING   IDX2    FROM   1  BY   1
                      UNTIL (LNK-SROUNYUIN-SRYCD(IDX1 IDX2) = SPACE)
                         OR (IDX2    >      20)
                     PERFORM    210-TENSU-KEISAN-HYOKETU-SEC
                 END-PERFORM
               END-IF
      *
               PERFORM   VARYING   KSN-IDX   FROM   1  BY   1
                         UNTIL (WRK-NYUKSN(KSN-IDX)  =  SPACE)  OR
                               (KSN-IDX    >      20)
                  IF    WRK-NYUKSN-DAY(KSN-IDX IDX1)  =  ZERO
                      CONTINUE
                  ELSE
                      ADD  WRK-NYUKSN-ZAITEN(KSN-IDX)
                                 TO  LNK-SROUNYUIN-ZAITEN-NACCT(IDX1)
                  END-IF
               END-PERFORM
      *
           END-PERFORM
      *
      *
           .
       210-ROUNYUIN-SET2-EXT.
           EXIT.
      *
      *****************************************************************
      *    œ´∫“∆˛±°•µ•÷§Œ ÷µ—√Õ•ª•√•»°  ø¿Æ£≤£≤«Ø£¥∑Ó∞ πﬂ°À
      *****************************************************************
       210-ROUNYUIN-SET3-SEC            SECTION.
      *
      *œ´∫“æËøÙ§Œ£≤Ωµ¥÷∞ ∆‚§Œ∫«Ω™∆¸§ÚºË∆¿
           INITIALIZE                      ORCSRSITUSANAREA
           MOVE    LNK-SROUNYUIN-HOSPNUM
                                       TO  RSITUSAN-HOSPNUM
           MOVE    LNK-SROUNYUIN-PTID
                                       TO  RSITUSAN-PTID
           MOVE    LNK-SROUNYUIN-HKNCOMBI
                                       TO  RSITUSAN-HKNCOMBI
           MOVE    LNK-SROUNYUIN-SANTEIYM
                                       TO  RSITUSAN-SRYYM
           CALL    "ORCSRSITUSAN"      USING
                                           ORCSRSITUSANAREA
                                           SPA-AREA
           IF    ( RSITUSAN-RC          =   ZERO )
               CONTINUE
           ELSE
               INITIALIZE  ORCSRSITUSANAREA
           END-IF
           PERFORM   VARYING   IDX1   FROM 1  BY  1
                     UNTIL     IDX1   >    31
               MOVE  RSITUSAN-DAY(IDX1) TO LNK-SROUNYUIN-JOUSU-KBN(IDX1)
           END-PERFORM
      *
      *œ´∫“∆˛±°Œ¡§ŒºË∆¿
           MOVE    ZERO                    TO  FLG-OK
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-SROUNYUIN-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-SROUNYUIN-PTID      TO  NACCT-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO  NACCT-SRYYM
           MOVE    "1"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key17"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                   MOVE      ZERO     TO      FLG-RYAKU
                                              WRK-KSNKBN
                                              WRK-CDKBN-KBNNUM
                   MOVE      SPACE    TO      WRK-CHUKSNCD-XXX
                   MOVE      ZERO     TO      WRK-NYUKHNKBN-XXX
                   MOVE      SPACE    TO      WRK-RYAKUSHO
                                              WRK-SRYKBN
                                              WRK-SHOKIKSN-TAIHI
                                              WRK-HYOKETU-TAIHI
                   MOVE      ZERO     TO      WRK-KHNTEN
                                              WRK-KSNTEN
                                              WRK-HYOTEN
                   IF    NACCT-SRYCDTOTAL     =    99999916
                       CONTINUE
                   ELSE
      *                ≥∫≈ˆ∆˛±°π‘∞Ÿ•∆°º•÷•Î§Œ∆…§ﬂπ˛§ﬂ
                       PERFORM    210-NYUINACT-GET-SEC
      *                ∆˛±°¥À‹Œ¡§»ΩÈ¥¸≤√ªª° ƒπ¥¸∏∫ªª°À§Œ¬‡»Ú
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF    (LNK-SROUNYUIN-NYUIN-FLG(IDX1) = "1")
                           AND   (NACCT-DAY(IDX1)    NOT =  ZERO) 
                               MOVE    NSRY-SRYCD(1)   TO  
                                                   WRK-NYUINKHN(IDX1)
      *
                               IF   FLG-RYAKU   =   ZERO
      *
                                  IF   NSRY-SRYCD(1)  NOT =   SPACE
                                      INITIALIZE    TNS-TENSU-REC
                                      MOVE NSRY-SRYCD(1)
                                                TO  TNS-SRYCD
                                      MOVE LNK-SROUNYUIN-HOSPNUM
                                                TO  TNS-HOSPNUM
                                      MOVE LNK-SROUNYUIN-SANTEIYM
                                                TO  TNS-YUKOSTYMD(1:6)
                                      MOVE "01" TO  TNS-YUKOSTYMD(7:2)
                                      MOVE LNK-SROUNYUIN-SANTEIYM
                                                TO  TNS-YUKOEDYMD(1:6)
                                      MOVE "01" TO  TNS-YUKOEDYMD(7:2)
                                      PERFORM 910-TENSU-READ-SEC
                                      IF  FLG-TENSU  NOT = ZERO
                                        MOVE  1 TO  WRK-NYUINRYO-ERR
                                      ELSE
                                        MOVE TNS-TEN
                                                TO  WRK-KHNTEN
                                      END-IF
                                  END-IF
      *
                                  PERFORM  VARYING  IDX3  FROM  2  BY  1
                                            UNTIL   IDX3   >    3
                                    IF   NSRY-SRYCD(IDX3)  NOT =   SPACE
                                      INITIALIZE    TNS-TENSU-REC
                                      MOVE NSRY-SRYCD(IDX3)
                                                TO  TNS-SRYCD
                                      MOVE LNK-SROUNYUIN-HOSPNUM
                                                TO  TNS-HOSPNUM
                                      MOVE LNK-SROUNYUIN-SANTEIYM
                                                TO  TNS-YUKOSTYMD(1:6)
                                      MOVE "01" TO  TNS-YUKOSTYMD(7:2)
                                      MOVE LNK-SROUNYUIN-SANTEIYM
                                                TO  TNS-YUKOEDYMD(1:6)
                                      MOVE "01" TO  TNS-YUKOEDYMD(7:2)
                                      PERFORM 910-TENSU-READ-SEC
                                      EVALUATE  TNS-TENSIKIBETU
                                         WHEN     3
                                             MOVE   NSRY-SRYCD(IDX3)  TO
                                                      WRK-SHOKIKSN-TAIHI
                                             MOVE     TNS-TEN         TO
                                                      WRK-KSNTEN
                                         WHEN     8
                                             MOVE   NSRY-SRYCD(IDX3)  TO
                                                      WRK-SHOKIKSN-TAIHI
                                             COMPUTE  WRK-KSNTEN =
                                                      TNS-TEN    *  -1
                                         WHEN     6
                                             MOVE   NSRY-SRYCD(IDX3)  TO
                                                      WRK-HYOKETU-TAIHI
                                             MOVE     TNS-TEN         TO
                                                      WRK-HYOTEN
                                      END-EVALUATE
                                    END-IF
                                  END-PERFORM
      *
                                  MOVE    NACCT-SRYKBN
                                                TO  WRK-SRYKBN
      *                           ≥∞«Ò•≥°º•… ‘Ω∏Õ—§À∆˛±°¥À‹Œ¡≤√ªª∂Ë ¨
      *                           §Œ¬‡»Ú
                                  PERFORM   210-GAIHAKU-MAE-SEC
      *
                                  MOVE   NSRY-SRYCD(1)
                                                TO  WRK-RYAKU-SRYCD
                                  MOVE   NACCT-ZAISKBKBN
                                                TO  WRK-ZAISKBKBN
                                  PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                                  MOVE    1     TO    FLG-RYAKU
      *
      *                           ∆˛±°Œ¡∫ﬁ≈¿•¡•ß•√•Ø
                                  COMPUTE  WRK-HIKAKU-ZAITEN =
                                           WRK-KHNTEN + WRK-KSNTEN
                                  IF  WRK-HYOTEN  NOT =  ZERO
                                    COMPUTE WRK-HIKAKU-ZAITEN =
                                          ( WRK-HIKAKU-ZAITEN -
                                           (WRK-HIKAKU-ZAITEN *
                                            (WRK-HYOTEN / 100)) )
                                                              + 0.5
                                  END-IF
                                  IF  NACCT-ZAITEN  NOT =  ZERO
                                    IF  NACCT-ZAITEN  =
                                                    WRK-HIKAKU-ZAITEN
                                      CONTINUE
                                    ELSE
                                      MOVE   1  TO  WRK-NYUINRYO-ERR2
                                    END-IF
                                  END-IF
      *
                               END-IF
      *
                               IF    WRK-SHOKIKSN-TAIHI  NOT = SPACE
                                  MOVE  WRK-SHOKIKSN-TAIHI
                                               TO  WRK-SHOKIKSN(IDX1)
                               END-IF
                               IF    WRK-HYOKETU-TAIHI   NOT = SPACE
                                  MOVE  WRK-HYOKETU-TAIHI
                                               TO  WRK-HYOKETU(IDX1)
                               END-IF
      *
                               MOVE  WRK-SRYKBN
                                         TO  LNK-SROUNYUIN-SRYKBN(IDX1)
                               MOVE  WRK-KSNKBN
                                         TO  WRK-NYUKHNKSNKBN(IDX1)
                               MOVE  WRK-CHUKSNCD-XXX
                                         TO  WRK-CHUKSNCD(IDX1)
                               MOVE  WRK-NYUKHNKBN-XXX
                                         TO  WRK-NYUKHNKBN(IDX1)
      *
                               IF    WRK-RYAKUSHO   NOT =  SPACE
                                  MOVE  WRK-RYAKUSHO
                                      TO  LNK-SROUNYUIN-RYAKUSYO(IDX1)
                               END-IF
      *
                               MOVE    1    TO   FLG-OK
                           END-IF
                       END-PERFORM
                   END-IF
                   MOVE   "tbl_nyuinacct"   TO  MCP-TABLE
                   MOVE   "key17"           TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
      ***      IF   FLG-OK     =   ZERO
      ***          MOVE     6      TO      LNK-SROUNYUIN-RC
      ***      END-IF
           ELSE
               MOVE     6      TO      LNK-SROUNYUIN-RC
           END-IF
      *    ∆˛±°≤Ò∑◊•Ø•Ì°º•∫
           MOVE     "tbl_nyuinacct"   TO  MCP-TABLE
           MOVE     "key17"           TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           IF   LNK-SROUNYUIN-RC    NOT =   ZERO
                   GO   TO    200-MAIN-EXT
           END-IF
      *
      *œ´∫“∆˛±°§Œœ´∫“æËøÙ° ¥¸¥÷≤√ªª°À§À¥ﬁ§·§Î√Ì≤√ªª§ŒºË∆¿
           PERFORM    210-CHUKSN-GET-SEC
      *
      *œ´∫“∆˛±°ª˛§Œ≥∞«ÒºË∆¿
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-SROUNYUIN-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-SROUNYUIN-PTID      TO  NACCT-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO  NACCT-SRYYM
           MOVE    "2"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key17"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
      *            ≥∞«Ò∂Ë ¨§Œ¬‡»Ú
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                              UNTIL     IDX1   >   31
      ****             IF    (WRK-NYUINKHN(IDX1) NOT = SPACE) 
                       IF     NACCT-DAY(IDX1)    NOT =  ZERO
                              PERFORM     210-GAIHAKU-SET-SEC
                       END-IF
                   END-PERFORM
                   MOVE   "tbl_nyuinacct"  TO  MCP-TABLE
                   MOVE   "key17"          TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    ∆˛±°≤Ò∑◊•Ø•Ì°º•∫
           MOVE     "tbl_nyuinacct"        TO  MCP-TABLE
           MOVE     "key17"                TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *œ´∫“∆˛±°ª˛§Œ∆˛±°Œ¡≤√ªªºË∆¿° ∆˛±°≤Ò∑◊§Ë§Í°À
           MOVE    ZERO                    TO  KSN-IDX
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-SROUNYUIN-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-SROUNYUIN-PTID      TO  NACCT-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO  NACCT-SRYYM
           MOVE    "6"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key17"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                   MOVE      ZERO     TO      FLG-RYAKU
                   MOVE      ZERO     TO      FLG-KSN
      *            ∆˛±°Œ¡≤√ªª§Œ¬‡»Ú
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                              UNTIL     IDX1   >   31
                       IF    ((WRK-NYUINKHN(IDX1) NOT = SPACE) OR
                              (WRK-NYUINDAY-KBN(IDX1) =  "1" ))
                       AND   ((LNK-SROUNYUIN-GAIHAKU(IDX1)
                                                      = ZERO ) OR
                              (LNK-SROUNYUIN-GAIHAKU(IDX1)
                                                     >=   4  ))
                       AND   ( NACCT-DAY(IDX1)    NOT =  ZERO )
      *
      *                  KSN1°ßÃÎ¥÷∂€µﬁ¬Œ¿©≥Œ ›≤√ªª
      *                  KSN12:∞Âª’«€√÷≤√ªª£±
      *                  KSN2°ß∞Âª’«€√÷≤√ªª£≤
      *                  KSN4°ß¥«∏Ó«€√÷≤√ªª£±
      *                  KSN3°ß¥«∏Ó«€√÷≤√ªª£≤
      *                  KSN6°ßÃÎ¥÷¥«∏Ó«€√÷≤√ªª£±
      *                  KSN5°ßÃÎ¥÷¥«∏Ó«€√÷≤√ªª£≤
      *                  KSN13:∞Ï»Ã…¬≈Ô¥«∏Ó…¨Õ◊≈Ÿ…æ≤¡≤√ªª
      *                  KSN16:∞Ï»Ã…¬≈Ô¥«∏Ó…¨Õ◊≈Ÿ…æ≤¡≤√ªª° ∆√ƒÍµ°«Ω…¬±°°À
      *                  KSN17:∞Ï»Ã…¬≈Ô¥«∏Ó…¨Õ◊≈Ÿ…æ≤¡≤√ªª° ¿ÏÃÁ…¬±°°À
      *                  KSN14:µﬂµﬁ°¶∫ﬂ¬≈˘ªŸ±ÁŒ≈Õ‹…¬æ≤ΩÈ¥¸≤√ªª
      *                  KSN18:Õ≠æ≤ø«Œ≈ΩÍ∞Ï»Ã…¬æ≤ΩÈ¥¸≤√ªª
      *                  KSN19:µﬂµﬁ°¶∫ﬂ¬≈˘ªŸ±ÁŒ≈Õ‹…¬æ≤ΩÈ¥¸≤√ªª
                         IF (NACCT-SRYCDTOTAL(6:9) = CONS-200804-KSN1
                                                  OR CONS-201004-KSN12
                                                  OR CONS-200804-KSN2
                                                  OR CONS-200804-KSN4
                                                  OR CONS-200804-KSN3
                                                  OR CONS-200804-KSN6
                                                  OR CONS-200804-KSN5
                                                  OR CONS-201004-KSN13
                                                  OR CONS-201004-KSN16
                                                  OR CONS-201004-KSN17)
                          IF  (WRK-NYUINKHN(IDX1)      NOT = SPACE)
                          AND ((LNK-SROUNYUIN-GAIHAKU(IDX1)
                                                           = ZERO) OR
                               (LNK-SROUNYUIN-GAIHAKU(IDX1)
                                                          >=   4 ))
                           EVALUATE   NACCT-SRYCDTOTAL(6:9)
                             WHEN  CONS-200804-KSN1
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-200804-KSN1(IDX1)
                             WHEN  CONS-201004-KSN12
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201004-KSN12(IDX1)
                             WHEN  CONS-200804-KSN2
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-200804-KSN2(IDX1)
                             WHEN  CONS-200804-KSN4
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-200804-KSN4(IDX1)
                             WHEN  CONS-200804-KSN3
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-200804-KSN3(IDX1)
                             WHEN  CONS-200804-KSN6
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-200804-KSN6(IDX1)
                             WHEN  CONS-200804-KSN5
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-200804-KSN5(IDX1)
                             WHEN  CONS-201004-KSN13
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201004-KSN13(IDX1)
                             WHEN  CONS-201004-KSN16
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201004-KSN16(IDX1)
                             WHEN  CONS-201004-KSN17
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201004-KSN17(IDX1)
                           END-EVALUATE
      *
                           IF   FLG-RYAKU   =   ZERO
                              MOVE   NACCT-SRYCDTOTAL(6:9)
                                            TO  WRK-RYAKU-SRYCD
                              MOVE   NACCT-ZAISKBKBN
                                            TO  WRK-ZAISKBKBN
                              PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                              MOVE    1     TO    FLG-RYAKU
                           END-IF
                          END-IF
                         ELSE
                           IF   FLG-KSN   =  ZERO
                               ADD     1    TO    KSN-IDX
                               MOVE    1    TO    FLG-KSN
                           END-IF
                           MOVE   NACCT-SRYCDTOTAL(6:9)
                                        TO  WRK-NYUKSN(KSN-IDX)
                           MOVE   NACCT-ZAITEN  
                                        TO  WRK-NYUKSN-ZAITEN(KSN-IDX)
                           MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-NYUKSN-DAY(KSN-IDX IDX1)
      *
                           IF   FLG-RYAKU   =   ZERO
                              MOVE   NACCT-SRYCDTOTAL(6:9)
                                            TO  WRK-RYAKU-SRYCD
                              MOVE   NACCT-ZAISKBKBN
                                            TO  WRK-ZAISKBKBN
                              PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                              MOVE    1     TO    FLG-RYAKU
                           END-IF
                         END-IF
      *
                       END-IF
                   END-PERFORM
                   MOVE   "tbl_nyuinacct"   TO  MCP-TABLE
                   MOVE   "key17"           TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    ∆˛±°≤Ò∑◊•Ø•Ì°º•∫
           MOVE     "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE     "key17"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *
      *œ´∫“∆˛±°ª˛§Œ∆˛±°Œ¡≤√ªªºË∆¿° ø«Œ≈≤Ò∑◊§Ë§Í°À
           INITIALIZE                           SRYACCT-REC
           MOVE    LNK-SROUNYUIN-HOSPNUM   TO   ACCT-HOSPNUM
           MOVE    "1"                     TO   ACCT-NYUGAIKBN
           MOVE    LNK-SROUNYUIN-PTID      TO   ACCT-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO   ACCT-SRYYM
           MOVE    LNK-SROUNYUIN-HKNCOMBI  TO   ACCT-HKNCOMBI
           MOVE    "tbl_sryacct"           TO   MCP-TABLE
           MOVE    "key22"                 TO   MCP-PATHNAME
           PERFORM 910-SRYACCT-SELECT-SEC
           IF      FLG-SRYACCT           =   ZERO
               MOVE     "tbl_sryacct"      TO   MCP-TABLE
               MOVE     "key22"            TO   MCP-PATHNAME
               PERFORM   910-SRYACCT-READ-SEC
               PERFORM   UNTIL   (FLG-SRYACCT    =   1)
                 MOVE      ZERO     TO      FLG-RYAKU
                 IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                      (ACCT-ZAIKAISU  NOT =    ZERO)    AND
                      (ACCT-JIHOKBN       =   SPACE)
                   IF   ACCT-SRYKBN  =  "90" OR "92"
      *
                     INITIALIZE                         SRYACT-REC
                     MOVE    ACCT-HOSPNUM       TO      SRY-HOSPNUM
                     MOVE    ACCT-NYUGAIKBN     TO      SRY-NYUGAIKBN
                     MOVE    ACCT-PTID          TO      SRY-PTID
                     MOVE    ACCT-SRYYM         TO      SRY-SRYYM
                     MOVE    ACCT-ZAINUM        TO      SRY-ZAINUM
                     MOVE    SRYACT-REC         TO      MCPDATA-REC
                     MOVE    "DBSELECT"         TO      MCP-FUNC
                     MOVE    "tbl_sryact"       TO      MCP-TABLE
                     MOVE    "key9"             TO      MCP-PATHNAME
                     CALL    "ORCDBMAIN"        USING   MCPAREA
                                                        MCPDATA-REC
                                                        SPA-AREA
                     IF      MCP-RC               =       ZERO
                         MOVE    "tbl_sryact"   TO      MCP-TABLE
                         MOVE    "key9"         TO      MCP-PATHNAME
                         PERFORM  910-SRYACT-READ-SEC
                         IF   FLG-SRYACT    =    0
      *                     KSN7°ßΩ≈≈Ÿ«ß√Œæ…≤√ªª
      *                     KSN8°ßÍÛ·Ï…æ≤¡º¬ª‹≤√ªª° Œ≈Õ‹…¬≈Ô°À
      *                     KSN9°ßÍÛ·Ï…æ≤¡º¬ª‹≤√ªª° Õ≠æ≤ø«Œ≈ΩÍ°À
      *                     KSN11:Ω≈æ…ª˘° º‘°Àºı∆˛œ¢∑»≤√ªª° æ„≥≤º‘ª‹¿ﬂ≈˘∆˛±°¥À‹Œ¡°À
      *                     KSN15:Ω≈≈Ÿ«ß√Œæ…≤√ªª° ¿∫ø¿…¬≈Ô∆˛±°¥À‹Œ¡°À
      *                     °›°›∞ ≤º°¢∆˛±°≤Ò∑◊§»ø«Œ≈≤Ò∑◊§´§ÈºË∆¿§π§Î≤√ªª°›°›
      *                     KSN1°ßÃÎ¥÷∂€µﬁ¬Œ¿©≥Œ ›≤√ªª
      *                     KSN12:∞Âª’«€√÷≤√ªª£±
      *                     KSN2°ß∞Âª’«€√÷≤√ªª£≤
      *                     KSN4°ß¥«∏Ó«€√÷≤√ªª£±
      *                     KSN3°ß¥«∏Ó«€√÷≤√ªª£≤
      *                     KSN6°ßÃÎ¥÷¥«∏Ó«€√÷≤√ªª£±
      *                     KSN5°ßÃÎ¥÷¥«∏Ó«€√÷≤√ªª£≤
      *                     KSN13:∞Ï»Ã…¬≈Ô¥«∏Ó…¨Õ◊≈Ÿ…æ≤¡≤√ªª
      *                     KSN16:∞Ï»Ã…¬≈Ô¥«∏Ó…¨Õ◊≈Ÿ…æ≤¡≤√ªª° ∆√ƒÍµ°«Ω…¬±°°À
      *                     KSN17:∞Ï»Ã…¬≈Ô¥«∏Ó…¨Õ◊≈Ÿ…æ≤¡≤√ªª° ¿ÏÃÁ…¬±°°À
      *                     KSN14:µﬂµﬁ°¶∫ﬂ¬≈˘ªŸ±ÁŒ≈Õ‹…¬æ≤ΩÈ¥¸≤√ªª
      *                     KSN18:Õ≠æ≤ø«Œ≈ΩÍ∞Ï»Ã…¬æ≤ΩÈ¥¸≤√ªª
      *                     KSN19:µﬂµﬁ°¶∫ﬂ¬≈˘ªŸ±ÁŒ≈Õ‹…¬æ≤ΩÈ¥¸≤√ªª
                            IF (SRY-SRYCD(1)  =  CONS-200804-KSN7
                                              OR CONS-200804-KSN8
                                              OR CONS-200804-KSN9
                                              OR CONS-201004-KSN13
                                              OR CONS-201004-KSN15
                                              OR CONS-201004-KSN16
                                              OR CONS-201004-KSN17
                                              OR CONS-200804-KSN1
                                              OR CONS-201004-KSN12
                                              OR CONS-200804-KSN2
                                              OR CONS-200804-KSN4
                                              OR CONS-200804-KSN3
                                              OR CONS-200804-KSN6
                                              OR CONS-200804-KSN5 )
                                MOVE  SRY-SRYCD(1)   TO
                                               ACCT-SRYCDTOTAL(6:9)
                            END-IF
                         END-IF
                     END-IF
      *              ø«Œ≈π‘∞Ÿ•Ø•Ì°º•∫
                     MOVE     "tbl_sryact"    TO    MCP-TABLE
                     MOVE     "key9"          TO    MCP-PATHNAME
                     PERFORM   910-DB-CLOSE-SEC
      *
                     IF ( ACCT-SRYCDTOTAL(6:9)  =
                                            CONS-200804-KSN7
                                        OR  CONS-200804-KSN8
                                        OR  CONS-200804-KSN9
                                        OR  CONS-201004-KSN13
                                        OR  CONS-201004-KSN15
                                        OR  CONS-201004-KSN16
                                        OR  CONS-201004-KSN17
                                        OR  CONS-200804-KSN1
                                        OR  CONS-201004-KSN12
                                        OR  CONS-200804-KSN2
                                        OR  CONS-200804-KSN4
                                        OR  CONS-200804-KSN3
                                        OR  CONS-200804-KSN6
                                        OR  CONS-200804-KSN5  )
      *                ∆˛±°Œ¡≤√ªª§Œ¬‡»Ú
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF    (WRK-NYUINKHN(IDX1) NOT = SPACE)
                           AND   ((LNK-SROUNYUIN-GAIHAKU(IDX1)
                                                         = ZERO) OR
                                  (LNK-SROUNYUIN-GAIHAKU(IDX1)
                                                        >=   4 ))
                           AND   (ACCT-DAY(1 IDX1)   NOT =  ZERO)
                              EVALUATE   ACCT-SRYCDTOTAL(6:9)
      *                         Ω≈≈Ÿ«ß√Œæ…≤√ªª
                                WHEN  CONS-200804-KSN7
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-200804-KSN7(IDX1)
      *                         ÍÛ·Ï…æ≤¡º¬ª‹≤√ªª° Œ≈Õ‹…¬≈Ô°À
                                WHEN  CONS-200804-KSN8
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-200804-KSN8(IDX1)
      *                         ÍÛ·Ï…æ≤¡º¬ª‹≤√ªª° Õ≠æ≤ø«Œ≈ΩÍ°À
                                WHEN  CONS-200804-KSN9
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-200804-KSN9(IDX1)
      *                         ∞Ï»Ã…¬≈Ô¥«∏Ó…¨Õ◊≈Ÿ…æ≤¡≤√ªª
                                WHEN  CONS-201004-KSN13
                                  IF  ACCT-DAY(1 IDX1)   NOT = ZERO
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201004-KSN13(IDX1)
                                  END-IF
      *                         Ω≈≈Ÿ«ß√Œæ…≤√ªª° ¿∫ø¿…¬≈Ô∆˛±°¥À‹Œ¡°À
                                WHEN  CONS-201004-KSN15
                                  MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201004-KSN15(IDX1)
      *                         ∞Ï»Ã…¬≈Ô¥«∏Ó…¨Õ◊≈Ÿ…æ≤¡≤√ªª° ∆√ƒÍµ°«Ω…¬±°°À
                                WHEN  CONS-201004-KSN16
                                  IF  ACCT-DAY(1 IDX1)   NOT = ZERO
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201004-KSN16(IDX1)
                                  END-IF
      *                         ∞Ï»Ã…¬≈Ô¥«∏Ó…¨Õ◊≈Ÿ…æ≤¡≤√ªª° ¿ÏÃÁ…¬±°°À
                                WHEN  CONS-201004-KSN17
                                  IF  ACCT-DAY(1 IDX1)   NOT = ZERO
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201004-KSN17(IDX1)
                                  END-IF
      *                         ÃÎ¥÷∂€µﬁ¬Œ¿©≥Œ ›≤√ªª
                                WHEN  CONS-200804-KSN1
                                  IF  ACCT-DAY(1 IDX1)   NOT = ZERO
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-200804-KSN1(IDX1)
                                  END-IF
      *                         ∞Âª’«€√÷≤√ªª£±
                                WHEN  CONS-201004-KSN12
                                  IF  ACCT-DAY(1 IDX1)   NOT = ZERO
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201004-KSN12(IDX1)
                                  END-IF
      *                         ∞Âª’«€√÷≤√ªª£≤
                                WHEN  CONS-200804-KSN2
                                  IF  ACCT-DAY(1 IDX1)   NOT = ZERO
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-200804-KSN2(IDX1)
                                  END-IF
      *                         ¥«∏Ó«€√÷≤√ªª£±
                                WHEN  CONS-200804-KSN4
                                  IF  ACCT-DAY(1 IDX1)   NOT = ZERO
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-200804-KSN4(IDX1)
                                  END-IF
      *                         ¥«∏Ó«€√÷≤√ªª£≤
                                WHEN  CONS-200804-KSN3
                                  IF  ACCT-DAY(1 IDX1)   NOT = ZERO
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-200804-KSN3(IDX1)
                                  END-IF
      *                         ÃÎ¥÷¥«∏Ó«€√÷≤√ªª£±
                                WHEN  CONS-200804-KSN6
                                  IF  ACCT-DAY(1 IDX1)   NOT = ZERO
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-200804-KSN6(IDX1)
                                  END-IF
      *                         ÃÎ¥÷¥«∏Ó«€√÷≤√ªª£≤
                                WHEN  CONS-200804-KSN5
                                  IF  ACCT-DAY(1 IDX1)   NOT = ZERO
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-200804-KSN5(IDX1)
                                  END-IF
                              END-EVALUATE
      *
                              IF   FLG-RYAKU   =   ZERO
                                 MOVE   ACCT-SRYCDTOTAL(6:9)
                                               TO  WRK-RYAKU-SRYCD
                                 MOVE   "6"    TO  WRK-ZAISKBKBN
                                 PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                                 MOVE    1     TO    FLG-RYAKU
                              END-IF
      *
                           END-IF
                       END-PERFORM
                     ELSE
                       IF  (ACCT-SRYCDTOTAL(6:9)  =  CONS-ROSAICHIRYO
                                                 OR  CONS-BYOUI
                                                 OR  CONS-TOKUKANSI)
                       OR  (ACCT-ZAIKBN           =    2           )
      *                  ∆˛±°Œ¡≤√ªª§Œ¬‡»Ú
                        IF  ACCT-ZAIKBN  =  2
                         PERFORM    VARYING   IDX1   FROM   1   BY   1
                                    UNTIL     IDX1   >   31
                             IF    ((WRK-NYUINKHN(IDX1) NOT = SPACE) OR
                                    (WRK-NYUINDAY-KBN(IDX1) =  "1" ))
                             AND   ( ACCT-DAY(1 IDX1)   NOT =  ZERO )
                               COMPUTE LNK-SROUNYUIN-GENTEN(IDX1)
                                    =  LNK-SROUNYUIN-GENTEN(IDX1)
                                    + (ACCT-ZAITEN * ACCT-DAY(1 IDX1))
                             END-IF
                         END-PERFORM
                        ELSE
                         PERFORM    VARYING   IDX1   FROM   1   BY   1
                                    UNTIL     IDX1   >   31
      **>                    IF    ((WRK-NYUINKHN(IDX1) NOT = SPACE) OR
      **>                           (WRK-NYUINDAY-KBN(IDX1) =  "1" ))
      **>                    AND   ( ACCT-DAY(1 IDX1)   NOT =  ZERO )
                             IF    ( ACCT-DAY(1 IDX1)   NOT =  ZERO )
                               COMPUTE LNK-SROUNYUIN-ZAITEN-ACCTETC
                                                             (IDX1)
                                    =  LNK-SROUNYUIN-ZAITEN-ACCTETC
                                                             (IDX1)
                                    + (ACCT-ZAITEN * ACCT-DAY(1 IDX1))
                               COMPUTE LNK-SROUNYUIN-ZAITEN-ACCTETC2
                                                             (IDX1)
                                    =  LNK-SROUNYUIN-ZAITEN-ACCTETC2
                                                             (IDX1)
                                    + (ACCT-ZAITEN * ACCT-DAY(2 IDX1))
                               COMPUTE LNK-SROUNYUIN-ZAITEN-ACCTETC3
                                                             (IDX1)
                                    =  LNK-SROUNYUIN-ZAITEN-ACCTETC3
                                                             (IDX1)
                                    + (ACCT-ZAITEN * ACCT-DAY(3 IDX1))
      *
                               IF   FLG-RYAKU   =   ZERO
                                  MOVE   ACCT-SRYCDTOTAL(6:9)
                                                TO  WRK-RYAKU-SRYCD
                                  MOVE   "6"    TO  WRK-ZAISKBKBN
                                  PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                                  MOVE    1     TO    FLG-RYAKU
                               END-IF
      *
                             END-IF
                         END-PERFORM
                        END-IF
                       ELSE
      *                  ∆˛±°Œ¡≤√ªª§Œ¬‡»Ú
                         PERFORM    VARYING   IDX1   FROM   1   BY   1
                                    UNTIL     IDX1   >   31
                             IF    ((WRK-NYUINKHN(IDX1) NOT = SPACE) OR
                                    (WRK-NYUINDAY-KBN(IDX1) =  "1" ))
                             AND   ( ACCT-DAY(1 IDX1)   NOT =  ZERO )
                               COMPUTE LNK-SROUNYUIN-ZAITEN-ACCT (IDX1)
                                    =  LNK-SROUNYUIN-ZAITEN-ACCT (IDX1)
                                    + (ACCT-ZAITEN * ACCT-DAY(1 IDX1))
                               COMPUTE LNK-SROUNYUIN-ZAITEN-ACCT2(IDX1)
                                    =  LNK-SROUNYUIN-ZAITEN-ACCT2(IDX1)
                                    + (ACCT-ZAITEN * ACCT-DAY(2 IDX1))
                               COMPUTE LNK-SROUNYUIN-ZAITEN-ACCT3(IDX1)
                                    =  LNK-SROUNYUIN-ZAITEN-ACCT3(IDX1)
                                    + (ACCT-ZAITEN * ACCT-DAY(3 IDX1))
      *
                               IF   FLG-RYAKU   =   ZERO
                                  PERFORM   210-SRYACT-HENSYU-SEC
      *
                                  MOVE    1     TO    FLG-RYAKU
                               END-IF
      *
                             END-IF
                         END-PERFORM
                       END-IF
                     END-IF
                   END-IF
                 END-IF
                 MOVE     "tbl_sryacct"         TO    MCP-TABLE
                 MOVE     "key22"               TO    MCP-PATHNAME
                 PERFORM   910-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *    ø«Œ≈≤Ò∑◊•Ø•Ì°º•∫
           MOVE     "tbl_sryacct"         TO    MCP-TABLE
           MOVE     "key22"               TO    MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *
      *    ºË∆¿§∑§øœ´∫“∆˛±°æ Û§´§È∆¸√±∞Ã§Œ∆˛±°≤Ò∑◊æ Û§Ú∫∆πΩ√€
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
               MOVE     ZERO                TO  WRK-MEISAISU
      *        ∆˛±°¥À‹Œ¡
               IF         WRK-NYUINKHN(IDX1)  NOT =        SPACE
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-NYUINKHN(IDX1) TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "1"           TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ≥∞«Ò
               IF        (WRK-NYUINKHN(IDX1)  NOT =        SPACE)
               AND       (WRK-GAIHAKU(IDX1)   NOT =        SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-GAIHAKU(IDX1)  TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "2"           TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ÃÎ¥÷∂€µﬁ¬Œ¿©≥Œ ›≤√ªª
               IF         WRK-200804-KSN1(IDX1)   NOT =   ZERO
                   ADD      1                 TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN1  TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"            TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ∞Âª’«€√÷≤√ªª£±
               IF         WRK-201004-KSN12(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-201004-KSN12    TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ∞Âª’«€√÷≤√ªª£≤
               IF         WRK-200804-KSN2(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN2     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ¥«∏Ó«€√÷≤√ªª£±
               IF         WRK-200804-KSN4(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN4     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ¥«∏Ó«€√÷≤√ªª£≤
               IF         WRK-200804-KSN3(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN3     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ÃÎ¥÷¥«∏Ó«€√÷≤√ªª£±
               IF         WRK-200804-KSN6(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN6     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ÃÎ¥÷¥«∏Ó«€√÷≤√ªª£≤
               IF         WRK-200804-KSN5(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN5     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        Ω≈≈Ÿ«ß√Œæ…≤√ªª° ∆√ƒÍµ°«Ω…¬±°¿∫ø¿…¬≈Ô°À
               IF         WRK-200804-KSN7(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN7     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ÍÛ·Ï…æ≤¡º¬ª‹≤√ªª° Œ≈Õ‹…¬≈Ô°À
               IF         WRK-200804-KSN8(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN8     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ÍÛ·Ï…æ≤¡º¬ª‹≤√ªª° Õ≠æ≤ø«Œ≈ΩÍ°À
               IF         WRK-200804-KSN9(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN9     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ∞Ï»Ã…¬≈Ô¥«∏Ó…¨Õ◊≈Ÿ…æ≤¡≤√ªª
               IF         WRK-201004-KSN13(IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-201004-KSN13    TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        Ω≈≈Ÿ«ß√Œæ…≤√ªª° ¿∫ø¿…¬≈Ô∆˛±°¥À‹Œ¡°À
               IF         WRK-201004-KSN15(IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-201004-KSN15    TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ∞Ï»Ã…¬≈Ô¥«∏Ó…¨Õ◊≈Ÿ…æ≤¡≤√ªª° ∆√ƒÍµ°«Ω…¬±°°À
               IF         WRK-201004-KSN16(IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-201004-KSN16    TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ∞Ï»Ã…¬≈Ô¥«∏Ó…¨Õ◊≈Ÿ…æ≤¡≤√ªª° ¿ÏÃÁ…¬±°°À
               IF         WRK-201004-KSN17(IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-201004-KSN17    TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *
               IF      (WRK-NYUINKHN(IDX1)   NOT =         SPACE)  AND
                       (WRK-GAIHAKU(IDX1)        =         SPACE)  AND
                       (WRK-GENSAN-IRYO(IDX1)   NOT =      SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-GENSAN3       TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"          TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                   MOVE     1               TO  WRK-GENSAN3-MIJISI
               END-IF
               IF      (WRK-NYUINKHN(IDX1)   NOT =         SPACE)  AND
                       (WRK-GAIHAKU(IDX1)        =         SPACE)  AND
                       (WRK-GENSAN-JYOKU(IDX1)   NOT =     SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-GENSAN4       TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"          TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                   MOVE     1               TO  WRK-GENSAN4-MIJISI
               END-IF
      *        œ´∫“æËøÙ° ∆√ƒÍ∆˛±°Œ¡§ŒæÏπÁ§œœ´∫“æËøÙ§ §∑°À
               IF       LNK-SROUNYUIN-SRYKBN(IDX1)    =    "90"
                 IF     FLG-KENPOJYUNKYO  =  1
                   CONTINUE
                 ELSE
                   EVALUATE  LNK-SROUNYUIN-JOUSU-KBN(IDX1)
                       WHEN  "1"
                           ADD      1           TO  WRK-MEISAISU
                           MOVE   "101900010"   TO  LNK-SROUNYUIN-SRYCD
                                               (IDX1 WRK-MEISAISU)
                           MOVE    "4"      TO  LNK-SROUNYUIN-SRYCD-SK
                                               (IDX1 WRK-MEISAISU)
                       WHEN  "2"
                           ADD      1           TO  WRK-MEISAISU
                           MOVE   "101900020"   TO  LNK-SROUNYUIN-SRYCD
                                               (IDX1 WRK-MEISAISU)
                           MOVE    "4"      TO  LNK-SROUNYUIN-SRYCD-SK
                                               (IDX1 WRK-MEISAISU)
                   END-EVALUATE
                 END-IF
               END-IF
      *        ΩÈ¥¸≤√ªª° ƒπ¥¸∏∫ªª°À
               IF      (WRK-GAIHAKU(IDX1)        =
                                     SPACE      OR
                                    "190142190" OR  "190142090" OR
                                    "190142290" OR  "190141590" OR
                                    "190108790" OR  "190108890" OR
                                    "190109090" OR
                                    "190153190" OR  "190153090" OR
                                    "190153390" OR  "190153290" OR
                                    "190161470" OR  "190161570" OR
                                    "190161870" OR  "190162070" OR
                                    "190161670" OR  "190161770" OR
                                    "190161970" OR  "190162170" )
               AND     (WRK-SHOKIKSN(IDX1)   NOT =         SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-SHOKIKSN(IDX1) TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "5"           TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        …∏∑Á
               IF       WRK-HYOKETU(IDX1)    NOT =         SPACE
                 IF    (WRK-GAIHAKU(IDX1)        =
                                     SPACE      OR
                                    "190142190" OR  "190142090" OR
                                    "190142290" OR  "190141590" OR
                                    "190108790" OR  "190108890" OR
                                    "190109090" OR
                                    "190153190" OR  "190153090" OR
                                    "190153390" OR  "190153290" OR
                                    "190161470" OR  "190161570" OR
                                    "190161870" OR  "190162070" OR
                                    "190161670" OR  "190161770" OR
                                    "190161970" OR  "190162170" )
                 AND   (WRK-NYUINKHN(IDX1)   NOT =       SPACE  )
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-HYOKETU(IDX1)  TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "6"           TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                 ELSE
                   MOVE   SPACE             TO  WRK-HYOKETU(IDX1)
                 END-IF
               END-IF
      *        ∆˛±°Œ¡≤√ªª° ∆˛±°≤Ò∑◊°À
      *         PERFORM   VARYING   KSN-IDX   FROM  1  BY  1
      *                   UNTIL    (WRK-NYUKSN(KSN-IDX)  =  SPACE)  OR
      *                            (KSN-IDX             >  20)
      *            IF    WRK-NYUKSN-DAY(KSN-IDX IDX1)  =  ZERO
      *                CONTINUE
      *            ELSE
      *                ADD     1           TO  WRK-MEISAISU
      *                MOVE WRK-NYUKSN(KSN-IDX)
      *                                    TO  LNK-SROUNYUIN-SRYCD
      *                                   (IDX1 WRK-MEISAISU)
      *            END-IF
      *         END-PERFORM
           END-PERFORM
      *
      *    ∫∆πΩ√€§∑§øœ´∫“∆˛±°≤Ò∑◊æ Û§´§È≈¿øÙ§ŒªªƒÍ
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
      *
               MOVE     ZERO       TO       WRK-HYOKETU-TEN
                                            WRK-ZAITEN-HYOKETU
                                            WRK-ZAITEN-HYOKETU2
               IF   WRK-HYOKETU(IDX1)   =   SPACE
      *          ≈¿øÙ•ﬁ•π•ø§Œ∆…§ﬂπ˛§ﬂ
                 PERFORM   VARYING   IDX2    FROM   1  BY   1
                      UNTIL (LNK-SROUNYUIN-SRYCD(IDX1 IDX2) = SPACE)
                         OR (IDX2    >      20)
                     PERFORM    210-TENSU-KEISAN-SEC
                 END-PERFORM
               ELSE
      *          ≈¿øÙ•ﬁ•π•ø§Œ∆…§ﬂπ˛§ﬂ
                 INITIALIZE                          TNS-TENSU-REC
                 MOVE    WRK-HYOKETU(IDX1)       TO  TNS-SRYCD
                 MOVE    LNK-SROUNYUIN-HOSPNUM   TO  TNS-HOSPNUM
                 MOVE    LNK-SROUNYUIN-SANTEIYM  TO  TNS-YUKOSTYMD(1:6)
                 MOVE    "01"                    TO  TNS-YUKOSTYMD(7:2)
                 MOVE    LNK-SROUNYUIN-SANTEIYM  TO  TNS-YUKOEDYMD(1:6)
                 MOVE    "01"                    TO  TNS-YUKOEDYMD(7:2)
                 PERFORM 910-TENSU-READ-SEC
                 MOVE    TNS-TEN                 TO  WRK-HYOKETU-TEN
      *
                 PERFORM   VARYING   IDX2    FROM   1  BY   1
                      UNTIL (LNK-SROUNYUIN-SRYCD(IDX1 IDX2) = SPACE)
                         OR (IDX2    >      20)
                     PERFORM    210-TENSU-KEISAN-HYOKETU-SEC
                 END-PERFORM
               END-IF
      *
               PERFORM   VARYING   KSN-IDX   FROM   1  BY   1
                         UNTIL (WRK-NYUKSN(KSN-IDX)  =  SPACE)  OR
                               (KSN-IDX    >      20)
                  IF    WRK-NYUKSN-DAY(KSN-IDX IDX1)  =  ZERO
                      CONTINUE
                  ELSE
                      ADD  WRK-NYUKSN-ZAITEN(KSN-IDX)
                                 TO  LNK-SROUNYUIN-ZAITEN-NACCT(IDX1)
                  END-IF
               END-PERFORM
      *
           END-PERFORM
      *
      *
           .
       210-ROUNYUIN-SET3-EXT.
           EXIT.
      *
      *****************************************************************
      *    œ´∫“∆˛±°•µ•÷§Œ ÷µ—√Õ•ª•√•»°  ø¿Æ£≤£¥«Ø£¥∑Ó∞ πﬂ°À
      *****************************************************************
       210-ROUNYUIN-SET4-SEC            SECTION.
      *
           IF      LNK-SROUNYUIN-SANTEIYM   >=   "201404"
           AND     LNK-SROUNYUIN-SANTEIYM   <=   "201406"
              PERFORM  210-SYSKAN-HENSYU-SEC
           END-IF
      *
      *ÃÎ¥÷¥«∏Ó¬Œ¿©∆√ƒÍ∆¸∏∫ªªºË∆¿
           IF      LNK-SROUNYUIN-SANTEIYM   >=   "201804"
             INITIALIZE                         ORCSKANGOGEN-AREA
             MOVE    LNK-SROUNYUIN-HOSPNUM  TO  LNK-KANGOGEN-HOSPNUM
             MOVE    LNK-SROUNYUIN-PTID     TO  LNK-KANGOGEN-PTID
             MOVE    LNK-SROUNYUIN-SANTEIYM TO  LNK-KANGOGEN-SANTEIYM
             CALL    "ORCSKANGOGEN"         USING  ORCSKANGOGEN-AREA
                                                   SPA-AREA
             IF      LNK-KANGOGEN-RC  =   ZERO
               PERFORM    VARYING   IDX1  FROM   1   BY   1
                           UNTIL    IDX1   >    31
                 MOVE  LNK-KANGOGEN-SRYCD(IDX1) TO  WRK-KANGOGEN(IDX1)
               END-PERFORM
             END-IF
           END-IF
      *
      *œ´∫“æËøÙ§Œ£≤Ωµ¥÷∞ ∆‚§Œ∫«Ω™∆¸§ÚºË∆¿
           INITIALIZE                      ORCSRSITUSANAREA
           MOVE    LNK-SROUNYUIN-HOSPNUM
                                       TO  RSITUSAN-HOSPNUM
           MOVE    LNK-SROUNYUIN-PTID
                                       TO  RSITUSAN-PTID
           MOVE    LNK-SROUNYUIN-HKNCOMBI
                                       TO  RSITUSAN-HKNCOMBI
           MOVE    LNK-SROUNYUIN-SANTEIYM
                                       TO  RSITUSAN-SRYYM
           CALL    "ORCSRSITUSAN"      USING
                                           ORCSRSITUSANAREA
                                           SPA-AREA
           IF    ( RSITUSAN-RC          =   ZERO )
               CONTINUE
           ELSE
               INITIALIZE  ORCSRSITUSANAREA
           END-IF
           PERFORM   VARYING   IDX1   FROM 1  BY  1
                     UNTIL     IDX1   >    31
               MOVE  RSITUSAN-DAY(IDX1) TO LNK-SROUNYUIN-JOUSU-KBN(IDX1)
           END-PERFORM
      *
      *œ´∫“∆˛±°Œ¡§ŒºË∆¿
           MOVE    ZERO                    TO  FLG-OK
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-SROUNYUIN-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-SROUNYUIN-PTID      TO  NACCT-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO  NACCT-SRYYM
           MOVE    "1"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key17"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                   MOVE      ZERO     TO      FLG-RYAKU
                                              WRK-KSNKBN
                                              WRK-CDKBN-KBNNUM
                   MOVE      SPACE    TO      WRK-CHUKSNCD-XXX
                   MOVE      ZERO     TO      WRK-NYUKHNKBN-XXX
                   MOVE      SPACE    TO      WRK-RYAKUSHO
                                              WRK-SRYKBN
                                              WRK-SHOKIKSN-TAIHI
                                              WRK-HYOKETU-TAIHI
                   MOVE      ZERO     TO      WRK-KHNTEN
                                              WRK-KSNTEN
                                              WRK-HYOTEN
                   MOVE      SPACE    TO      WRK-SENTEI-SRYCD1
                                              WRK-SENTEI-SRYCD2
                   IF    NACCT-SRYCDTOTAL     =    99999916
      *                ¬Ëª∞º‘π‘∞Ÿ§Œ¡™ƒÍ∆˛±°Œ¡ ‘Ω∏
                       IF   LNK-SROUNYUIN-DAISANFLG  =  "1"
                         PERFORM    VARYING   IDX1   FROM   1   BY   1
                                    UNTIL     IDX1   >   31
                           IF    (LNK-SROUNYUIN-NYUIN-FLG(IDX1) = "1")
                           AND   (NACCT-DAY(IDX1)    NOT =  ZERO) 
                               MOVE    NACCT-DAY(IDX1) TO
                                                   WRK-DAY-SENTEI(IDX1)
                           END-IF
                         END-PERFORM
                       END-IF
                   ELSE
      *                ≥∫≈ˆ∆˛±°π‘∞Ÿ•∆°º•÷•Î§Œ∆…§ﬂπ˛§ﬂ
                       PERFORM    210-NYUINACT-GET-SEC
      *                ∆˛±°¥À‹Œ¡§»ΩÈ¥¸≤√ªª° ƒπ¥¸∏∫ªª°À§Œ¬‡»Ú
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF    (LNK-SROUNYUIN-NYUIN-FLG(IDX1) = "1")
                           AND   (NACCT-DAY(IDX1)    NOT =  ZERO) 
                               MOVE    NSRY-SRYCD(1)   TO  
                                                   WRK-NYUINKHN(IDX1)
      *
                               IF   FLG-RYAKU   =   ZERO
      *
                                  IF   NSRY-SRYCD(1)  NOT =   SPACE
                                      INITIALIZE    TNS-TENSU-REC
                                      MOVE NSRY-SRYCD(1)
                                                TO  TNS-SRYCD
                                      MOVE LNK-SROUNYUIN-HOSPNUM
                                                TO  TNS-HOSPNUM
                                      MOVE LNK-SROUNYUIN-SANTEIYM
                                                TO  TNS-YUKOSTYMD(1:6)
                                      MOVE "01" TO  TNS-YUKOSTYMD(7:2)
                                      MOVE LNK-SROUNYUIN-SANTEIYM
                                                TO  TNS-YUKOEDYMD(1:6)
                                      MOVE "01" TO  TNS-YUKOEDYMD(7:2)
                                      PERFORM 910-TENSU-READ-SEC
                                      IF  FLG-TENSU  NOT = ZERO
                                        MOVE  1 TO  WRK-NYUINRYO-ERR
                                      ELSE
                                        MOVE TNS-TEN
                                                TO  WRK-KHNTEN
                                      END-IF
                                  END-IF
      *
                                  PERFORM  VARYING  IDX3  FROM  2  BY  1
                                            UNTIL   IDX3   >    3
                                    IF   NSRY-SRYCD(IDX3)  NOT =   SPACE
                                      INITIALIZE    TNS-TENSU-REC
                                      MOVE NSRY-SRYCD(IDX3)
                                                TO  TNS-SRYCD
                                      MOVE LNK-SROUNYUIN-HOSPNUM
                                                TO  TNS-HOSPNUM
                                      MOVE LNK-SROUNYUIN-SANTEIYM
                                                TO  TNS-YUKOSTYMD(1:6)
                                      MOVE "01" TO  TNS-YUKOSTYMD(7:2)
                                      MOVE LNK-SROUNYUIN-SANTEIYM
                                                TO  TNS-YUKOEDYMD(1:6)
                                      MOVE "01" TO  TNS-YUKOEDYMD(7:2)
                                      PERFORM 910-TENSU-READ-SEC
                                      EVALUATE  TNS-TENSIKIBETU
                                         WHEN     3
                                             MOVE   NSRY-SRYCD(IDX3)  TO
                                                      WRK-SHOKIKSN-TAIHI
                                             MOVE     TNS-TEN         TO
                                                      WRK-KSNTEN
                                         WHEN     8
                                             MOVE   NSRY-SRYCD(IDX3)  TO
                                                      WRK-SHOKIKSN-TAIHI
                                             COMPUTE  WRK-KSNTEN =
                                                      TNS-TEN    *  -1
                                         WHEN     6
                                             MOVE   NSRY-SRYCD(IDX3)  TO
                                                      WRK-HYOKETU-TAIHI
                                             MOVE     TNS-TEN         TO
                                                      WRK-HYOTEN
                                      END-EVALUATE
                                    END-IF
                                  END-PERFORM
      *
      *                           ¬Ëª∞º‘π‘∞Ÿ§Œ¡™ƒÍ∆˛±°Œ¡ ‘Ω∏
                                  IF   LNK-SROUNYUIN-DAISANFLG  =  "1"
                                    IF   NSRY-SRYCD(1)  NOT =   SPACE
                                      PERFORM  220-DAISAN-HENSYU-SEC
                                    END-IF
                                  END-IF
      *
                                  MOVE    NACCT-SRYKBN
                                                TO  WRK-SRYKBN
      *                           ≥∞«Ò•≥°º•… ‘Ω∏Õ—§À∆˛±°¥À‹Œ¡≤√ªª∂Ë ¨
      *                           §Œ¬‡»Ú
                                  PERFORM   210-GAIHAKU-MAE-SEC
      *
                                  MOVE   ZERO   TO  WRK-TOKUREI-FLG
                                  MOVE   NSRY-SRYCD(1)
                                                TO  WRK-RYAKU-SRYCD
                                                    WRK-TOKUREI-SRYCD2
                                  MOVE   SPACE  TO  WRK-TOKUREI-SRYCD
                                  PERFORM  500-RYAKUSYO-KEISO-CHECK-SEC
                                  IF   WRK-TOKUREI-SRYCD  NOT = SPACE
                                    MOVE  WRK-TOKUREI-SRYCD
                                                TO  WRK-RYAKU-SRYCD
                                  END-IF
      *
                                  MOVE   NACCT-ZAISKBKBN
                                                TO  WRK-ZAISKBKBN
                                  PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                                  MOVE    1     TO    FLG-RYAKU
      *
      *                           ∆˛±°Œ¡∫ﬁ≈¿•¡•ß•√•Ø
                                  COMPUTE  WRK-HIKAKU-ZAITEN =
                                           WRK-KHNTEN + WRK-KSNTEN
                                  IF  WRK-HYOTEN  NOT =  ZERO
                                    COMPUTE WRK-HIKAKU-ZAITEN =
                                          ( WRK-HIKAKU-ZAITEN -
                                           (WRK-HIKAKU-ZAITEN *
                                            (WRK-HYOTEN / 100)) )
                                                              + 0.5
                                  END-IF
                                  IF  NACCT-ZAITEN  NOT =  ZERO
                                    IF  NACCT-ZAITEN  =
                                                    WRK-HIKAKU-ZAITEN
                                      CONTINUE
                                    ELSE
                                      MOVE   1  TO  WRK-NYUINRYO-ERR2
                                    END-IF
                                  END-IF
      *
                               END-IF
      *
                               IF    WRK-SHOKIKSN-TAIHI  NOT = SPACE
                                  MOVE  WRK-SHOKIKSN-TAIHI
                                               TO  WRK-SHOKIKSN(IDX1)
                               END-IF
                               IF    WRK-HYOKETU-TAIHI   NOT = SPACE
                                  MOVE  WRK-HYOKETU-TAIHI
                                               TO  WRK-HYOKETU(IDX1)
                               END-IF
      *
                               IF    WRK-SENTEI-SRYCD1   NOT = SPACE
                                  MOVE  WRK-SENTEI-SRYCD1
                                            TO  WRK-SENTEI-KHN(IDX1)
                               END-IF
                               IF    WRK-SENTEI-SRYCD2   NOT = SPACE
                                  MOVE  WRK-SENTEI-SRYCD2
                                            TO  WRK-SENTEI-KHNGEN(IDX1)
                               END-IF
      *
                               MOVE  WRK-SRYKBN
                                         TO  LNK-SROUNYUIN-SRYKBN(IDX1)
                               MOVE  WRK-KSNKBN
                                         TO  WRK-NYUKHNKSNKBN(IDX1)
      *                        ∆˛±°¥À‹Œ¡≤√ªª∂Ë ¨√÷§≠¥π§®
                               IF  LNK-SROUNYUIN-SANTEIYM >= "201804"
      *                          Œ≈Õ‹…¬≈Ô∆˛±°¥À‹Œ¡
      *                                        ° ∆√ Ã∆˛±°¥À‹Œ¡°À
                                 IF   (WRK-KSNKBN       = 27 )
                                 AND  (WRK-CDKBN-KBNNUM = 101)
                                   MOVE  901 TO WRK-NYUKHNKSNKBN(IDX1)
                                 END-IF
      *                          Õ≠æ≤ø«Œ≈ΩÍŒ≈Õ‹…¬æ≤∆˛±°¥À‹Œ¡
      *                                        ° ∆√ Ã∆˛±°¥À‹Œ¡°À
                                 IF   (WRK-KSNKBN       = 27 )
                                 AND  (WRK-CDKBN-KBNNUM = 109)
                                   MOVE  909 TO WRK-NYUKHNKSNKBN(IDX1)
                                 END-IF
                               END-IF
                               MOVE  WRK-CHUKSNCD-XXX
                                         TO  WRK-CHUKSNCD(IDX1)
                               MOVE  WRK-NYUKHNKBN-XXX
                                         TO  WRK-NYUKHNKBN(IDX1)
      *
                               IF    WRK-RYAKUSHO   NOT =  SPACE
                                  MOVE  WRK-RYAKUSHO
                                      TO  LNK-SROUNYUIN-RYAKUSYO(IDX1)
                               END-IF
      *
                               MOVE    1    TO   FLG-OK
                           END-IF
                       END-PERFORM
                   END-IF
                   MOVE   "tbl_nyuinacct"   TO  MCP-TABLE
                   MOVE   "key17"           TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
      ***      IF   FLG-OK     =   ZERO
      ***          MOVE     6      TO      LNK-SROUNYUIN-RC
      ***      END-IF
           ELSE
               MOVE     6      TO      LNK-SROUNYUIN-RC
           END-IF
      *    ∆˛±°≤Ò∑◊•Ø•Ì°º•∫
           MOVE     "tbl_nyuinacct"   TO  MCP-TABLE
           MOVE     "key17"           TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           IF   LNK-SROUNYUIN-RC    NOT =   ZERO
                   GO   TO    200-MAIN-EXT
           END-IF
      *
      *Œ¨æŒ§Œ ‘Ω∏° ÃÎ¥÷¥«∏Ó¬Œ¿©∆√ƒÍ∆¸∏∫ªª°À
           PERFORM    VARYING   IDX1  FROM   1   BY   1
                       UNTIL    IDX1   >    31
             IF   (WRK-NYUINKHN(IDX1)  NOT =  SPACE)
             AND  (WRK-KANGOGEN(IDX1)  NOT =  SPACE)
               MOVE     WRK-KANGOGEN(IDX1)  TO  WRK-RYAKU-SRYCD
               PERFORM  210-RYAKUSYO-YAGEN-HENSYU-SEC
             END-IF
           END-PERFORM
      *
      *œ´∫“∆˛±°§Œœ´∫“æËøÙ° ¥¸¥÷≤√ªª°À§À¥ﬁ§·§Î√Ì≤√ªª§ŒºË∆¿
           PERFORM    210-CHUKSN-GET-SEC
      *
      *œ´∫“∆˛±°ª˛§Œ≥∞«ÒºË∆¿
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-SROUNYUIN-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-SROUNYUIN-PTID      TO  NACCT-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO  NACCT-SRYYM
           MOVE    "2"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key17"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
      *            ≥∞«Ò∂Ë ¨§Œ¬‡»Ú
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                              UNTIL     IDX1   >   31
      ****             IF    (WRK-NYUINKHN(IDX1) NOT = SPACE) 
                       IF     NACCT-DAY(IDX1)    NOT =  ZERO
                              PERFORM     210-GAIHAKU-SET-SEC
                       END-IF
                   END-PERFORM
                   MOVE   "tbl_nyuinacct"  TO  MCP-TABLE
                   MOVE   "key17"          TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    ∆˛±°≤Ò∑◊•Ø•Ì°º•∫
           MOVE     "tbl_nyuinacct"        TO  MCP-TABLE
           MOVE     "key17"                TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *œ´∫“∆˛±°ª˛§Œ∆˛±°Œ¡≤√ªªºË∆¿° ∆˛±°≤Ò∑◊§Ë§Í°À
           MOVE    ZERO                    TO  KSN-IDX
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-SROUNYUIN-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-SROUNYUIN-PTID      TO  NACCT-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO  NACCT-SRYYM
           MOVE    "6"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE     "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE     "key17"            TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                   MOVE      ZERO     TO      FLG-RYAKU
                   MOVE      ZERO     TO      FLG-KSN
      *            ∆˛±°Œ¡≤√ªª§Œ¬‡»Ú
                   PERFORM    VARYING   IDX1   FROM   1   BY   1
                              UNTIL     IDX1   >   31
                       IF    ((WRK-NYUINKHN(IDX1) NOT = SPACE) OR
                              (WRK-NYUINDAY-KBN(IDX1) =  "1" ))
                       AND   ((LNK-SROUNYUIN-GAIHAKU(IDX1)
                                                      = ZERO ) OR
                              (LNK-SROUNYUIN-GAIHAKU(IDX1)
                                                     >=   4  ))
                       AND   ( NACCT-DAY(IDX1)    NOT =  ZERO )
      *                  KSN1°ßÃÎ¥÷∂€µﬁ¬Œ¿©≥Œ ›≤√ªª
      *                  KSN12:∞Âª’«€√÷≤√ªª£±
      *                  KSN2°ß∞Âª’«€√÷≤√ªª£≤
      *                  KSN4°ß¥«∏Ó«€√÷≤√ªª£±
      *                  KSN3°ß¥«∏Ó«€√÷≤√ªª£≤
      *                  KSN6°ßÃÎ¥÷¥«∏Ó«€√÷≤√ªª£±
      *                  KSN5°ßÃÎ¥÷¥«∏Ó«€√÷≤√ªª£≤
      *                  KSN7°ßΩ≈≈Ÿ«ß√Œæ…≤√ªª
      *                  KSN8°ßÍÛ·Ï…æ≤¡º¬ª‹≤√ªª° Œ≈Õ‹…¬≈Ô°À
      *                  KSN9°ßÍÛ·Ï…æ≤¡º¬ª‹≤√ªª° Õ≠æ≤ø«Œ≈ΩÍ°À
      *                  KSN11:Ω≈æ…ª˘° º‘°Àºı∆˛œ¢∑»≤√ªª° æ„≥≤º‘ª‹¿ﬂ≈˘∆˛±°¥À‹Œ¡°À
      *                  KSN13:∞Ï»Ã…¬≈Ô¥«∏Ó…¨Õ◊≈Ÿ…æ≤¡≤√ªª
      *                  KSN15:Ω≈≈Ÿ«ß√Œæ…≤√ªª° ¿∫ø¿…¬≈Ô∆˛±°¥À‹Œ¡°À
      *                  KSN17:∞Ï»Ã…¬≈Ô¥«∏Ó…¨Õ◊≈Ÿ…æ≤¡≤√ªª° ¿ÏÃÁ…¬±°°À
                         IF  ( FLG-KENPOJYUNKYO  = ZERO )
                         AND ( NACCT-SRYCDTOTAL(6:9) =
                                    CONS-200804-KSN1
                                 OR CONS-201004-KSN12
                                 OR CONS-200804-KSN2
                                 OR CONS-200804-KSN4
                                 OR CONS-200804-KSN3
                                 OR CONS-200804-KSN6
                                 OR CONS-200804-KSN5
                                 OR CONS-200804-KSN7
                                 OR CONS-200804-KSN8
                                 OR CONS-200804-KSN9
                                 OR CONS-201004-KSN13
                                 OR CONS-201004-KSN15
                                 OR CONS-201004-KSN17
                                 OR CONS-201204-KSN4
                                 OR CONS-201204-KSN5
                                 OR CONS-201204-KSN8
                                 OR CONS-201204-KSN9
                                 OR CONS-201204-KSN10
                                 OR CONS-201204-KSN11
                                 OR CONS-201307-KSN1
                                 OR CONS-201307-KSN2
                                 OR CONS-201307-KSN3
                                 OR CONS-201404-KSN1
                                 OR CONS-201404-KSN2
                                 OR CONS-201404-KSN3
                                 OR CONS-201404-KSN4
                                 OR CONS-201404-KSN5
                                 OR CONS-201404-KSN6
                                 OR CONS-201404-KSN7
                                 OR CONS-201404-KSN8
                                 OR CONS-201404-KSN9
                                 OR CONS-201407-KSN1
                                 OR CONS-201407-KSN2
                                 OR CONS-201407-KSN3
                                 OR CONS-201407-KSN4
                                 OR CONS-201604-KSN1
                                 OR CONS-201604-KSN2
                                 OR CONS-201604-KSN3
                                 OR CONS-201604-KSN4
                                 OR CONS-201604-KSN5
                                 OR CONS-201604-KSN6
                                 OR CONS-201604-KSN7
                                 OR CONS-201604-KSN8
                                 OR CONS-201604-KSN9
                                 OR CONS-201604-KSN10
                                 OR CONS-201604-KSN11
                                 OR CONS-201804-KSN1
                                 OR CONS-201804-KSN4
                                 OR CONS-201804-KSN5
                                 OR CONS-201804-KSN6    )
                          IF  (WRK-NYUINKHN(IDX1)      NOT = SPACE)
                          AND ((LNK-SROUNYUIN-GAIHAKU(IDX1)
                                                           = ZERO) OR
                               (LNK-SROUNYUIN-GAIHAKU(IDX1)
                                                          >=   4 ))
                           EVALUATE   NACCT-SRYCDTOTAL(6:9)
                             WHEN  CONS-200804-KSN1
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-200804-KSN1(IDX1)
                             WHEN  CONS-201004-KSN12
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201004-KSN12(IDX1)
                             WHEN  CONS-200804-KSN2
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-200804-KSN2(IDX1)
                             WHEN  CONS-200804-KSN4
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-200804-KSN4(IDX1)
                             WHEN  CONS-200804-KSN3
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-200804-KSN3(IDX1)
                             WHEN  CONS-200804-KSN6
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-200804-KSN6(IDX1)
                             WHEN  CONS-200804-KSN5
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-200804-KSN5(IDX1)
                             WHEN  CONS-200804-KSN7
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-200804-KSN7(IDX1)
                             WHEN  CONS-200804-KSN8
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-200804-KSN8(IDX1)
                             WHEN  CONS-200804-KSN9
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-200804-KSN9(IDX1)
                             WHEN  CONS-201004-KSN13
                             WHEN  CONS-201307-KSN1
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201004-KSN13(IDX1)
                             WHEN  CONS-201004-KSN15
                             WHEN  CONS-201307-KSN2
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201004-KSN15(IDX1)
                             WHEN  CONS-201004-KSN17
                             WHEN  CONS-201307-KSN3
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201004-KSN17(IDX1)
      *
                             WHEN  CONS-201204-KSN4
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201204-KSN4 (IDX1)
                             WHEN  CONS-201204-KSN5
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201204-KSN5 (IDX1)
                             WHEN  CONS-201204-KSN8
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201204-KSN8 (IDX1)
                             WHEN  CONS-201204-KSN9
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201204-KSN9 (IDX1)
                             WHEN  CONS-201204-KSN10
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201204-KSN10(IDX1)
                             WHEN  CONS-201204-KSN11
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201204-KSN11(IDX1)
      *
                             WHEN  CONS-201404-KSN1
                             WHEN  CONS-201404-KSN8
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201404-KSN1(IDX1)
                             WHEN  CONS-201404-KSN2
                             WHEN  CONS-201404-KSN9
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201404-KSN2(IDX1)
                             WHEN  CONS-201404-KSN3
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201404-KSN3(IDX1)
                             WHEN  CONS-201404-KSN4
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201404-KSN4(IDX1)
                             WHEN  CONS-201404-KSN5
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201404-KSN5(IDX1)
                             WHEN  CONS-201404-KSN6
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201404-KSN6(IDX1)
                             WHEN  CONS-201404-KSN7
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201404-KSN7(IDX1)
      *
                             WHEN  CONS-201407-KSN1
                             WHEN  CONS-201407-KSN3
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201407-KSN1(IDX1)
                             WHEN  CONS-201407-KSN2
                             WHEN  CONS-201407-KSN4
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201407-KSN2(IDX1)
      *
                             WHEN  CONS-201604-KSN1
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201604-KSN1(IDX1)
                             WHEN  CONS-201604-KSN2
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201604-KSN2(IDX1)
                             WHEN  CONS-201604-KSN3
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201604-KSN3(IDX1)
                             WHEN  CONS-201604-KSN4
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201604-KSN4(IDX1)
                             WHEN  CONS-201604-KSN5
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201604-KSN5(IDX1)
                             WHEN  CONS-201604-KSN6
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201604-KSN6(IDX1)
                             WHEN  CONS-201604-KSN7
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201604-KSN7(IDX1)
                             WHEN  CONS-201604-KSN8
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201604-KSN8(IDX1)
                             WHEN  CONS-201604-KSN9
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201604-KSN9(IDX1)
                             WHEN  CONS-201604-KSN10
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201604-KSN10(IDX1)
                             WHEN  CONS-201604-KSN11
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201604-KSN11(IDX1)
      *
                             WHEN  CONS-201804-KSN1
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201804-KSN1(IDX1)
                             WHEN  CONS-201804-KSN4
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201804-KSN4(IDX1)
                             WHEN  CONS-201804-KSN5
                             WHEN  CONS-201804-KSN6
                                  MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-201804-KSN5(IDX1)
                           END-EVALUATE
      *
                           IF   FLG-RYAKU   =   ZERO
                              MOVE   ZERO   TO  WRK-TOKUREI-FLG
                              MOVE   NACCT-SRYCDTOTAL(6:9)
                                            TO  WRK-RYAKU-SRYCD
                              MOVE   NACCT-ZAISKBKBN
                                            TO  WRK-ZAISKBKBN
                              PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                              MOVE    1     TO    FLG-RYAKU
                           END-IF
                          END-IF
                         ELSE
                           IF   FLG-KSN   =  ZERO
                               ADD     1    TO    KSN-IDX
                               MOVE    1    TO    FLG-KSN
                           END-IF
                           MOVE   NACCT-SRYCDTOTAL(6:9)
                                        TO  WRK-NYUKSN(KSN-IDX)
                           MOVE   NACCT-ZAITEN  
                                        TO  WRK-NYUKSN-ZAITEN(KSN-IDX)
                           MOVE   NACCT-DAY(IDX1)
                                        TO  WRK-NYUKSN-DAY(KSN-IDX IDX1)
      *
                           IF   FLG-RYAKU   =   ZERO
                              MOVE   ZERO   TO  WRK-TOKUREI-FLG
                              MOVE   NACCT-SRYCDTOTAL(6:9)
                                            TO  WRK-RYAKU-SRYCD
                              MOVE   NACCT-ZAISKBKBN
                                            TO  WRK-ZAISKBKBN
                              PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                              MOVE    1     TO    FLG-RYAKU
                           END-IF
                         END-IF
      *
                       END-IF
                   END-PERFORM
                   MOVE   "tbl_nyuinacct"   TO  MCP-TABLE
                   MOVE   "key17"           TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    ∆˛±°≤Ò∑◊•Ø•Ì°º•∫
           MOVE     "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE     "key17"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *
      *œ´∫“∆˛±°ª˛§Œ∆˛±°Œ¡≤√ªªºË∆¿° ø«Œ≈≤Ò∑◊§Ë§Í°À
           INITIALIZE                           SRYACCT-REC
           MOVE    LNK-SROUNYUIN-HOSPNUM   TO   ACCT-HOSPNUM
           MOVE    "1"                     TO   ACCT-NYUGAIKBN
           MOVE    LNK-SROUNYUIN-PTID      TO   ACCT-PTID
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO   ACCT-SRYYM
           MOVE    LNK-SROUNYUIN-HKNCOMBI  TO   ACCT-HKNCOMBI
           MOVE    "tbl_sryacct"           TO   MCP-TABLE
           MOVE    "key22"                 TO   MCP-PATHNAME
           PERFORM 910-SRYACCT-SELECT-SEC
           IF      FLG-SRYACCT           =   ZERO
               MOVE     "tbl_sryacct"      TO   MCP-TABLE
               MOVE     "key22"            TO   MCP-PATHNAME
               PERFORM   910-SRYACCT-READ-SEC
               PERFORM   UNTIL   (FLG-SRYACCT    =   1)
                 MOVE      ZERO     TO      FLG-RYAKU
                 IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                      (ACCT-ZAIKAISU  NOT =    ZERO)    AND
                      (ACCT-JIHOKBN       =   SPACE)
                   IF   ACCT-SRYKBN  =  "90" OR "92"
      *
                     INITIALIZE                         SRYACT-REC
                     MOVE    ACCT-HOSPNUM       TO      SRY-HOSPNUM
                     MOVE    ACCT-NYUGAIKBN     TO      SRY-NYUGAIKBN
                     MOVE    ACCT-PTID          TO      SRY-PTID
                     MOVE    ACCT-SRYYM         TO      SRY-SRYYM
                     MOVE    ACCT-ZAINUM        TO      SRY-ZAINUM
                     MOVE    SRYACT-REC         TO      MCPDATA-REC
                     MOVE    "DBSELECT"         TO      MCP-FUNC
                     MOVE    "tbl_sryact"       TO      MCP-TABLE
                     MOVE    "key9"             TO      MCP-PATHNAME
                     CALL    "ORCDBMAIN"        USING   MCPAREA
                                                        MCPDATA-REC
                                                        SPA-AREA
                     IF      MCP-RC               =       ZERO
                         MOVE    "tbl_sryact"   TO      MCP-TABLE
                         MOVE    "key9"         TO      MCP-PATHNAME
                         PERFORM  910-SRYACT-READ-SEC
                         IF   FLG-SRYACT    =    0
      *                     KSN1°ßÃÎ¥÷∂€µﬁ¬Œ¿©≥Œ ›≤√ªª
      *                     KSN12:∞Âª’«€√÷≤√ªª£±
      *                     KSN2°ß∞Âª’«€√÷≤√ªª£≤
      *                     KSN4°ß¥«∏Ó«€√÷≤√ªª£±
      *                     KSN3°ß¥«∏Ó«€√÷≤√ªª£≤
      *                     KSN6°ßÃÎ¥÷¥«∏Ó«€√÷≤√ªª£±
      *                     KSN5°ßÃÎ¥÷¥«∏Ó«€√÷≤√ªª£≤
      *                     KSN7°ßΩ≈≈Ÿ«ß√Œæ…≤√ªª
      *                     KSN8°ßÍÛ·Ï…æ≤¡º¬ª‹≤√ªª° Œ≈Õ‹…¬≈Ô°À
      *                     KSN9°ßÍÛ·Ï…æ≤¡º¬ª‹≤√ªª° Õ≠æ≤ø«Œ≈ΩÍ°À
      *                     KSN11:Ω≈æ…ª˘° º‘°Àºı∆˛œ¢∑»≤√ªª° æ„≥≤º‘ª‹¿ﬂ≈˘∆˛±°¥À‹Œ¡°À
      *                     KSN13:∞Ï»Ã…¬≈Ô¥«∏Ó…¨Õ◊≈Ÿ…æ≤¡≤√ªª
      *                     KSN15:Ω≈≈Ÿ«ß√Œæ…≤√ªª° ¿∫ø¿…¬≈Ô∆˛±°¥À‹Œ¡°À
      *                     KSN17:∞Ï»Ã…¬≈Ô¥«∏Ó…¨Õ◊≈Ÿ…æ≤¡≤√ªª° ¿ÏÃÁ…¬±°°À
                            IF (SRY-SRYCD(1)  =  CONS-200804-KSN1
                                              OR CONS-201004-KSN12
                                              OR CONS-200804-KSN2
                                              OR CONS-200804-KSN4
                                              OR CONS-200804-KSN3
                                              OR CONS-200804-KSN6
                                              OR CONS-200804-KSN5
                                              OR CONS-200804-KSN7
                                              OR CONS-200804-KSN8
                                              OR CONS-200804-KSN9
                                              OR CONS-201004-KSN13
                                              OR CONS-201004-KSN15
                                              OR CONS-201004-KSN17)
                            OR (SRY-SRYCD(1)  =  CONS-201204-KSN4
                                              OR CONS-201204-KSN5
                                              OR CONS-201204-KSN8
                                              OR CONS-201204-KSN9
                                              OR CONS-201204-KSN10
                                              OR CONS-201204-KSN11)
                            OR (SRY-SRYCD(1)  =  CONS-201307-KSN1
                                              OR CONS-201307-KSN2
                                              OR CONS-201307-KSN3)
                            OR (SRY-SRYCD(1)  =  CONS-201404-KSN1
                                              OR CONS-201404-KSN2
                                              OR CONS-201404-KSN3
                                              OR CONS-201404-KSN4
                                              OR CONS-201404-KSN5
                                              OR CONS-201404-KSN6
                                              OR CONS-201404-KSN7
                                              OR CONS-201404-KSN8
                                              OR CONS-201404-KSN9)
                            OR (SRY-SRYCD(1)  =  CONS-201407-KSN1
                                              OR CONS-201407-KSN2
                                              OR CONS-201407-KSN3
                                              OR CONS-201407-KSN4)
                            OR (SRY-SRYCD(1)  =  CONS-201604-KSN1
                                              OR CONS-201604-KSN2
                                              OR CONS-201604-KSN3
                                              OR CONS-201604-KSN4
                                              OR CONS-201604-KSN5
                                              OR CONS-201604-KSN6
                                              OR CONS-201604-KSN7
                                              OR CONS-201604-KSN8
                                              OR CONS-201604-KSN9
                                              OR CONS-201604-KSN10
                                              OR CONS-201604-KSN11)
                            OR (SRY-SRYCD(1)  =  CONS-201804-KSN1
                                              OR CONS-201804-KSN4
                                              OR CONS-201804-KSN5
                                              OR CONS-201804-KSN6)
                                MOVE  SRY-SRYCD(1)   TO
                                               ACCT-SRYCDTOTAL(6:9)
                            END-IF
                         END-IF
                     END-IF
      *              ø«Œ≈π‘∞Ÿ•Ø•Ì°º•∫
                     MOVE     "tbl_sryact"    TO    MCP-TABLE
                     MOVE     "key9"          TO    MCP-PATHNAME
                     PERFORM   910-DB-CLOSE-SEC
      *
                     IF  ( FLG-KENPOJYUNKYO  = ZERO )
                     AND ( ACCT-SRYCDTOTAL(6:9) =
                               CONS-200804-KSN1
                            OR CONS-201004-KSN12
                            OR CONS-200804-KSN2
                            OR CONS-200804-KSN4
                            OR CONS-200804-KSN3
                            OR CONS-200804-KSN6
                            OR CONS-200804-KSN5
                            OR CONS-200804-KSN7
                            OR CONS-200804-KSN8
                            OR CONS-200804-KSN9
                            OR CONS-201004-KSN13
                            OR CONS-201004-KSN15
                            OR CONS-201004-KSN17
                            OR CONS-201204-KSN4
                            OR CONS-201204-KSN5
                            OR CONS-201204-KSN8
                            OR CONS-201204-KSN9
                            OR CONS-201204-KSN10
                            OR CONS-201204-KSN11
                            OR CONS-201307-KSN1
                            OR CONS-201307-KSN2
                            OR CONS-201307-KSN3
                            OR CONS-201404-KSN1
                            OR CONS-201404-KSN2
                            OR CONS-201404-KSN3
                            OR CONS-201404-KSN4
                            OR CONS-201404-KSN5
                            OR CONS-201404-KSN6
                            OR CONS-201404-KSN7
                            OR CONS-201404-KSN8
                            OR CONS-201404-KSN9
                            OR CONS-201407-KSN1
                            OR CONS-201407-KSN2
                            OR CONS-201407-KSN3
                            OR CONS-201407-KSN4
                            OR CONS-201604-KSN1
                            OR CONS-201604-KSN2
                            OR CONS-201604-KSN3
                            OR CONS-201604-KSN4
                            OR CONS-201604-KSN5
                            OR CONS-201604-KSN6
                            OR CONS-201604-KSN7
                            OR CONS-201604-KSN8
                            OR CONS-201604-KSN9
                            OR CONS-201604-KSN10
                            OR CONS-201604-KSN11
                            OR CONS-201804-KSN1
                            OR CONS-201804-KSN4
                            OR CONS-201804-KSN5
                            OR CONS-201804-KSN6     )
      *                ∆˛±°Œ¡≤√ªª§Œ¬‡»Ú
                       PERFORM    VARYING   IDX1   FROM   1   BY   1
                                  UNTIL     IDX1   >   31
                           IF    (WRK-NYUINKHN(IDX1) NOT = SPACE)
                           AND   ((LNK-SROUNYUIN-GAIHAKU(IDX1)
                                                         = ZERO) OR
                                  (LNK-SROUNYUIN-GAIHAKU(IDX1)
                                                        >=   4 ))
                           AND   (ACCT-DAY(1 IDX1)   NOT =  ZERO)
                              EVALUATE   ACCT-SRYCDTOTAL(6:9)
                                WHEN  CONS-200804-KSN1
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-200804-KSN1(IDX1)
                                WHEN  CONS-201004-KSN12
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201004-KSN12(IDX1)
                                WHEN  CONS-200804-KSN2
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-200804-KSN2(IDX1)
                                WHEN  CONS-200804-KSN4
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-200804-KSN4(IDX1)
                                WHEN  CONS-200804-KSN3
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-200804-KSN3(IDX1)
                                WHEN  CONS-200804-KSN6
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-200804-KSN6(IDX1)
                                WHEN  CONS-200804-KSN5
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-200804-KSN5(IDX1)
                                WHEN  CONS-200804-KSN7
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-200804-KSN7(IDX1)
                                WHEN  CONS-200804-KSN8
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-200804-KSN8(IDX1)
                                WHEN  CONS-200804-KSN9
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-200804-KSN9(IDX1)
                                WHEN  CONS-201004-KSN13
                                WHEN  CONS-201307-KSN1
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201004-KSN13(IDX1)
                                WHEN  CONS-201004-KSN15
                                WHEN  CONS-201307-KSN2
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201004-KSN15(IDX1)
                                WHEN  CONS-201004-KSN17
                                WHEN  CONS-201307-KSN3
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201004-KSN17(IDX1)
      *
                                WHEN  CONS-201204-KSN4
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201204-KSN4 (IDX1)
                                WHEN  CONS-201204-KSN5
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201204-KSN5 (IDX1)
                                WHEN  CONS-201204-KSN8
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201204-KSN8 (IDX1)
                                WHEN  CONS-201204-KSN9
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201204-KSN9 (IDX1)
                                WHEN  CONS-201204-KSN10
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201204-KSN10(IDX1)
                                WHEN  CONS-201204-KSN11
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201204-KSN11(IDX1)
      *
                                WHEN  CONS-201404-KSN1
                                WHEN  CONS-201404-KSN8
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201404-KSN1(IDX1)
                                WHEN  CONS-201404-KSN2
                                WHEN  CONS-201404-KSN9
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201404-KSN2(IDX1)
                                WHEN  CONS-201404-KSN3
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201404-KSN3(IDX1)
                                WHEN  CONS-201404-KSN4
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201404-KSN4(IDX1)
                                WHEN  CONS-201404-KSN5
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201404-KSN5(IDX1)
                                WHEN  CONS-201404-KSN6
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201404-KSN6(IDX1)
                                WHEN  CONS-201404-KSN7
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201404-KSN7(IDX1)
      *
                                WHEN  CONS-201407-KSN1
                                WHEN  CONS-201407-KSN3
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201407-KSN1(IDX1)
                                WHEN  CONS-201407-KSN2
                                WHEN  CONS-201407-KSN4
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201407-KSN2(IDX1)
      *
                                WHEN  CONS-201604-KSN1
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201604-KSN1(IDX1)
                                WHEN  CONS-201604-KSN2
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201604-KSN2(IDX1)
                                WHEN  CONS-201604-KSN3
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201604-KSN3(IDX1)
                                WHEN  CONS-201604-KSN4
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201604-KSN4(IDX1)
                                WHEN  CONS-201604-KSN5
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201604-KSN5(IDX1)
                                WHEN  CONS-201604-KSN6
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201604-KSN6(IDX1)
                                WHEN  CONS-201604-KSN7
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201604-KSN7(IDX1)
                                WHEN  CONS-201604-KSN8
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201604-KSN8(IDX1)
                                WHEN  CONS-201604-KSN9
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201604-KSN9(IDX1)
                                WHEN  CONS-201604-KSN10
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201604-KSN10(IDX1)
                                WHEN  CONS-201604-KSN11
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201604-KSN11(IDX1)
      *
                                WHEN  CONS-201804-KSN1
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201804-KSN1(IDX1)
                                WHEN  CONS-201804-KSN4
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201804-KSN4(IDX1)
                                WHEN  CONS-201804-KSN5
                                WHEN  CONS-201804-KSN6
                                      MOVE   ACCT-DAY(1 IDX1)
                                        TO  WRK-201804-KSN5(IDX1)
                              END-EVALUATE
      *
                              IF   FLG-RYAKU   =   ZERO
                                 MOVE   ZERO   TO  WRK-TOKUREI-FLG
                                 MOVE   ACCT-SRYCDTOTAL(6:9)
                                               TO  WRK-RYAKU-SRYCD
                                 MOVE   "6"    TO  WRK-ZAISKBKBN
                                 PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                                 MOVE    1     TO    FLG-RYAKU
                              END-IF
      *
                           END-IF
                       END-PERFORM
                     ELSE
      *                 IF  (ACCT-SRYCDTOTAL(6:9)  =  CONS-ROSAICHIRYO
      *                                           OR  CONS-BYOUI
      *                                           OR  CONS-TOKUKANSI)
                       IF  (SRY-SRYCD(1)  =   CONS-ROSAICHIRYO)  OR
                           (SRY-SRYCD(2)  =   CONS-ROSAICHIRYO)  OR
                           (SRY-SRYCD(3)  =   CONS-ROSAICHIRYO)  OR
                           (SRY-SRYCD(4)  =   CONS-ROSAICHIRYO)  OR
                           (SRY-SRYCD(5)  =   CONS-ROSAICHIRYO)  OR
                           (SRY-SRYCD(1)  =   CONS-BYOUI)  OR
                           (SRY-SRYCD(2)  =   CONS-BYOUI)  OR
                           (SRY-SRYCD(3)  =   CONS-BYOUI)  OR
                           (SRY-SRYCD(4)  =   CONS-BYOUI)  OR
                           (SRY-SRYCD(5)  =   CONS-BYOUI)  
                       OR  (ACCT-ZAIKBN           =    2           )
      *                  ∆˛±°Œ¡≤√ªª§Œ¬‡»Ú
                        IF  ACCT-ZAIKBN  =  2
                         PERFORM    VARYING   IDX1   FROM   1   BY   1
                                    UNTIL     IDX1   >   31
                             IF    ((WRK-NYUINKHN(IDX1) NOT = SPACE) OR
                                    (WRK-NYUINDAY-KBN(IDX1) =  "1" ))
                             AND   ( ACCT-DAY(1 IDX1)   NOT =  ZERO )
                               COMPUTE LNK-SROUNYUIN-GENTEN(IDX1)
                                    =  LNK-SROUNYUIN-GENTEN(IDX1)
                                    + (ACCT-ZAITEN * ACCT-DAY(1 IDX1))
                             END-IF
                         END-PERFORM
                        ELSE
                         PERFORM    VARYING   IDX1   FROM   1   BY   1
                                    UNTIL     IDX1   >   31
      **>                    IF    ((WRK-NYUINKHN(IDX1) NOT = SPACE) OR
      **>                           (WRK-NYUINDAY-KBN(IDX1) =  "1" ))
      **>                    AND   ( ACCT-DAY(1 IDX1)   NOT =  ZERO )
                             IF    ( ACCT-DAY(1 IDX1)   NOT =  ZERO )
                               COMPUTE LNK-SROUNYUIN-ZAITEN-ACCTETC
                                                             (IDX1)
                                    =  LNK-SROUNYUIN-ZAITEN-ACCTETC
                                                             (IDX1)
                                    + (ACCT-ZAITEN * ACCT-DAY(1 IDX1))
                               COMPUTE LNK-SROUNYUIN-ZAITEN-ACCTETC2
                                                             (IDX1)
                                    =  LNK-SROUNYUIN-ZAITEN-ACCTETC2
                                                             (IDX1)
                                    + (ACCT-ZAITEN * ACCT-DAY(2 IDX1))
                               COMPUTE LNK-SROUNYUIN-ZAITEN-ACCTETC3
                                                             (IDX1)
                                    =  LNK-SROUNYUIN-ZAITEN-ACCTETC3
                                                             (IDX1)
                                    + (ACCT-ZAITEN * ACCT-DAY(3 IDX1))
      *
                               IF   FLG-RYAKU   =   ZERO
                                  MOVE   ZERO   TO  WRK-TOKUREI-FLG
                                  MOVE   ACCT-SRYCDTOTAL(6:9)
                                                TO  WRK-RYAKU-SRYCD
                                  MOVE   "6"    TO  WRK-ZAISKBKBN
                                  PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                                  MOVE    1     TO    FLG-RYAKU
                               END-IF
      *
                             END-IF
                         END-PERFORM
                        END-IF
                       ELSE
      *                  ∆˛±°Œ¡≤√ªª§Œ¬‡»Ú
                         PERFORM    VARYING   IDX1   FROM   1   BY   1
                                    UNTIL     IDX1   >   31
                             IF    ((WRK-NYUINKHN(IDX1) NOT = SPACE) OR
                                    (WRK-NYUINDAY-KBN(IDX1) =  "1" ))
                             AND   ( ACCT-DAY(1 IDX1)   NOT =  ZERO )
                               COMPUTE LNK-SROUNYUIN-ZAITEN-ACCT (IDX1)
                                    =  LNK-SROUNYUIN-ZAITEN-ACCT (IDX1)
                                    + (ACCT-ZAITEN * ACCT-DAY(1 IDX1))
                               COMPUTE LNK-SROUNYUIN-ZAITEN-ACCT2(IDX1)
                                    =  LNK-SROUNYUIN-ZAITEN-ACCT2(IDX1)
                                    + (ACCT-ZAITEN * ACCT-DAY(2 IDX1))
                               COMPUTE LNK-SROUNYUIN-ZAITEN-ACCT3(IDX1)
                                    =  LNK-SROUNYUIN-ZAITEN-ACCT3(IDX1)
                                    + (ACCT-ZAITEN * ACCT-DAY(3 IDX1))
      *
                               IF   FLG-RYAKU   =   ZERO
                                  MOVE    ZERO  TO    WRK-TOKUREI-FLG
                                  PERFORM   210-SRYACT-HENSYU-SEC
      *
                                  MOVE    1     TO    FLG-RYAKU
                               END-IF
      *
                             END-IF
                         END-PERFORM
                       END-IF
                     END-IF
                   END-IF
                 END-IF
                 MOVE     "tbl_sryacct"         TO    MCP-TABLE
                 MOVE     "key22"               TO    MCP-PATHNAME
                 PERFORM   910-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *    ø«Œ≈≤Ò∑◊•Ø•Ì°º•∫
           MOVE     "tbl_sryacct"         TO    MCP-TABLE
           MOVE     "key22"               TO    MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *
      *    ºË∆¿§∑§øœ´∫“∆˛±°æ Û§´§È∆¸√±∞Ã§Œ∆˛±°≤Ò∑◊æ Û§Ú∫∆πΩ√€
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
               IF    WRK-DAY-SENTEI(IDX1)  =  ZERO
                   MOVE  SPACE  TO  WRK-SENTEI-KHN   (IDX1)
                   MOVE  SPACE  TO  WRK-SENTEI-KHNGEN(IDX1)
               END-IF
           END-PERFORM
      *
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
               MOVE     ZERO                TO  WRK-MEISAISU
      *        ∆˛±°¥À‹Œ¡
               IF         WRK-NYUINKHN(IDX1)  NOT =        SPACE
                 IF   WRK-DAY-SENTEI(IDX1)  NOT =  ZERO
                   IF    WRK-SENTEI-KHNGEN(IDX1)  NOT =  SPACE
                     ADD    1   TO  WRK-MEISAISU
                     MOVE  WRK-SENTEI-KHN(IDX1)
                          TO  LNK-SROUNYUIN-SRYCD(IDX1 WRK-MEISAISU)
                     MOVE  "B"
                          TO  LNK-SROUNYUIN-SRYCD-SK(IDX1 WRK-MEISAISU)
      *
                     ADD    1   TO  WRK-MEISAISU
                     MOVE  WRK-SENTEI-KHNGEN(IDX1)
                          TO  LNK-SROUNYUIN-SRYCD(IDX1 WRK-MEISAISU)
                     MOVE  "1"
                          TO  LNK-SROUNYUIN-SRYCD-SK(IDX1 WRK-MEISAISU)
                     MOVE  "1"  TO  LNK-SROUNYUIN-SENTEI-FLG(IDX1)
                   ELSE
                     ADD    1   TO  WRK-MEISAISU
                     MOVE  WRK-SENTEI-KHN(IDX1)
                          TO  LNK-SROUNYUIN-SRYCD(IDX1 WRK-MEISAISU)
                     MOVE  "1"
                          TO  LNK-SROUNYUIN-SRYCD-SK(IDX1 WRK-MEISAISU)
                   END-IF
                 ELSE
                     ADD    1   TO  WRK-MEISAISU
                     MOVE  WRK-NYUINKHN(IDX1)
                          TO  LNK-SROUNYUIN-SRYCD(IDX1 WRK-MEISAISU)
                     MOVE  "1"
                          TO  LNK-SROUNYUIN-SRYCD-SK(IDX1 WRK-MEISAISU)
                 END-IF
               END-IF
      *
      *        ÃÎ¥÷¥«∏Ó¬Œ¿©∆√ƒÍ∆¸∏∫ªª
               IF        (WRK-NYUINKHN(IDX1)  NOT =        SPACE)
               AND       (WRK-KANGOGEN(IDX1)  NOT =        SPACE)
                     ADD    1   TO  WRK-MEISAISU
                     MOVE  WRK-KANGOGEN(IDX1)
                          TO  LNK-SROUNYUIN-SRYCD(IDX1 WRK-MEISAISU)
                     MOVE  "C"
                          TO  LNK-SROUNYUIN-SRYCD-SK(IDX1 WRK-MEISAISU)
               END-IF
      *
      *        ≥∞«Ò
               IF        (WRK-NYUINKHN(IDX1)  NOT =        SPACE)
               AND       (WRK-GAIHAKU(IDX1)   NOT =        SPACE)
                   IF    WRK-GAIHAKU2(IDX1)       NOT =  SPACE
                     ADD    1   TO  WRK-MEISAISU
                     MOVE  WRK-GAIHAKU(IDX1)
                          TO  LNK-SROUNYUIN-SRYCD(IDX1 WRK-MEISAISU)
                     MOVE  "A"
                          TO  LNK-SROUNYUIN-SRYCD-SK(IDX1 WRK-MEISAISU)
      *
                     ADD    1   TO  WRK-MEISAISU
                     MOVE  WRK-GAIHAKU2(IDX1)
                          TO  LNK-SROUNYUIN-SRYCD(IDX1 WRK-MEISAISU)
                     MOVE  "2"
                          TO  LNK-SROUNYUIN-SRYCD-SK(IDX1 WRK-MEISAISU)
                   ELSE
                     ADD    1   TO  WRK-MEISAISU
                     MOVE  WRK-GAIHAKU(IDX1)
                          TO  LNK-SROUNYUIN-SRYCD(IDX1 WRK-MEISAISU)
                     MOVE  "2"
                          TO  LNK-SROUNYUIN-SRYCD-SK(IDX1 WRK-MEISAISU)
                   END-IF
               END-IF
      *
      *        ΩÈ¥¸≤√ªª° ƒπ¥¸∏∫ªª°À
               IF      FLG-KENPOJYUNKYO  =  1
                 IF    (WRK-GAIHAKU(IDX1)        =
                                     SPACE      OR
                                    "190142190" OR  "190142090" OR
                                    "190142290" OR  "190141590" OR
                                    "190108790" OR  "190108890" OR
                                    "190109090" OR
                                    "190153190" OR  "190153090" OR
                                    "190153390" OR  "190153290" OR
                                    "190161470" OR  "190161570" OR
                                    "190161870" OR  "190162070" OR
                                    "190161670" OR  "190161770" OR
                                    "190161970" OR  "190162170" OR
                                    "190182990" OR
                                    "190198570" OR  "190198670" OR
                                    "190213790" OR  "190214290" OR
                                    "190213890" OR  "190214090" OR
                                    "190214390" OR  "190213990" OR
                                    "190214190"                 )
                 AND   (WRK-SHOKIKSN(IDX1)   NOT =         SPACE)
                   ADD      1                TO  WRK-MEISAISU
                   MOVE   WRK-SHOKIKSN(IDX1) TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "D"           TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                 ELSE
                   MOVE   SPACE          TO  WRK-SHOKIKSN(IDX1)
                 END-IF
               END-IF
      *
               IF   LNK-SROUNYUIN-SANTEIYM   >=   "201604"
      *          ¥«∏Ó…¨Õ◊≈Ÿ≤√ªª£±° ∞Ï»Ã…¬≈Ô∆˛±°¥À‹Œ¡°À(9017-4)
                 IF       WRK-201604-KSN1 (IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-201604-KSN1     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                 END-IF
      *          ¥«∏Ó…¨Õ◊≈Ÿ≤√ªª£≤° ∞Ï»Ã…¬≈Ô∆˛±°¥À‹Œ¡°À(9017-4)
                 IF       WRK-201604-KSN2 (IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-201604-KSN2     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                 END-IF
      *          ¥«∏Ó…¨Õ◊≈Ÿ≤√ªª£≥° ∞Ï»Ã…¬≈Ô∆˛±°¥À‹Œ¡°À(9017-4)
                 IF       WRK-201604-KSN3 (IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-201604-KSN3     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                 END-IF
               ELSE
      *          ¥«∏Ó…¨Õ◊≈Ÿ≤√ªª£±° ∞Ï»Ã…¬≈Ô∆˛±°¥À‹Œ¡°À(9017-4)
                 IF       WRK-201204-KSN4 (IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-201204-KSN4     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                 END-IF
      *          ¥«∏Ó…¨Õ◊≈Ÿ≤√ªª£≤° ∞Ï»Ã…¬≈Ô∆˛±°¥À‹Œ¡°À(9017-4)
                 IF       WRK-201204-KSN5 (IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-201204-KSN5     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                 END-IF
               END-IF
      *        ∞Ï»Ã…¬≈Ô¥«∏Ó…¨Õ◊≈Ÿ…æ≤¡≤√ªª° ∞Ï»Ã…¬≈Ô∆˛±°¥À‹Œ¡°À(9017-6)
               IF         WRK-201004-KSN13(IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   IF    LNK-SROUNYUIN-SANTEIYM   >=   "201307"
                     IF     FLG-KENPOJYUNKYO  =  1
                       MOVE  CONS-201004-KSN13 TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                     ELSE
                       MOVE  CONS-201307-KSN1  TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                     END-IF
                   ELSE
                    MOVE  CONS-201004-KSN13    TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   END-IF
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ¿∫ø¿ ›∑Ú °ª„ªŒ«€√÷≤√ªª° ¿∫ø¿…¬≈Ô∆˛±°¥À‹Œ¡°À(9024-2)
               IF         WRK-201404-KSN3 (IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-201404-KSN3     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        Ω≈≈Ÿ«ß√Œæ…≤√ªª° ¿∫ø¿…¬≈Ô∆˛±°¥À‹Œ¡°À(9024-3)
               IF         WRK-201004-KSN15(IDX1)      NOT =   ZERO
               AND        LNK-SROUNYUIN-SANTEIYM      >=  "201804"
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-201004-KSN15    TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
               IF   LNK-SROUNYUIN-SANTEIYM   >=   "201604"
      *          ¥«∏Ó…¨Õ◊≈Ÿ≤√ªª£±° ∆√ƒÍµ°«Ω…¬±°∆˛±°¥À‹Œ¡°À(9026-3)
                 IF       WRK-201604-KSN4 (IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-201604-KSN4     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                 END-IF
      *          ¥«∏Ó…¨Õ◊≈Ÿ≤√ªª£≤° ∆√ƒÍµ°«Ω…¬±°∆˛±°¥À‹Œ¡°À(9026-3)
                 IF       WRK-201604-KSN5 (IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-201604-KSN5     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                 END-IF
      *          ¥«∏Ó…¨Õ◊≈Ÿ≤√ªª£≥° ∆√ƒÍµ°«Ω…¬±°∆˛±°¥À‹Œ¡°À(9026-3)
                 IF       WRK-201604-KSN6 (IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-201604-KSN6     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                 END-IF
               ELSE
      *          ¥«∏Ó…¨Õ◊≈Ÿ≤√ªª£±° ∆√ƒÍµ°«Ω…¬±°∆˛±°¥À‹Œ¡°À(9026-3)
                 IF       WRK-201204-KSN8 (IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-201204-KSN8     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                 END-IF
      *          ¥«∏Ó…¨Õ◊≈Ÿ≤√ªª£≤° ∆√ƒÍµ°«Ω…¬±°∆˛±°¥À‹Œ¡°À(9026-3)
                 IF       WRK-201204-KSN9 (IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-201204-KSN9     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                 END-IF
               END-IF
      *        Ω≈≈Ÿ«ß√Œæ…≤√ªª° ∆√ƒÍµ°«Ω…¬±°¿∫ø¿…¬≈Ô∆˛±°¥À‹Œ¡°À(9029-2)
               IF         WRK-200804-KSN7(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN7     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
               IF   LNK-SROUNYUIN-SANTEIYM   >=   "201604"
      *          ¥«∏Ó…¨Õ◊≈Ÿ≤√ªª£±° ¿ÏÃÁ…¬±°∆˛±°¥À‹Œ¡°À(9030-3)
                 IF       WRK-201604-KSN7 (IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-201604-KSN7     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                 END-IF
      *          ¥«∏Ó…¨Õ◊≈Ÿ≤√ªª£≤° ¿ÏÃÁ…¬±°∆˛±°¥À‹Œ¡°À(9030-3)
                 IF       WRK-201604-KSN8 (IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-201604-KSN8     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                 END-IF
      *          ¥«∏Ó…¨Õ◊≈Ÿ≤√ªª£≥° ¿ÏÃÁ…¬±°∆˛±°¥À‹Œ¡°À(9030-3)
                 IF       WRK-201604-KSN9 (IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-201604-KSN9     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                 END-IF
               ELSE
      *          ¥«∏Ó…¨Õ◊≈Ÿ≤√ªª£±° ¿ÏÃÁ…¬±°∆˛±°¥À‹Œ¡°À(9030-3)
                 IF       WRK-201204-KSN10(IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-201204-KSN10    TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                 END-IF
      *          ¥«∏Ó…¨Õ◊≈Ÿ≤√ªª£≤° ¿ÏÃÁ…¬±°∆˛±°¥À‹Œ¡°À(9030-3)
                 IF       WRK-201204-KSN11(IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-201204-KSN11    TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                 END-IF
               END-IF
      *        ∞Ï»Ã…¬≈Ô¥«∏Ó…¨Õ◊≈Ÿ…æ≤¡≤√ªª° ¿ÏÃÁ…¬±°∆˛±°¥À‹Œ¡°À(9030-5)
               IF         WRK-201004-KSN17(IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   IF    LNK-SROUNYUIN-SANTEIYM   >=   "201307"
                     IF     FLG-KENPOJYUNKYO  =  1
                       MOVE  CONS-201004-KSN17 TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                     ELSE
                       MOVE  CONS-201307-KSN3  TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                     END-IF
                   ELSE
                    MOVE  CONS-201004-KSN17    TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   END-IF
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ÃÎ¥÷∂€µﬁ¬Œ¿©≥Œ ›≤√ªª° Õ≠æ≤ø«Œ≈ΩÍ∆˛±°¥À‹Œ¡°À(9039-1)
               IF         WRK-200804-KSN1(IDX1)   NOT =   ZERO
                   ADD      1                 TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN1  TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"            TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ∞Âª’«€√÷≤√ªª£±° Õ≠æ≤ø«Œ≈ΩÍ∆˛±°¥À‹Œ¡°À(9039-2)
               IF         WRK-201004-KSN12(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-201004-KSN12    TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ∞Âª’«€√÷≤√ªª£≤° Õ≠æ≤ø«Œ≈ΩÍ∆˛±°¥À‹Œ¡°À(9039-2)
               IF         WRK-200804-KSN2(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN2     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ¥«∏Ó«€√÷≤√ªª£±° Õ≠æ≤ø«Œ≈ΩÍ∆˛±°¥À‹Œ¡°À(9039-3)
               IF         WRK-200804-KSN4(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN4     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ¥«∏Ó«€√÷≤√ªª£≤° Õ≠æ≤ø«Œ≈ΩÍ∆˛±°¥À‹Œ¡°À(9039-3)
               IF         WRK-200804-KSN3(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN3     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ÃÎ¥÷¥«∏Ó«€√÷≤√ªª£±° Õ≠æ≤ø«Œ≈ΩÍ∆˛±°¥À‹Œ¡°À(9039-4)
               IF         WRK-200804-KSN6(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN6     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ÃÎ¥÷¥«∏Ó«€√÷≤√ªª£≤° Õ≠æ≤ø«Œ≈ΩÍ∆˛±°¥À‹Œ¡°À(9039-4)
               IF         WRK-200804-KSN5(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN5     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ¥«∏Ó ‰Ωı«€√÷≤√ªª£±° Õ≠æ≤ø«Œ≈ΩÍ∆˛±°¥À‹Œ¡°À(9039-5)
               IF         WRK-201404-KSN4 (IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-201404-KSN4     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ¥«∏Ó ‰Ωı«€√÷≤√ªª£≤° Õ≠æ≤ø«Œ≈ΩÍ∆˛±°¥À‹Œ¡°À(9039-5)
               IF         WRK-201404-KSN5 (IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-201404-KSN5     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ±…Õ‹¥…Õ˝º¬ª‹≤√ªª° Õ≠æ≤ø«Œ≈ΩÍ∆˛±°¥À‹Œ¡°À(9039-6)
               IF         WRK-201404-KSN6 (IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-201404-KSN6     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        Õ≠æ≤ø«Œ≈ΩÍ∫ﬂ¬…¸µ¢µ°«Ω∂Ø≤Ω≤√ªª° Õ≠æ≤ø«Œ≈ΩÍ∆˛±°¥À‹Œ¡°À(9039-7)
               IF         WRK-201604-KSN10(IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-201604-KSN10    TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ÍÛ·Ï…æ≤¡º¬ª‹≤√ªª° Œ≈Õ‹…¬≈Ô°À(9058-1)
               IF         WRK-200804-KSN8(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN8     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ÍÛ·Ï¬–∫ˆ≤√ªª£≤° Œ≈Õ‹…¬≈Ô°À(9058-1)
               IF         WRK-201804-KSN1(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-201804-KSN1     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ÍÛ·Ï…æ≤¡º¬ª‹≤√ªª° Õ≠æ≤ø«Œ≈ΩÍŒ≈Õ‹…¬æ≤∆˛±°¥À‹Œ¡°À(9085-1)
               IF         WRK-200804-KSN9(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-200804-KSN9     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ÍÛ·Ï¬–∫ˆ≤√ªª£≤° Õ≠æ≤ø«Œ≈ΩÍŒ≈Õ‹…¬æ≤∆˛±°¥À‹Œ¡°À(9085-1)
               IF         WRK-201804-KSN4(IDX1)     NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-201804-KSN4     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ±…Õ‹¥…Õ˝º¬ª‹≤√ªª° Õ≠æ≤ø«Œ≈ΩÍŒ≈Õ‹…¬æ≤∆˛±°¥À‹Œ¡°À(9085-2)
               IF         WRK-201404-KSN7 (IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-201404-KSN7     TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        Õ≠æ≤ø«Œ≈ΩÍŒ≈Õ‹…¬æ≤∫ﬂ¬…¸µ¢µ°«Ω∂Ø≤Ω≤√ªª  (9085-3)
               IF         WRK-201604-KSN11(IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   MOVE   CONS-201604-KSN11    TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        Ω≈≈Ÿ«ß√Œæ…≤√ªª° ¿∫ø¿…¬≈Ô∆˛±°¥À‹Œ¡°À(0000-0)
               IF         WRK-201004-KSN15(IDX1)      NOT =   ZERO
               AND        LNK-SROUNYUIN-SANTEIYM      <=  "201803"
                   ADD      1                  TO  WRK-MEISAISU
                   IF    LNK-SROUNYUIN-SANTEIYM   >=   "201307"
                     IF     FLG-KENPOJYUNKYO  =  1
                       MOVE  CONS-201004-KSN15 TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                     ELSE
                       MOVE  CONS-201307-KSN2  TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                     END-IF
                   ELSE
                    MOVE  CONS-201004-KSN15    TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   END-IF
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        À˝¿≠∞›ª˝∆©¿œ¥…Õ˝≤√ªª° Œ≈Õ‹…¬≈Ô∆˛±°¥À‹Œ¡°À(0000-0)
               IF         WRK-201404-KSN1 (IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   IF     FLG-KENPOJYUNKYO  =  1
                       MOVE  CONS-201404-KSN1  TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   ELSE
                       MOVE  CONS-201404-KSN8  TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   END-IF
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ∫ﬂ¬…¸µ¢µ°«Ω∂Ø≤Ω≤√ªª° Œ≈Õ‹…¬≈Ô∆˛±°¥À‹Œ¡°À(0000-0)
               IF         WRK-201404-KSN2 (IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   IF     FLG-KENPOJYUNKYO  =  1
                     MOVE   CONS-201404-KSN2   TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   ELSE
                     MOVE   CONS-201404-KSN9   TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   END-IF
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ±…Õ‹¥…Õ˝¬Œ¿©∏∫ªªµ¨ƒÍ≥∫≈ˆ° ∆˛±°¥À‹Œ¡°À(0000-0)
               IF         WRK-201407-KSN1 (IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   IF     FLG-KENPOJYUNKYO  =  1
                       MOVE  CONS-201407-KSN1  TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   ELSE
                       MOVE  CONS-201407-KSN3  TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   END-IF
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ±…Õ‹¥…Õ˝¬Œ¿©∏∫ªªµ¨ƒÍ≥∫≈ˆ° ∆√ƒÍ∆˛±°Œ¡µ⁄§”√ª¥¸¬⁄∫ﬂºÍΩ—≈˘¥À‹Œ¡£≤°À(0000-0)
               IF         WRK-201407-KSN2 (IDX1)      NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   IF     FLG-KENPOJYUNKYO  =  1
                       MOVE  CONS-201407-KSN2  TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   ELSE
                       MOVE  CONS-201407-KSN4  TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   END-IF
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *        ÃÎ¥÷¥«∏Ó≤√ªª(0000-0))
               IF         WRK-201804-KSN5(IDX1)       NOT =   ZERO
                   ADD      1                  TO  WRK-MEISAISU
                   IF     FLG-KENPOJYUNKYO  =  1
                       MOVE  CONS-201804-KSN5  TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   ELSE
                       MOVE  CONS-201804-KSN6  TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   END-IF
                   MOVE    "3"             TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
               END-IF
      *
               IF      (WRK-NYUINKHN(IDX1)   NOT =         SPACE)  AND
                       (WRK-GAIHAKU(IDX1)        =         SPACE)  AND
                       (WRK-GENSAN-IRYO(IDX1)   NOT =      SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-GENSAN3       TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"          TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                   MOVE     1               TO  WRK-GENSAN3-MIJISI
               END-IF
               IF      (WRK-NYUINKHN(IDX1)   NOT =         SPACE)  AND
                       (WRK-GAIHAKU(IDX1)        =         SPACE)  AND
                       (WRK-GENSAN-JYOKU(IDX1)   NOT =     SPACE)
                   ADD      1               TO  WRK-MEISAISU
                   MOVE   WRK-GENSAN4       TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "3"          TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                   MOVE     1               TO  WRK-GENSAN4-MIJISI
               END-IF
      *
      *        œ´∫“æËøÙ° ∆√ƒÍ∆˛±°Œ¡§ŒæÏπÁ§œœ´∫“æËøÙ§ §∑°À
               IF       LNK-SROUNYUIN-SRYKBN(IDX1)    =    "90"
                 IF     FLG-KENPOJYUNKYO  =  1
                   CONTINUE
                 ELSE
                   EVALUATE  LNK-SROUNYUIN-JOUSU-KBN(IDX1)
                       WHEN  "1"
                           ADD      1           TO  WRK-MEISAISU
                           MOVE   "101900010"   TO  LNK-SROUNYUIN-SRYCD
                                               (IDX1 WRK-MEISAISU)
                           MOVE    "4"      TO  LNK-SROUNYUIN-SRYCD-SK
                                               (IDX1 WRK-MEISAISU)
                       WHEN  "2"
                           ADD      1           TO  WRK-MEISAISU
                           MOVE   "101900020"   TO  LNK-SROUNYUIN-SRYCD
                                               (IDX1 WRK-MEISAISU)
                           MOVE    "4"      TO  LNK-SROUNYUIN-SRYCD-SK
                                               (IDX1 WRK-MEISAISU)
                   END-EVALUATE
                 END-IF
               END-IF
      *
      *        ΩÈ¥¸≤√ªª° ƒπ¥¸∏∫ªª°À
               IF      FLG-KENPOJYUNKYO  = ZERO
                 IF    (WRK-GAIHAKU(IDX1)        =
                                     SPACE      OR
                                    "190142190" OR  "190142090" OR
                                    "190142290" OR  "190141590" OR
                                    "190108790" OR  "190108890" OR
                                    "190109090" OR
                                    "190153190" OR  "190153090" OR
                                    "190153390" OR  "190153290" OR
                                    "190161470" OR  "190161570" OR
                                    "190161870" OR  "190162070" OR
                                    "190161670" OR  "190161770" OR
                                    "190161970" OR  "190162170" OR
                                    "190182990" OR
                                    "190198570" OR  "190198670" OR
                                    "190213790" OR  "190214290" OR
                                    "190213890" OR  "190214090" OR
                                    "190214390" OR  "190213990" OR
                                    "190214190"                 )
                 AND   (WRK-SHOKIKSN(IDX1)   NOT =         SPACE)
                   ADD      1                TO  WRK-MEISAISU
                   MOVE   WRK-SHOKIKSN(IDX1) TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "5"           TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                 END-IF
               END-IF
      *
      *        …∏∑Á
               IF       WRK-HYOKETU(IDX1)    NOT =         SPACE
                 IF    (WRK-GAIHAKU(IDX1)        =
                                     SPACE      OR
                                    "190142190" OR  "190142090" OR
                                    "190142290" OR  "190141590" OR
                                    "190108790" OR  "190108890" OR
                                    "190109090" OR
                                    "190153190" OR  "190153090" OR
                                    "190153390" OR  "190153290" OR
                                    "190161470" OR  "190161570" OR
                                    "190161870" OR  "190162070" OR
                                    "190161670" OR  "190161770" OR
                                    "190161970" OR  "190162170" OR
                                    "190182990" OR
                                    "190198570" OR  "190198670" OR
                                    "190213790" OR  "190214290" OR
                                    "190213890" OR  "190214090" OR
                                    "190214390" OR  "190213990" OR
                                    "190214190"                 )
                 AND   (WRK-NYUINKHN(IDX1)   NOT =       SPACE  )
                   ADD      1                TO  WRK-MEISAISU
                   MOVE   WRK-HYOKETU(IDX1)  TO  LNK-SROUNYUIN-SRYCD
                                           (IDX1 WRK-MEISAISU)
                   MOVE    "6"           TO  LNK-SROUNYUIN-SRYCD-SK
                                           (IDX1 WRK-MEISAISU)
                 ELSE
                   MOVE   SPACE              TO  WRK-HYOKETU(IDX1)
                 END-IF
               END-IF
           END-PERFORM
      *
      *    ∫∆πΩ√€§∑§øœ´∫“∆˛±°≤Ò∑◊æ Û§´§È≈¿øÙ§ŒªªƒÍ
           PERFORM      VARYING    IDX1   FROM   1  BY  1
                        UNTIL      IDX1   >   31
      *
               MOVE     ZERO       TO       WRK-HYOKETU-TEN
                                            WRK-ZAITEN-HYOKETU
                                            WRK-ZAITEN-HYOKETU2
               IF   WRK-HYOKETU(IDX1)   =   SPACE
      *          ≈¿øÙ•ﬁ•π•ø§Œ∆…§ﬂπ˛§ﬂ
                 PERFORM   VARYING   IDX2    FROM   1  BY   1
                      UNTIL (LNK-SROUNYUIN-SRYCD(IDX1 IDX2) = SPACE)
                         OR (IDX2    >      20)
                   IF    FLG-KENPOJYUNKYO  =  ZERO
                     PERFORM    210-TENSU-KEISAN-SEC
                   ELSE
                     PERFORM    210X-TENSU-KEISAN-SEC
                   END-IF
                 END-PERFORM
                 IF      FLG-KENPOJYUNKYO  =  1
                   COMPUTE  LNK-SROUNYUIN-ZAITEN(IDX1)  =
                            WRK-ZAITEN-HYOKETU2         +  0.5
                 END-IF
               ELSE
      *          ≈¿øÙ•ﬁ•π•ø§Œ∆…§ﬂπ˛§ﬂ
                 INITIALIZE                          TNS-TENSU-REC
                 MOVE    WRK-HYOKETU(IDX1)       TO  TNS-SRYCD
                 MOVE    LNK-SROUNYUIN-HOSPNUM   TO  TNS-HOSPNUM
                 MOVE    LNK-SROUNYUIN-SANTEIYM  TO  TNS-YUKOSTYMD(1:6)
                 MOVE    "01"                    TO  TNS-YUKOSTYMD(7:2)
                 MOVE    LNK-SROUNYUIN-SANTEIYM  TO  TNS-YUKOEDYMD(1:6)
                 MOVE    "01"                    TO  TNS-YUKOEDYMD(7:2)
                 PERFORM 910-TENSU-READ-SEC
                 MOVE    TNS-TEN                 TO  WRK-HYOKETU-TEN
      *
                 PERFORM   VARYING   IDX2    FROM   1  BY   1
                      UNTIL (LNK-SROUNYUIN-SRYCD(IDX1 IDX2) = SPACE)
                         OR (IDX2    >      20)
                   IF    FLG-KENPOJYUNKYO  =  ZERO
                     PERFORM    210-TENSU-KEISAN-HYOKETU-SEC
                   ELSE
                     PERFORM    210X-TENSU-KEISAN-HYOKETU-SEC
                   END-IF
                 END-PERFORM
                 IF      FLG-KENPOJYUNKYO  =  1
                   COMPUTE  LNK-SROUNYUIN-ZAITEN(IDX1)  =
                            WRK-ZAITEN-HYOKETU2         +  0.5
                 END-IF
               END-IF
      *
               PERFORM   VARYING   KSN-IDX   FROM   1  BY   1
                         UNTIL (WRK-NYUKSN(KSN-IDX)  =  SPACE)  OR
                               (KSN-IDX    >      20)
                  IF    WRK-NYUKSN-DAY(KSN-IDX IDX1)  =  ZERO
                      CONTINUE
                  ELSE
                      ADD  WRK-NYUKSN-ZAITEN(KSN-IDX)
                                 TO  LNK-SROUNYUIN-ZAITEN-NACCT(IDX1)
                  END-IF
               END-PERFORM
      *
           END-PERFORM
      *
           .
       210-ROUNYUIN-SET4-EXT.
           EXIT.
      *
      *****************************************************************
      *    ∆˛±°ø«Œ≈π‘∞ŸºË∆¿
      *****************************************************************
       210-NYUINACT-GET-SEC            SECTION.
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM       TO  NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-NYUINACT-SELECT-SEC
           IF      FLG-NYUINACT        =   ZERO
               MOVE     "tbl_nyuinact" TO  MCP-TABLE
               MOVE     "key9"         TO  MCP-PATHNAME
               PERFORM   910-NYUINACT-READ-SEC
           END-IF
      *
      *    ∆˛±°π‘∞Ÿ•Ø•Ì°º•∫
           MOVE     "tbl_nyuinact"     TO  MCP-TABLE
           MOVE     "key9"             TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       210-NYUINACT-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ≥∞«Ò¡∞ΩËÕ˝
      *****************************************************************
       210-GAIHAKU-MAE-SEC            SECTION.
      *
      *    ≥∞«Ò•≥°º•… ‘Ω∏Õ—§À∆˛±°¥À‹Œ¡≤√ªª∂Ë ¨§»ø«Œ≈∂Ë ¨§Œ¬‡»Ú
           INITIALIZE                          TNS-TENSU-REC
           MOVE    NSRY-SRYCD(1)           TO  TNS-SRYCD
           MOVE    LNK-SROUNYUIN-HOSPNUM   TO  TNS-HOSPNUM
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO  TNS-YUKOSTYMD(1:6)
           MOVE    "01"                    TO  TNS-YUKOSTYMD(7:2)
           MOVE    LNK-SROUNYUIN-SANTEIYM  TO  TNS-YUKOEDYMD(1:6)
           MOVE    "01"                    TO  TNS-YUKOEDYMD(7:2)
           PERFORM 910-TENSU-READ-SEC
           IF  (LNK-SROUNYUIN-SANTEIYM >= "202004")
           AND (TNS-SRYCD           NOT = SPACE   )
           AND (TNS-NYUKHNKSNKBN        = ZERO    )
             INITIALIZE                        SNTENSU-AREA
             MOVE  TNS-SRYCD               TO  SNTENSU-SRYCD
             MOVE  LNK-SROUNYUIN-SANTEIYM  TO  SNTENSU-BASEYMD(1:6)
             MOVE  "01"                    TO  SNTENSU-BASEYMD(7:2)
             CALL  "ORCSNTENSU"            USING
                                           SNTENSU-AREA
                                           TNS-TENSU-REC
                                           SPA-AREA
           END-IF
           MOVE    TNS-NYUKHNKSNKBN        TO  WRK-KSNKBN
           MOVE    TNS-CDKBN-KBNNUM        TO  WRK-CDKBN-KBNNUM
           MOVE    TNS-CHUKSNCD            TO  WRK-CHUKSNCD-XXX
           MOVE    TNS-NYUKHNKBN           TO  WRK-NYUKHNKBN-XXX
      *
           .
       210-GAIHAKU-MAE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ≥∞«Òø«Œ≈•≥°º•…ºË∆¿
      *****************************************************************
       210-GAIHAKU-SET-SEC            SECTION.
      *
           MOVE   NACCT-DAY(IDX1)     TO    LNK-SROUNYUIN-GAIHAKU(IDX1)
      *
           EVALUATE   LNK-SROUNYUIN-SRYKBN(IDX1)
              WHEN     "90"
               IF   WRK-DAY-SENTEI(IDX1)  NOT =  ZERO
                 EVALUATE   NACCT-DAY(IDX1)
                    WHEN     1
                      MOVE   SPACE         TO  WRK-NYUINKHN(IDX1)
                      MOVE   "190107290"   TO  WRK-GAIHAKU(IDX1)
                    WHEN     3
                      MOVE   "190111590"   TO  WRK-GAIHAKU(IDX1)
                    WHEN     4
                    WHEN     5
                    WHEN     6
                    WHEN     7
                    WHEN    14
                    WHEN    15
                    WHEN    16
                      PERFORM      2101-GAIHAKU-TAKIKAN-SEN-SEC
                    WHEN     8
                    WHEN     9
      ***           WHEN    11
                    WHEN    13
                    WHEN    17
                      PERFORM      2101-GAIHAKU-GENSAN-SEC
                 END-EVALUATE
               ELSE
                 EVALUATE   NACCT-DAY(IDX1)
                    WHEN     1
                      MOVE   "190107290"   TO  WRK-GAIHAKU(IDX1)
                    WHEN     2
                      MOVE   "190107390"   TO  WRK-GAIHAKU(IDX1)
                    WHEN     4
                    WHEN     5
                    WHEN     6
                    WHEN     7
                    WHEN    14
                    WHEN    15
                    WHEN    16
                      IF     FLG-KENPOJYUNKYO  = ZERO
                         MOVE      1       TO  WRK-TAIJYUSIN-ERR
                      ELSE
                         PERFORM   2101-GAIHAKU-TAKIKAN-SEC
                      END-IF
                    WHEN     8
                    WHEN     9
      ***           WHEN    11
                    WHEN    13
                    WHEN    17
                      IF    (FLG-KENPOJYUNKYO  = ZERO          )
                      AND   (NACCT-DAY(IDX1)   = 11 OR 13 OR 17)
                         MOVE      1       TO  WRK-TAIJYUSIN-ERR
                      ELSE
                         PERFORM   2101-GAIHAKU-GENSAN-SEC
                      END-IF
                 END-EVALUATE
               END-IF
              WHEN     "92"
               IF   WRK-DAY-SENTEI(IDX1)  NOT =  ZERO
                 EVALUATE   NACCT-DAY(IDX1)
                    WHEN     1
                      MOVE   SPACE         TO  WRK-NYUINKHN(IDX1)
                      MOVE   "190107490"   TO  WRK-GAIHAKU(IDX1)
                    WHEN     3
                      MOVE   "190111590"   TO  WRK-GAIHAKU(IDX1)
                    WHEN     4
                    WHEN     5
                    WHEN     6
                    WHEN     7
                    WHEN    14
                    WHEN    15
                    WHEN    16
                      PERFORM      2101-GAIHAKU-TAKIKAN-SEN-SEC
                    WHEN     8
                    WHEN     9
      ***           WHEN    11
                    WHEN    13
                    WHEN    17
                      PERFORM      2101-GAIHAKU-GENSAN-SEC
                 END-EVALUATE
               ELSE
                 EVALUATE   NACCT-DAY(IDX1)
                    WHEN     1
                      MOVE   "190107490"   TO  WRK-GAIHAKU(IDX1)
                    WHEN     2
                      MOVE   "190107590"   TO  WRK-GAIHAKU(IDX1)
                    WHEN     4
                    WHEN     5
                    WHEN     6
                    WHEN     7
                    WHEN    14
                    WHEN    15
                    WHEN    16
                      IF     FLG-KENPOJYUNKYO  = ZERO
                         MOVE      1       TO  WRK-TAIJYUSIN-ERR
                      ELSE
                         PERFORM   2101-GAIHAKU-TAKIKAN-SEC
                      END-IF
                    WHEN     8
                    WHEN     9
      ***           WHEN    11
                    WHEN    13
                    WHEN    17
                      IF    (FLG-KENPOJYUNKYO  = ZERO          )
                      AND   (NACCT-DAY(IDX1)   = 11 OR 13 OR 17)
                         MOVE      1       TO  WRK-TAIJYUSIN-ERR
                      ELSE
                         PERFORM   2101-GAIHAKU-GENSAN-SEC
                      END-IF
                 END-EVALUATE
               END-IF
           END-EVALUATE
      *
           IF       WRK-GAIHAKU(IDX1)   =    SPACE
                 MOVE   ZERO    TO    LNK-SROUNYUIN-GAIHAKU(IDX1)
           END-IF
      *
           .
       210-GAIHAKU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ≥∞«Òø«Œ≈•≥°º•…ºË∆¿
      *****************************************************************
       2101-GAIHAKU-TAKIKAN-SEC           SECTION.
      *
           IF       LNK-SROUNYUIN-SANTEIYM  >=  "201804"
                   PERFORM   2101-GAIHAKU-TAKIKAN3-SEC
           ELSE
             IF     LNK-SROUNYUIN-SANTEIYM  >=  "201604"
                   PERFORM   2101-GAIHAKU-TAKIKAN2-SEC
             ELSE
                   PERFORM   2101-GAIHAKU-TAKIKAN1-SEC
             END-IF
           END-IF
      *
           .
       2101-GAIHAKU-TAKIKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ≥∞«Òø«Œ≈•≥°º•…ºË∆¿£±
      *****************************************************************
       2101-GAIHAKU-TAKIKAN1-SEC           SECTION.
      *
           EVALUATE   WRK-NYUKHNKSNKBN(IDX1)
      *
             WHEN   099  THRU  100
               IF   LNK-SROUNYUIN-SANTEIYM   >=   "201004"
                 EVALUATE   NACCT-DAY(IDX1)
                  WHEN     4
                    MOVE   "190108890"    TO   WRK-GAIHAKU(IDX1)
                  WHEN     5
                    MOVE   "190142190"    TO   WRK-GAIHAKU(IDX1)
                  WHEN     6
                  WHEN     7
                    MOVE      1           TO   WRK-TAIJYUSIN-ERR
                 END-EVALUATE
               END-IF
             WHEN   201  THRU  219
             WHEN   221  THRU  222
             WHEN   224
             WHEN   227  THRU  229
             WHEN   230  THRU  239
             WHEN   241  THRU  249
             WHEN   250  THRU  255
      *      (H26-ADD-ST)
             WHEN   256
             WHEN   257  THRU  259
             WHEN   260  THRU  261
             WHEN   262  THRU  265
             WHEN   266  THRU  267
      *      (H26-ADD-ED)
               IF    WRK-NYUKHNKSNKBN(IDX1) = 221 OR 232
                 EVALUATE   NACCT-DAY(IDX1)
                  WHEN     4
                    MOVE   "190108790"    TO   WRK-GAIHAKU(IDX1)
                  WHEN     5
                    MOVE   "190142090"    TO   WRK-GAIHAKU(IDX1)
                  WHEN     6
                    MOVE   "190153190"    TO   WRK-GAIHAKU(IDX1)
                  WHEN     7
                    MOVE   "190153390"    TO   WRK-GAIHAKU(IDX1)
                 END-EVALUATE
               ELSE
                 IF  WRK-NYUKHNKSNKBN(IDX1) =
                         229 OR 238 OR 215 OR 216 OR 217 OR 239
                                                         OR 252
                   EVALUATE   NACCT-DAY(IDX1)
                     WHEN     4
                       MOVE   "190108790" TO   WRK-GAIHAKU(IDX1)
                     WHEN     5
                       MOVE   "190142090" TO   WRK-GAIHAKU(IDX1)
                     WHEN     6
                       MOVE   "190153190" TO   WRK-GAIHAKU(IDX1)
                     WHEN     7
                       MOVE      1        TO   WRK-TAIJYUSIN-ERR
                   END-EVALUATE
                 ELSE
                   EVALUATE   NACCT-DAY(IDX1)
                     WHEN     4
                       MOVE   "190108790" TO   WRK-GAIHAKU(IDX1)
                     WHEN     5
                       MOVE   "190142090" TO   WRK-GAIHAKU(IDX1)
                     WHEN     6
                     WHEN     7
                       MOVE      1        TO   WRK-TAIJYUSIN-ERR
                   END-EVALUATE
                 END-IF
               END-IF
             WHEN   013  THRU  021
             WHEN   101  THRU  109
             WHEN   147  THRU  152
             WHEN   173  THRU  190
             WHEN   196
      *      (H26-ADD-ST)
             WHEN   199
             WHEN   004  THRU  008
             WHEN   010  THRU  012
      *      (H26-ADD-ED)
               EVALUATE   NACCT-DAY(IDX1)
                  WHEN     4
                    MOVE   "190108890"    TO   WRK-GAIHAKU(IDX1)
                  WHEN     5
                    MOVE   "190142190"    TO   WRK-GAIHAKU(IDX1)
                  WHEN     6
                  WHEN     7
                    MOVE      1           TO   WRK-TAIJYUSIN-ERR
               END-EVALUATE
             WHEN   075  THRU  080
               EVALUATE   NACCT-DAY(IDX1)
                  WHEN     4
                    MOVE   "190108990"    TO   WRK-GAIHAKU(IDX1)
               END-EVALUATE
             WHEN   085  THRU  086
             WHEN   132  THRU  133
             WHEN   153  THRU  158
               IF    WRK-NYUKHNKSNKBN(IDX1) =
                         153 OR 154 OR 155 OR 156 OR 157 OR 158
                 EVALUATE   NACCT-DAY(IDX1)
                  WHEN     4
                    MOVE   "190109090"    TO   WRK-GAIHAKU(IDX1)
                  WHEN     5
                    MOVE   "190142290"    TO   WRK-GAIHAKU(IDX1)
                  WHEN     6
                    MOVE      1           TO   WRK-TAIJYUSIN-ERR
                  WHEN     7
                    MOVE   "190153290"    TO   WRK-GAIHAKU(IDX1)
                 END-EVALUATE
               ELSE
                 EVALUATE   NACCT-DAY(IDX1)
                  WHEN     4
                    MOVE   "190109090"    TO   WRK-GAIHAKU(IDX1)
                  WHEN     5
                    MOVE   "190142290"    TO   WRK-GAIHAKU(IDX1)
                  WHEN     6
                  WHEN     7
                    MOVE      1           TO   WRK-TAIJYUSIN-ERR
                 END-EVALUATE
               END-IF
             WHEN   226
             WHEN   240
               IF        LNK-SROUNYUIN-SANTEIYM   >=  "201004"
                       MOVE    1    TO    WRK-TAIJYUSIN-ERR
               END-IF
             WHEN   031
             WHEN   033
             WHEN   034
             WHEN   035
             WHEN   038
             WHEN   170
             WHEN   191
      *      (H26-ADD-ST)
             WHEN   017  THRU  020
      *      (H26-ADD-ED)
               EVALUATE   NACCT-DAY(IDX1)
                  WHEN     4
                  WHEN     6
                    IF   LNK-SROUNYUIN-SANTEIYM   >=  "201004"
                       MOVE    1          TO   WRK-TAIJYUSIN-ERR
                    END-IF
                  WHEN     5
                    MOVE   "190141590"    TO   WRK-GAIHAKU(IDX1)
                  WHEN     7
                    MOVE   "190153090"    TO   WRK-GAIHAKU(IDX1)
               END-EVALUATE
             WHEN   022
             WHEN   023
             WHEN   024
             WHEN   025
             WHEN   026
             WHEN   029
             WHEN   136
             WHEN   168
             WHEN   169
      *      (H26-ADD-ST)
             WHEN   013  THRU  016
      *      (H26-ADD-ED)
               EVALUATE   NACCT-DAY(IDX1)
                  WHEN     4
                  WHEN     6
                    IF   LNK-SROUNYUIN-SANTEIYM   >=  "201004"
                       MOVE    1          TO   WRK-TAIJYUSIN-ERR
                    END-IF
                  WHEN     5
                    MOVE   "190141590"    TO   WRK-GAIHAKU(IDX1)
                  WHEN     7
                    MOVE   "190153090"    TO   WRK-GAIHAKU(IDX1)
               END-EVALUATE
             WHEN   081
             WHEN   083
             WHEN   172
      *      (H26-ADD-ST)
             WHEN   032  THRU  037
      *      (H26-ADD-ED)
               EVALUATE   NACCT-DAY(IDX1)
                  WHEN     4
                  WHEN     6
                    IF   LNK-SROUNYUIN-SANTEIYM   >=  "201004"
                       MOVE    1          TO   WRK-TAIJYUSIN-ERR
                    END-IF
                  WHEN     5
                    MOVE   "190141590"    TO   WRK-GAIHAKU(IDX1)
                  WHEN     7
                    MOVE   "190153090"    TO   WRK-GAIHAKU(IDX1)
               END-EVALUATE
             WHEN   OTHER
               EVALUATE   NACCT-DAY(IDX1)
                  WHEN     4
                  WHEN     6
                  WHEN     7
                    IF   LNK-SROUNYUIN-SANTEIYM   >=  "201004"
                       MOVE    1          TO   WRK-TAIJYUSIN-ERR
                    END-IF
                  WHEN     5
                    MOVE   "190141590"    TO   WRK-GAIHAKU(IDX1)
               END-EVALUATE
      *
           END-EVALUATE
      *
           .
       2101-GAIHAKU-TAKIKAN1-EXT.
           EXIT.
      *
      *****************************************************************
      *    ≥∞«Òø«Œ≈•≥°º•…ºË∆¿£≤  201604
      *****************************************************************
       2101-GAIHAKU-TAKIKAN2-SEC           SECTION.
      *
           IF   NACCT-DAY(IDX1)  =   7
             MOVE    1    TO    WRK-TAIJYUSIN-ERR
           END-IF
           EVALUATE   WRK-NYUKHNKSNKBN(IDX1)
      *      ∆√ƒÍ∆˛±°Œ¡
             WHEN   201  THRU  206
             WHEN   208  THRU  210
             WHEN   212  THRU  217
             WHEN   221
             WHEN   227  THRU  229
             WHEN   232  THRU  236
             WHEN   238  THRU  239
             WHEN   241  THRU  249
             WHEN   250  THRU  254
             WHEN   257  THRU  267
      *      --- H28.4 TUIKA ---
             WHEN   268  THRU  270
      *        «ß√Œæ…º£Œ≈…¬≈Ô∆˛±°Œ¡°¢¿∫ø¿Œ≈Õ‹…¬≈Ô∆˛±°Œ¡
      *        √œ∞Ë∞‹π‘µ°«Ω∂Ø≤Ω…¬≈Ô∆˛±°Œ¡
               IF  WRK-NYUKHNKSNKBN(IDX1) = 221 OR 232 OR 217 OR 268
                 EVALUATE   NACCT-DAY(IDX1)
                  WHEN     4
                    MOVE   "190108790"    TO   WRK-GAIHAKU(IDX1)
                  WHEN     5
                    MOVE   "190142090"    TO   WRK-GAIHAKU(IDX1)
                  WHEN     6
                    MOVE   "190182990"    TO   WRK-GAIHAKU(IDX1)
                 END-EVALUATE
               ELSE
                 EVALUATE   NACCT-DAY(IDX1)
                  WHEN     4
                    MOVE   "190108790"    TO   WRK-GAIHAKU(IDX1)
                  WHEN     5
                    MOVE   "190142090"    TO   WRK-GAIHAKU(IDX1)
                  WHEN     6
                    MOVE      1           TO   WRK-TAIJYUSIN-ERR
                 END-EVALUATE
               END-IF
      *      Œ≈Õ‹…¬≈Ô∆˛±°¥À‹Œ¡
             WHEN   004  THRU  008
             WHEN   010  THRU  012
             WHEN   152
             WHEN   173  THRU  190
             WHEN   196
             WHEN   199
      *      --- H28.4 TUIKA ---
             WHEN   048  THRU  049
             WHEN   051  THRU  054
             WHEN   057  THRU  068
               EVALUATE   NACCT-DAY(IDX1)
                WHEN     4
                  MOVE   "190108890"    TO   WRK-GAIHAKU(IDX1)
                WHEN     5
                  MOVE   "190142190"    TO   WRK-GAIHAKU(IDX1)
                WHEN     6
                  MOVE      1           TO   WRK-TAIJYUSIN-ERR
               END-EVALUATE
      *      Õ≠æ≤ø«Œ≈ΩÍŒ≈Õ‹…¬æ≤∆˛±°¥À‹Œ¡
             WHEN   153  THRU  158
               EVALUATE   NACCT-DAY(IDX1)
                WHEN     4
                  MOVE   "190108890"    TO   WRK-GAIHAKU(IDX1)
                WHEN     5
                  MOVE   "190142190"    TO   WRK-GAIHAKU(IDX1)
                WHEN     6
                  MOVE   "190109090"    TO   WRK-GAIHAKU(IDX1)
               END-EVALUATE
      *      √ª¥¸¬⁄∫ﬂºÍΩ—¥À‹Œ¡£≤°¶√ª¥¸¬⁄∫ﬂºÍΩ—¥À‹Œ¡£≥
             WHEN   226
             WHEN   240
                  MOVE      1           TO   WRK-TAIJYUSIN-ERR
             WHEN   OTHER
               EVALUATE   NACCT-DAY(IDX1)
                WHEN     4
                WHEN     6
                  MOVE      1           TO   WRK-TAIJYUSIN-ERR
                WHEN     5
                  MOVE   "190141590"    TO   WRK-GAIHAKU(IDX1)
               END-EVALUATE
           END-EVALUATE
      *
           .
       2101-GAIHAKU-TAKIKAN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ≥∞«Òø«Œ≈•≥°º•…ºË∆¿£≥  201804
      *****************************************************************
       2101-GAIHAKU-TAKIKAN3-SEC           SECTION.
      *
           IF   NACCT-DAY(IDX1)  =   7
             MOVE    1    TO    WRK-TAIJYUSIN-ERR
           END-IF
           EVALUATE   WRK-NYUKHNKSNKBN(IDX1)
      *      ∆√ƒÍ∆˛±°Œ¡
             WHEN   200  THRU  257
             WHEN   260  THRU  263
      *        «ß√Œæ…º£Œ≈…¬≈Ô∆˛±°Œ¡°¢¿∫ø¿Œ≈Õ‹…¬≈Ô∆˛±°Œ¡
      *        √œ∞Ë∞‹π‘µ°«Ω∂Ø≤Ω…¬≈Ô∆˛±°Œ¡
               IF  WRK-NYUKHNKSNKBN(IDX1) = 255 OR 254 OR 257
                 EVALUATE   NACCT-DAY(IDX1)
                  WHEN     4
                    MOVE   "190108790"    TO   WRK-GAIHAKU(IDX1)
                  WHEN     5
                    MOVE   "190142090"    TO   WRK-GAIHAKU(IDX1)
                  WHEN     6
                    MOVE   "190182990"    TO   WRK-GAIHAKU(IDX1)
                  WHEN    14
                    MOVE   "190214390"    TO   WRK-GAIHAKU(IDX1)
                  WHEN    15
                    MOVE   "190213990"    TO   WRK-GAIHAKU(IDX1)
                  WHEN    16
                    MOVE   "190214190"    TO   WRK-GAIHAKU(IDX1)
                 END-EVALUATE
               ELSE
                 EVALUATE   NACCT-DAY(IDX1)
                  WHEN     4
                    MOVE   "190108790"    TO   WRK-GAIHAKU(IDX1)
                  WHEN     5
                    MOVE   "190142090"    TO   WRK-GAIHAKU(IDX1)
                  WHEN     6
                    MOVE      1           TO   WRK-TAIJYUSIN-ERR
                  WHEN    14
                    MOVE   "190214390"    TO   WRK-GAIHAKU(IDX1)
                  WHEN    15
                    MOVE   "190213990"    TO   WRK-GAIHAKU(IDX1)
                  WHEN    16
                    MOVE      1           TO   WRK-TAIJYUSIN-ERR
                 END-EVALUATE
               END-IF
      *      Œ≈Õ‹…¬≈Ô∆˛±°¥À‹Œ¡
             WHEN    11  THRU   14
             WHEN    51
             WHEN   901
               EVALUATE   NACCT-DAY(IDX1)
                WHEN     4
                  MOVE   "190108890"    TO   WRK-GAIHAKU(IDX1)
                WHEN     5
                  MOVE   "190142190"    TO   WRK-GAIHAKU(IDX1)
                WHEN     6
                  MOVE      1           TO   WRK-TAIJYUSIN-ERR
                WHEN    14
                  MOVE   "190214290"    TO   WRK-GAIHAKU(IDX1)
                WHEN    15
                  MOVE   "190213890"    TO   WRK-GAIHAKU(IDX1)
                WHEN    16
                  MOVE      1           TO   WRK-TAIJYUSIN-ERR
               END-EVALUATE
      *      Õ≠æ≤ø«Œ≈ΩÍŒ≈Õ‹…¬æ≤∆˛±°¥À‹Œ¡
             WHEN    26
             WHEN    53
             WHEN   909
               EVALUATE   NACCT-DAY(IDX1)
                WHEN     4
                  MOVE   "190108890"    TO   WRK-GAIHAKU(IDX1)
                WHEN     5
                  MOVE   "190142190"    TO   WRK-GAIHAKU(IDX1)
                WHEN     6
                  MOVE   "190109090"    TO   WRK-GAIHAKU(IDX1)
                WHEN    14
                  MOVE   "190214290"    TO   WRK-GAIHAKU(IDX1)
                WHEN    15
                  MOVE   "190213890"    TO   WRK-GAIHAKU(IDX1)
                WHEN    16
                  MOVE   "190214090"    TO   WRK-GAIHAKU(IDX1)
               END-EVALUATE
      *      √ª¥¸¬⁄∫ﬂºÍΩ—¥À‹Œ¡£≤°¶√ª¥¸¬⁄∫ﬂºÍΩ—¥À‹Œ¡£≥
             WHEN   258  THRU  259
                  MOVE      1           TO   WRK-TAIJYUSIN-ERR
             WHEN   OTHER
               EVALUATE   NACCT-DAY(IDX1)
                WHEN     4
                WHEN     6
                  MOVE      1           TO   WRK-TAIJYUSIN-ERR
                WHEN     5
                  MOVE   "190141590"    TO   WRK-GAIHAKU(IDX1)
                WHEN    14
                  MOVE   "190213790"    TO   WRK-GAIHAKU(IDX1)
                WHEN    15
                WHEN    16
                  MOVE      1           TO   WRK-TAIJYUSIN-ERR
               END-EVALUATE
           END-EVALUATE
      *
           .
       2101-GAIHAKU-TAKIKAN3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ≥∞«Òø«Œ≈•≥°º•…ºË∆¿° ¡™ƒÍŒ≈Õ‹°À
      *****************************************************************
       2101-GAIHAKU-TAKIKAN-SEN-SEC           SECTION.
      *
           IF       LNK-SROUNYUIN-SANTEIYM  >=  "201804"
                   PERFORM   2101-GAIHAKU-TAKIKAN-SEN2-SEC
           ELSE
             IF     LNK-SROUNYUIN-SANTEIYM  >=  "201604"
                   PERFORM   2101-GAIHAKU-TAKIKAN-SEN1-SEC
             END-IF
           END-IF
      *
           .
       2101-GAIHAKU-TAKIKAN-SEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ≥∞«Òø«Œ≈•≥°º•…ºË∆¿° ¡™ƒÍŒ≈Õ‹°À£±  201604
      *****************************************************************
       2101-GAIHAKU-TAKIKAN-SEN1-SEC           SECTION.
      *
           IF   NACCT-DAY(IDX1)  =   7
             MOVE    1    TO    WRK-TAIJYUSIN-ERR
           END-IF
           EVALUATE   WRK-NYUKHNKSNKBN(IDX1)
      *      ∆√ƒÍ∆˛±°Œ¡
             WHEN   201  THRU  206
             WHEN   208  THRU  210
             WHEN   212  THRU  217
             WHEN   221
             WHEN   227  THRU  229
             WHEN   232  THRU  236
             WHEN   238  THRU  239
             WHEN   241  THRU  249
             WHEN   250  THRU  254
             WHEN   257  THRU  267
      *      --- H28.4 TUIKA ---
             WHEN   268  THRU  270
                  MOVE      1           TO   WRK-TAIJYUSIN-ERR
      *      Œ≈Õ‹…¬≈Ô∆˛±°¥À‹Œ¡
             WHEN   004  THRU  008
             WHEN   010  THRU  012
             WHEN   152
             WHEN   173  THRU  190
             WHEN   196
             WHEN   199
      *      --- H28.4 TUIKA ---
             WHEN   048  THRU  049
             WHEN   051  THRU  054
             WHEN   057  THRU  068
                  MOVE      1           TO   WRK-TAIJYUSIN-ERR
      *      Õ≠æ≤ø«Œ≈ΩÍŒ≈Õ‹…¬æ≤∆˛±°¥À‹Œ¡
             WHEN   153  THRU  158
                  MOVE      1           TO   WRK-TAIJYUSIN-ERR
      *      √ª¥¸¬⁄∫ﬂºÍΩ—¥À‹Œ¡£≤°¶√ª¥¸¬⁄∫ﬂºÍΩ—¥À‹Œ¡£≥
             WHEN   226
             WHEN   240
                  MOVE      1           TO   WRK-TAIJYUSIN-ERR
             WHEN   OTHER
               EVALUATE   NACCT-DAY(IDX1)
                WHEN     4
                WHEN     6
                  MOVE      1           TO   WRK-TAIJYUSIN-ERR
                WHEN     5
                  MOVE   "190141590"    TO   WRK-GAIHAKU(IDX1)
               END-EVALUATE
           END-EVALUATE
      *
           .
       2101-GAIHAKU-TAKIKAN-SEN1-EXT.
           EXIT.
      *
      *****************************************************************
      *    ≥∞«Òø«Œ≈•≥°º•…ºË∆¿° ¡™ƒÍŒ≈Õ‹°À£≤  201804
      *****************************************************************
       2101-GAIHAKU-TAKIKAN-SEN2-SEC           SECTION.
      *
           IF   NACCT-DAY(IDX1)  =   7
             MOVE    1    TO    WRK-TAIJYUSIN-ERR
           END-IF
           EVALUATE   WRK-NYUKHNKSNKBN(IDX1)
      *      ∆√ƒÍ∆˛±°Œ¡
             WHEN   200  THRU  257
             WHEN   260  THRU  263
                  MOVE      1           TO   WRK-TAIJYUSIN-ERR
      *      Œ≈Õ‹…¬≈Ô∆˛±°¥À‹Œ¡
             WHEN    11  THRU   14
             WHEN    51
             WHEN   901
                  MOVE      1           TO   WRK-TAIJYUSIN-ERR
      *      Õ≠æ≤ø«Œ≈ΩÍŒ≈Õ‹…¬æ≤∆˛±°¥À‹Œ¡
             WHEN    26
             WHEN    53
             WHEN   909
                  MOVE      1           TO   WRK-TAIJYUSIN-ERR
      *      √ª¥¸¬⁄∫ﬂºÍΩ—¥À‹Œ¡£≤°¶√ª¥¸¬⁄∫ﬂºÍΩ—¥À‹Œ¡£≥
             WHEN   258  THRU  259
                  MOVE      1           TO   WRK-TAIJYUSIN-ERR
             WHEN   OTHER
               EVALUATE   NACCT-DAY(IDX1)
                WHEN     4
                WHEN     6
                  MOVE      1           TO   WRK-TAIJYUSIN-ERR
                WHEN     5
                  MOVE   "190141590"    TO   WRK-GAIHAKU(IDX1)
                WHEN    14
                  MOVE   "190213790"    TO   WRK-GAIHAKU(IDX1)
                WHEN    15
                WHEN    16
                  MOVE      1           TO   WRK-TAIJYUSIN-ERR
               END-EVALUATE
           END-EVALUATE
      *
           .
       2101-GAIHAKU-TAKIKAN-SEN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ∏∫ªª•≥°º•…ºË∆¿
      *****************************************************************
       2101-GAIHAKU-GENSAN-SEC           SECTION.
      *
           IF       LNK-SROUNYUIN-SANTEIYM  >=  "201804"
                   PERFORM   2101-GAIHAKU-GENSAN3-SEC
           ELSE
             IF     LNK-SROUNYUIN-SANTEIYM  >=  "201604"
                   PERFORM   2101-GAIHAKU-GENSAN2-SEC
             ELSE
                   PERFORM   2101-GAIHAKU-GENSAN1-SEC
             END-IF
           END-IF
      *
           .
       2101-GAIHAKU-GENSAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ∏∫ªª•≥°º•…ºË∆¿£±
      *****************************************************************
       2101-GAIHAKU-GENSAN1-SEC           SECTION.
      *
           EVALUATE   NACCT-DAY(IDX1)
             WHEN     8
      ***    WHEN    11
                EVALUATE   WRK-CHUKSNCD(IDX1)
                   WHEN   "9017"
                      IF   LNK-SROUNYUIN-SANTEIYM   >=  "201404"
                        IF  (WRK-NYUKHNKBN(IDX1) =   79 OR 88  )
                        OR  (WRK-NYUINKHN(IDX1)  =  "190799510")
                          MOVE  "190161570" TO   WRK-GAIHAKU(IDX1)
                        ELSE
                          MOVE  "190161470" TO   WRK-GAIHAKU(IDX1)
                        END-IF
                      ELSE
                          MOVE  "190161470" TO   WRK-GAIHAKU(IDX1)
                      END-IF
                   WHEN   "9018"
                      MOVE   "190161570"    TO   WRK-GAIHAKU(IDX1)
                   WHEN   "9026"
                      MOVE   "190161870"    TO   WRK-GAIHAKU(IDX1)
                   WHEN   "9030"
                      MOVE   "190162070"    TO   WRK-GAIHAKU(IDX1)
                   WHEN    OTHER
                      MOVE    1             TO   WRK-GENSAN-ERR
                END-EVALUATE
                IF   WRK-GENSAN-ERR    = ZERO
                  IF   NACCT-DAY(IDX1) = 11
                    EVALUATE   WRK-NYUKHNKSNKBN(IDX1)
                     WHEN   099  THRU  100
                      MOVE   "190142190"    TO   WRK-GAIHAKU2(IDX1)
                     WHEN   OTHER
                      MOVE   "190141590"    TO   WRK-GAIHAKU2(IDX1)
                    END-EVALUATE
                  END-IF
                END-IF
      *
      *         ∆˛±°Œ¨æŒ§ŒºË∆¿° ∆√ƒÍª˛¥÷¬‡±°∏∫ªª°À
                IF    LNK-SROUNYUIN-RYAKUKBN    =  "1"
                  IF    WRK-NYUINKHN(IDX1)  NOT =  SPACE
                    MOVE     ZERO     TO    FLG-ARI
                    PERFORM  VARYING  ID1    FROM    1   BY  1
                          UNTIL  (ID1                          >  20  )
                            OR   (LNK-SROUNYUIN-RYAKUSYO2(ID1) = SPACE)
                            OR   (FLG-ARI                      =   1  )
                       IF  LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:6)
                                           =    "∏·¡∞∏∫"
                         MOVE    1    TO    FLG-ARI
                       END-IF
                    END-PERFORM
                    IF     FLG-ARI  =  ZERO
                    AND    ID1     <=   20
                         MOVE   "∏·¡∞∏∫"
                                TO    LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:6)
                    END-IF
                  END-IF
                END-IF
             WHEN     9
             WHEN    13
                EVALUATE   WRK-CHUKSNCD(IDX1)
                   WHEN   "9017"
                      IF   LNK-SROUNYUIN-SANTEIYM   >=  "201404"
                        IF  (WRK-NYUKHNKBN(IDX1) =   79 OR 88  )
                        OR  (WRK-NYUINKHN(IDX1)  =  "190799510")
                          MOVE  "190161770" TO   WRK-GAIHAKU(IDX1)
                        ELSE
                          MOVE  "190161670" TO   WRK-GAIHAKU(IDX1)
                        END-IF
                      ELSE
                          MOVE  "190161670" TO   WRK-GAIHAKU(IDX1)
                      END-IF
                   WHEN   "9018"
                      MOVE   "190161770"    TO   WRK-GAIHAKU(IDX1)
                   WHEN   "9026"
                      MOVE   "190161970"    TO   WRK-GAIHAKU(IDX1)
                   WHEN   "9030"
                      MOVE   "190162170"    TO   WRK-GAIHAKU(IDX1)
                   WHEN    OTHER
                      MOVE    1             TO   WRK-GENSAN-ERR2
                END-EVALUATE
                IF   WRK-GENSAN-ERR2   = ZERO
                  IF   NACCT-DAY(IDX1) = 13
                    EVALUATE   WRK-NYUKHNKSNKBN(IDX1)
                     WHEN   099  THRU  100
                      MOVE   "190142190"    TO   WRK-GAIHAKU2(IDX1)
                     WHEN   OTHER
                      MOVE   "190141590"    TO   WRK-GAIHAKU2(IDX1)
                    END-EVALUATE
                  END-IF
                END-IF
      *
      *         ∆˛±°Œ¨æŒ§ŒºË∆¿° ∆√ƒÍÕÀ∆¸∆˛¬‡±°∏∫ªª°À
                IF    LNK-SROUNYUIN-RYAKUKBN    =  "1"
                  IF    WRK-NYUINKHN(IDX1)  NOT =  SPACE
                    MOVE     ZERO     TO    FLG-ARI
                    PERFORM  VARYING  ID1    FROM    1   BY  1
                          UNTIL  (ID1                          >  20  )
                            OR   (LNK-SROUNYUIN-RYAKUSYO2(ID1) = SPACE)
                            OR   (FLG-ARI                      =   1  )
                       IF  LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:6)
                                           =    "≈⁄∆¸∏∫"
                         MOVE    1    TO    FLG-ARI
                       END-IF
                    END-PERFORM
                    IF     FLG-ARI  =  ZERO
                    AND    ID1     <=   20
                         MOVE   "≈⁄∆¸∏∫"
                                TO    LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:6)
                    END-IF
                  END-IF
                END-IF
           END-EVALUATE
      *
           .
       2101-GAIHAKU-GENSAN1-EXT.
           EXIT.
      *
      *****************************************************************
      *    ∏∫ªª•≥°º•…ºË∆¿£≤  201604
      *****************************************************************
       2101-GAIHAKU-GENSAN2-SEC           SECTION.
      *
           EVALUATE   NACCT-DAY(IDX1)
             WHEN     8
      ***    WHEN    11
                EVALUATE   WRK-CHUKSNCD(IDX1)
                   WHEN   "9017"
                      EVALUATE   WRK-NYUKHNKSNKBN(IDX1)
                        WHEN     9
                        WHEN    42   THRU   44
                        WHEN    47
                        WHEN   166   THRU  167
                        WHEN   197   THRU  198
                          MOVE  "190161570" TO   WRK-GAIHAKU(IDX1)
                        WHEN   OTHER
                          MOVE  "190161470" TO   WRK-GAIHAKU(IDX1)
                      END-EVALUATE
                   WHEN   "9026"
                      MOVE   "190161870"    TO   WRK-GAIHAKU(IDX1)
                   WHEN   "9030"
                      MOVE   "190162070"    TO   WRK-GAIHAKU(IDX1)
                   WHEN   "9090"
                      MOVE   "190198570"    TO   WRK-GAIHAKU(IDX1)
                   WHEN    OTHER
                      MOVE    1             TO   WRK-GENSAN-ERR
                END-EVALUATE
                IF   WRK-GENSAN-ERR    = ZERO
                  IF   NACCT-DAY(IDX1) = 11
                      MOVE   "190141590"    TO   WRK-GAIHAKU2(IDX1)
                  END-IF
                END-IF
      *
      *         ∆˛±°Œ¨æŒ§ŒºË∆¿° ∆√ƒÍª˛¥÷¬‡±°∏∫ªª°À
                IF    LNK-SROUNYUIN-RYAKUKBN    =  "1"
                  IF    WRK-NYUINKHN(IDX1)  NOT =  SPACE
                    MOVE     ZERO     TO    FLG-ARI
                    PERFORM  VARYING  ID1    FROM    1   BY  1
                          UNTIL  (ID1                          >  20  )
                            OR   (LNK-SROUNYUIN-RYAKUSYO2(ID1) = SPACE)
                            OR   (FLG-ARI                      =   1  )
                       IF  LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:6)
                                           =    "∏·¡∞∏∫"
                         MOVE    1    TO    FLG-ARI
                       END-IF
                    END-PERFORM
                    IF     FLG-ARI  =  ZERO
                    AND    ID1     <=   20
                         MOVE   "∏·¡∞∏∫"
                                TO    LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:6)
                    END-IF
                  END-IF
                END-IF
             WHEN     9
             WHEN    13
                EVALUATE   WRK-CHUKSNCD(IDX1)
                   WHEN   "9017"
                      EVALUATE   WRK-NYUKHNKSNKBN(IDX1)
                        WHEN     9
                        WHEN    42   THRU   44
                        WHEN    47
                        WHEN   166   THRU  167
                        WHEN   197   THRU  198
                          MOVE  "190161770" TO   WRK-GAIHAKU(IDX1)
                        WHEN   OTHER
                          MOVE  "190161670" TO   WRK-GAIHAKU(IDX1)
                      END-EVALUATE
                   WHEN   "9026"
                      MOVE   "190161970"    TO   WRK-GAIHAKU(IDX1)
                   WHEN   "9030"
                      MOVE   "190162170"    TO   WRK-GAIHAKU(IDX1)
                   WHEN   "9090"
                      MOVE   "190198670"    TO   WRK-GAIHAKU(IDX1)
                   WHEN    OTHER
                      MOVE    1             TO   WRK-GENSAN-ERR2
                END-EVALUATE
                IF   WRK-GENSAN-ERR2   = ZERO
                  IF   NACCT-DAY(IDX1) = 13
                      MOVE   "190141590"    TO   WRK-GAIHAKU2(IDX1)
                  END-IF
                END-IF
      *
      *         ∆˛±°Œ¨æŒ§ŒºË∆¿° ∆√ƒÍÕÀ∆¸∆˛¬‡±°∏∫ªª°À
                IF    LNK-SROUNYUIN-RYAKUKBN    =  "1"
                  IF    WRK-NYUINKHN(IDX1)  NOT =  SPACE
                    MOVE     ZERO     TO    FLG-ARI
                    PERFORM  VARYING  ID1    FROM    1   BY  1
                          UNTIL  (ID1                          >  20  )
                            OR   (LNK-SROUNYUIN-RYAKUSYO2(ID1) = SPACE)
                            OR   (FLG-ARI                      =   1  )
                       IF  LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:6)
                                           =    "≈⁄∆¸∏∫"
                         MOVE    1    TO    FLG-ARI
                       END-IF
                    END-PERFORM
                    IF     FLG-ARI  =  ZERO
                    AND    ID1     <=   20
                         MOVE   "≈⁄∆¸∏∫"
                                TO    LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:6)
                    END-IF
                  END-IF
                END-IF
           END-EVALUATE
      *
           .
       2101-GAIHAKU-GENSAN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ∏∫ªª•≥°º•…ºË∆¿£≥  201804
      *****************************************************************
       2101-GAIHAKU-GENSAN3-SEC           SECTION.
      *
           EVALUATE   NACCT-DAY(IDX1)
             WHEN     8
      ***    WHEN    11
                EVALUATE   WRK-CHUKSNCD(IDX1)
                   WHEN   "9017"
                      EVALUATE   WRK-NYUKHNKSNKBN(IDX1)
                        WHEN    27
                        WHEN    40
                          MOVE  "190161570" TO   WRK-GAIHAKU(IDX1)
                        WHEN   OTHER
                          MOVE  "190161470" TO   WRK-GAIHAKU(IDX1)
                      END-EVALUATE
                   WHEN   "9026"
                      MOVE   "190161870"    TO   WRK-GAIHAKU(IDX1)
                   WHEN   "9030"
                      MOVE   "190162070"    TO   WRK-GAIHAKU(IDX1)
                   WHEN   "9090"
                      MOVE   "190198570"    TO   WRK-GAIHAKU(IDX1)
                   WHEN    OTHER
                      MOVE    1             TO   WRK-GENSAN-ERR
                END-EVALUATE
                IF   WRK-GENSAN-ERR    = ZERO
                  IF   NACCT-DAY(IDX1) = 11
                      MOVE   "190141590"    TO   WRK-GAIHAKU2(IDX1)
                  END-IF
                END-IF
      *
      *         ∆˛±°Œ¨æŒ§ŒºË∆¿° ∆√ƒÍª˛¥÷¬‡±°∏∫ªª°À
                IF    LNK-SROUNYUIN-RYAKUKBN    =  "1"
                  IF    WRK-NYUINKHN(IDX1)  NOT =  SPACE
                    MOVE     ZERO     TO    FLG-ARI
                    PERFORM  VARYING  ID1    FROM    1   BY  1
                          UNTIL  (ID1                          >  20  )
                            OR   (LNK-SROUNYUIN-RYAKUSYO2(ID1) = SPACE)
                            OR   (FLG-ARI                      =   1  )
                       IF  LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:6)
                                           =    "∏·¡∞∏∫"
                         MOVE    1    TO    FLG-ARI
                       END-IF
                    END-PERFORM
                    IF     FLG-ARI  =  ZERO
                    AND    ID1     <=   20
                         MOVE   "∏·¡∞∏∫"
                                TO    LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:6)
                    END-IF
                  END-IF
                END-IF
             WHEN     9
             WHEN    13
             WHEN    17
                EVALUATE   WRK-CHUKSNCD(IDX1)
                   WHEN   "9017"
                      EVALUATE   WRK-NYUKHNKSNKBN(IDX1)
                        WHEN    27
                        WHEN    40
                          MOVE  "190161770" TO   WRK-GAIHAKU(IDX1)
                        WHEN   OTHER
                          MOVE  "190161670" TO   WRK-GAIHAKU(IDX1)
                      END-EVALUATE
                   WHEN   "9026"
                      MOVE   "190161970"    TO   WRK-GAIHAKU(IDX1)
                   WHEN   "9030"
                      MOVE   "190162170"    TO   WRK-GAIHAKU(IDX1)
                   WHEN   "9090"
                      MOVE   "190198670"    TO   WRK-GAIHAKU(IDX1)
                   WHEN    OTHER
                      MOVE    1             TO   WRK-GENSAN-ERR2
                END-EVALUATE
                IF   WRK-GENSAN-ERR2   = ZERO
                  IF   NACCT-DAY(IDX1) = 13
                      MOVE   "190141590"    TO   WRK-GAIHAKU2(IDX1)
                  END-IF
                  IF   NACCT-DAY(IDX1) = 17
                      MOVE   "190213790"    TO   WRK-GAIHAKU2(IDX1)
                  END-IF
                END-IF
      *
      *         ∆˛±°Œ¨æŒ§ŒºË∆¿° ∆√ƒÍÕÀ∆¸∆˛¬‡±°∏∫ªª°À
                IF    LNK-SROUNYUIN-RYAKUKBN    =  "1"
                  IF    WRK-NYUINKHN(IDX1)  NOT =  SPACE
                    MOVE     ZERO     TO    FLG-ARI
                    PERFORM  VARYING  ID1    FROM    1   BY  1
                          UNTIL  (ID1                          >  20  )
                            OR   (LNK-SROUNYUIN-RYAKUSYO2(ID1) = SPACE)
                            OR   (FLG-ARI                      =   1  )
                       IF  LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:6)
                                           =    "≈⁄∆¸∏∫"
                         MOVE    1    TO    FLG-ARI
                       END-IF
                    END-PERFORM
                    IF     FLG-ARI  =  ZERO
                    AND    ID1     <=   20
                         MOVE   "≈⁄∆¸∏∫"
                                TO    LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:6)
                    END-IF
                  END-IF
                END-IF
           END-EVALUATE
      *
           .
       2101-GAIHAKU-GENSAN3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ≈¿øÙ∑◊ªªΩËÕ˝
      *****************************************************************
       210-TENSU-KEISAN-SEC            SECTION.
      *
           INITIALIZE                      TNS-TENSU-REC
           IF    (LNK-SROUNYUIN-SRYCD-SK(IDX1 IDX2) =  "1"  )
           AND   (WRK-GAIHAKU(IDX1)           NOT   =  SPACE)
           AND   (LNK-SROUNYUIN-SANTEIYM   <=  "201203"     )
               MOVE    SPACE    TO   WRK-GAIHAKU-SRYCD
               PERFORM 2101-GAIHAKU-NYUINKHN-SET-SEC
               IF    WRK-GAIHAKU-SRYCD    NOT =   SPACE
                  MOVE    WRK-GAIHAKU-SRYCD   TO  TNS-SRYCD
               ELSE
                  MOVE    LNK-SROUNYUIN-SRYCD(IDX1 IDX2)
                                              TO  TNS-SRYCD
               END-IF
           ELSE
                  MOVE    LNK-SROUNYUIN-SRYCD(IDX1 IDX2)
                                              TO  TNS-SRYCD
           END-IF
           MOVE   LNK-SROUNYUIN-HOSPNUM       TO  TNS-HOSPNUM
           MOVE   LNK-SROUNYUIN-SANTEIYM      TO  TNS-YUKOSTYMD(1:6)
           MOVE   "01"                        TO  TNS-YUKOSTYMD(7:2)
           MOVE   LNK-SROUNYUIN-SANTEIYM      TO  TNS-YUKOEDYMD(1:6)
           MOVE   "01"                        TO  TNS-YUKOEDYMD(7:2)
           PERFORM 910-TENSU-READ-SEC
      *
      *    ≈¿øÙªªƒÍ
           EVALUATE    TNS-TENSIKIBETU
      *        ≈¿øÙ° •◊•È•π°À
               WHEN    3
                   ADD   TNS-TEN     TO  LNK-SROUNYUIN-ZAITEN(IDX1)
      *        °Û≤√ªª
               WHEN    5
                   COMPUTE    LNK-SROUNYUIN-ZAITEN(IDX1)  =
                              LNK-SROUNYUIN-ZAITEN(IDX1)  +
                            ((LNK-SROUNYUIN-ZAITEN(IDX1)  *
                             (TNS-TEN   /  100))  + 0.5)
      *        °Û∏∫ªª
               WHEN    6
                   COMPUTE    LNK-SROUNYUIN-ZAITEN(IDX1)  =
                            ( LNK-SROUNYUIN-ZAITEN(IDX1)  -
                             (LNK-SROUNYUIN-ZAITEN(IDX1)  *
                             (TNS-TEN   /  100))) + 0.5
      *        ≈¿øÙ° •ﬁ•§• •π°À
               WHEN    8
                   COMPUTE  LNK-SROUNYUIN-ZAITEN(IDX1)  =
                            LNK-SROUNYUIN-ZAITEN(IDX1)  -  TNS-TEN
      *        WHEN  OTHER
      *            ADD   TNS-TEN     TO  LNK-SROUNYUIN-ZAITEN(IDX1)
           END-EVALUATE
      *
           IF    (LNK-SROUNYUIN-SRYCD-SK(IDX1 IDX2) =  "2"        )
           AND   (LNK-SROUNYUIN-SRYCD(IDX1 IDX2)    =  "190111590")
                   MOVE  ZERO        TO  LNK-SROUNYUIN-ZAITEN(IDX1)
           END-IF
      *
      *    ø«Œ≈•≥°º•…∑◊
           IF    (LNK-SROUNYUIN-SRYCD-SK(IDX1 IDX2) =  "2"        )
           AND   (LNK-SROUNYUIN-SRYCD(IDX1 IDX2)    =  "190111590")
             MOVE "190111590"         TO  WRK-SRYCD-X
           ELSE
             MOVE  TNS-SRYCD          TO  WRK-SRYCD-X
           END-IF
           COMPUTE LNK-SROUNYUIN-SRYCDTOTAL(IDX1)  =
                   LNK-SROUNYUIN-SRYCDTOTAL(IDX1)  +   WRK-SRYCD-9
           ADD      1          TO  LNK-SROUNYUIN-MEISAISU(IDX1)
      *
           .
       210-TENSU-KEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ≈¿øÙ∑◊ªªΩËÕ˝
      *****************************************************************
       210X-TENSU-KEISAN-SEC            SECTION.
      *
           INITIALIZE                             TNS-TENSU-REC
           MOVE   LNK-SROUNYUIN-SRYCD(IDX1 IDX2)
                                              TO  TNS-SRYCD
           MOVE   LNK-SROUNYUIN-HOSPNUM       TO  TNS-HOSPNUM
           MOVE   LNK-SROUNYUIN-SANTEIYM      TO  TNS-YUKOSTYMD(1:6)
           MOVE   "01"                        TO  TNS-YUKOSTYMD(7:2)
           MOVE   LNK-SROUNYUIN-SANTEIYM      TO  TNS-YUKOEDYMD(1:6)
           MOVE   "01"                        TO  TNS-YUKOEDYMD(7:2)
           PERFORM 910-TENSU-READ-SEC
      *
      *    ≈¿øÙªªƒÍ
           EVALUATE    TNS-TENSIKIBETU
      *        ≈¿øÙ° •◊•È•π°À
               WHEN    3
                   COMPUTE    WRK-ZAITEN-HYOKETU2   =
                              WRK-ZAITEN-HYOKETU2   +
                              TNS-TEN
      *        °Û≤√ªª
               WHEN    5
                   COMPUTE    WRK-ZAITEN-HYOKETU2   =
                              WRK-ZAITEN-HYOKETU2   +
                             (WRK-ZAITEN-HYOKETU2   *
                             (TNS-TEN   /  100))
      *        °Û∏∫ªª
               WHEN    6
                 IF  (LNK-SROUNYUIN-SRYCD-SK(IDX1 IDX2) = "1"        )
                 AND (LNK-SROUNYUIN-SRYCD(IDX1 IDX2)    = "190122890")
                   COMPUTE    WRK-KEISAN            =
                            ( WRK-ZAITEN-HYOKETU2   -
                             (WRK-ZAITEN-HYOKETU2   *
                             (TNS-TEN   /  100)))   +  0.5
                   MOVE       WRK-KEISAN   TO   WRK-ZAITEN-HYOKETU2
                 ELSE
                   COMPUTE    WRK-ZAITEN-HYOKETU2   =
                              WRK-ZAITEN-HYOKETU2   -
                             (WRK-ZAITEN-HYOKETU2   *
                             (TNS-TEN   /  100))
                 END-IF
      *        ≈¿øÙ° •ﬁ•§• •π°À
               WHEN    8
                   COMPUTE    WRK-ZAITEN-HYOKETU2   =
                              WRK-ZAITEN-HYOKETU2   -
                              TNS-TEN
      *        WHEN  OTHER
      *            COMPUTE    WRK-ZAITEN-HYOKETU2   =
      *                       WRK-ZAITEN-HYOKETU2   +
      *                       TNS-TEN
           END-EVALUATE
      *
      *    ø«Œ≈•≥°º•…∑◊
           MOVE  TNS-SRYCD     TO  WRK-SRYCD-X
           COMPUTE LNK-SROUNYUIN-SRYCDTOTAL(IDX1)  =
                   LNK-SROUNYUIN-SRYCDTOTAL(IDX1)  +   WRK-SRYCD-9
           ADD      1          TO  LNK-SROUNYUIN-MEISAISU(IDX1)
      *
           .
       210X-TENSU-KEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ≈¿øÙ∑◊ªªΩËÕ˝° …∏∑Á°À
      *****************************************************************
       210-TENSU-KEISAN-HYOKETU-SEC            SECTION.
      *
           IF   LNK-SROUNYUIN-SRYCD-SK(IDX1 IDX2)       =   "5"
              MOVE  LNK-SROUNYUIN-ZAITEN(IDX1)  TO  WRK-ZAITEN-HYOKETU
              MOVE       ZERO      TO     LNK-SROUNYUIN-ZAITEN(IDX1)
           END-IF
      *
           IF   LNK-SROUNYUIN-SRYCD-SK(IDX1 IDX2)   NOT =   "6"
              PERFORM    210-TENSU-KEISAN-SEC
           END-IF
      *
           IF    ((LNK-SROUNYUIN-SRYCD-SK(IDX1 IDX2) =  "1"  ) AND
                  (WRK-KANGOGEN(IDX1)                =  SPACE) AND
                  (WRK-GAIHAKU(IDX1)                 =  SPACE))
           OR    ((LNK-SROUNYUIN-SRYCD-SK(IDX1 IDX2) =  "C"  ) AND
                  (WRK-GAIHAKU(IDX1)                 =  SPACE))
           OR    ( LNK-SROUNYUIN-SRYCD-SK(IDX1 IDX2) =  "2"   )
      *      …∏∑Á°ß°Û∏∫ªª
             COMPUTE    LNK-SROUNYUIN-ZAITEN(IDX1)  =
                      ( LNK-SROUNYUIN-ZAITEN(IDX1)  -
                       (LNK-SROUNYUIN-ZAITEN(IDX1)  *
                       (WRK-HYOKETU-TEN   /  100))) +  0.5
      *      ø«Œ≈•≥°º•…∑◊
             MOVE    WRK-HYOKETU(IDX1)   TO  WRK-SRYCD-X
             COMPUTE LNK-SROUNYUIN-SRYCDTOTAL(IDX1)  =
                     LNK-SROUNYUIN-SRYCDTOTAL(IDX1)  +   WRK-SRYCD-9
             ADD      1          TO  LNK-SROUNYUIN-MEISAISU(IDX1)
           END-IF
      *
           IF      LNK-SROUNYUIN-SRYCD-SK(IDX1 IDX2)  =   "5"
             IF    LNK-SROUNYUIN-ZAITEN(IDX1)   >=   ZERO
      *        …∏∑Á°ß°Û∏∫ªª
               COMPUTE    LNK-SROUNYUIN-ZAITEN(IDX1)  =
                        ( LNK-SROUNYUIN-ZAITEN(IDX1)  -
                         (LNK-SROUNYUIN-ZAITEN(IDX1)  *
                         (WRK-HYOKETU-TEN   /  100))) +  0.5
             ELSE
               COMPUTE    LNK-SROUNYUIN-ZAITEN(IDX1)  =
                          LNK-SROUNYUIN-ZAITEN(IDX1)  *
                          -1
               COMPUTE    LNK-SROUNYUIN-ZAITEN(IDX1)  =
                        ( LNK-SROUNYUIN-ZAITEN(IDX1)  -
                         (LNK-SROUNYUIN-ZAITEN(IDX1)  *
                         (WRK-HYOKETU-TEN   /  100))) +  0.5
               COMPUTE    LNK-SROUNYUIN-ZAITEN(IDX1)  =
                          LNK-SROUNYUIN-ZAITEN(IDX1)  *
                          -1
             END-IF
      *
             COMPUTE LNK-SROUNYUIN-ZAITEN(IDX1)  =
                     LNK-SROUNYUIN-ZAITEN(IDX1)  +  WRK-ZAITEN-HYOKETU
           END-IF
      *
           .
       210-TENSU-KEISAN-HYOKETU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ≈¿øÙ∑◊ªªΩËÕ˝° …∏∑Á°À
      *****************************************************************
       210X-TENSU-KEISAN-HYOKETU-SEC            SECTION.
      *
           IF   LNK-SROUNYUIN-SRYCD-SK(IDX1 IDX2)   NOT =   "6"
              PERFORM    210X-TENSU-KEISAN-SEC
           END-IF
      *
           IF    ((LNK-SROUNYUIN-SRYCD-SK(IDX1 IDX2) =  "1"  ) AND
                  (WRK-KANGOGEN(IDX1)                =  SPACE) AND
                  (WRK-GAIHAKU(IDX1)                 =  SPACE) AND
                  (WRK-SHOKIKSN(IDX1)                =  SPACE))
           OR    ((LNK-SROUNYUIN-SRYCD-SK(IDX1 IDX2) =  "C"  ) AND
                  (WRK-GAIHAKU(IDX1)                 =  SPACE) AND
                  (WRK-SHOKIKSN(IDX1)                =  SPACE))
           OR    ((LNK-SROUNYUIN-SRYCD-SK(IDX1 IDX2) =  "2"  ) AND
                  (WRK-SHOKIKSN(IDX1)                =  SPACE))
           OR    ( LNK-SROUNYUIN-SRYCD-SK(IDX1 IDX2) =  "D"   )
      *      …∏∑Á°ß°Û∏∫ªª
             COMPUTE    WRK-ZAITEN-HYOKETU2   =
                        WRK-ZAITEN-HYOKETU2   -
                       (WRK-ZAITEN-HYOKETU2   *
                       (WRK-HYOKETU-TEN   /  100))
      *      ø«Œ≈•≥°º•…∑◊
             MOVE    WRK-HYOKETU(IDX1)   TO  WRK-SRYCD-X
             COMPUTE LNK-SROUNYUIN-SRYCDTOTAL(IDX1)  =
                     LNK-SROUNYUIN-SRYCDTOTAL(IDX1)  +   WRK-SRYCD-9
             ADD      1          TO  LNK-SROUNYUIN-MEISAISU(IDX1)
           END-IF
      *
           .
       210X-TENSU-KEISAN-HYOKETU-EXT.
           EXIT.
      *
      **************************************************************
      *    ≥∞«Òª˛§Œ∆˛±°¥À‹Œ¡ºË∆¿
      **************************************************************
       2101-GAIHAKU-NYUINKHN-SET-SEC           SECTION.
      *
           EVALUATE  LNK-SROUNYUIN-SRYCD(IDX1 IDX2)
               WHEN  "190078210"
                     MOVE  "190077410"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078310"
                     MOVE  "190077510"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078410"
                     MOVE  "190077610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078510"
                     MOVE  "190077710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078610"
                     MOVE  "190077810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078710"
                     MOVE  "190077910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078810"
                     MOVE  "190078010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078910"
                     MOVE  "190078110"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190082410"
                     MOVE  "190081710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082510"
                     MOVE  "190081810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082610"
                     MOVE  "190081910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082710"
                     MOVE  "190082010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082810"
                     MOVE  "190082110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082910"
                     MOVE  "190082210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190083010"
                     MOVE  "190082310"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190084310"
                     MOVE  "190083610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084410"
                     MOVE  "190083710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084510"
                     MOVE  "190083810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084610"
                     MOVE  "190083910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084710"
                     MOVE  "190084010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084810"
                     MOVE  "190084110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084910"
                     MOVE  "190084210"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190087410"
                     MOVE  "190085910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087510"
                     MOVE  "190086010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087610"
                     MOVE  "190086110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087710"
                     MOVE  "190086210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087810"
                     MOVE  "190086310"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087910"
                     MOVE  "190086410"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088010"
                     MOVE  "190086510"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088110"
                     MOVE  "190086610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088210"
                     MOVE  "190086710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088310"
                     MOVE  "190086810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088410"
                     MOVE  "190086910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088510"
                     MOVE  "190087010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088610"
                     MOVE  "190087110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088710"
                     MOVE  "190087210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088810"
                     MOVE  "190087310"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190094710"
                     MOVE  "190093710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190094810"
                     MOVE  "190093810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190094910"
                     MOVE  "190093910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190095010"
                     MOVE  "190094010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190095110"
                     MOVE  "190094110"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190797410"
                     MOVE  "190796610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797510"
                     MOVE  "190796710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797610"
                     MOVE  "190796810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797710"
                     MOVE  "190796910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797810"
                     MOVE  "190797010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797910"
                     MOVE  "190797110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190798010"
                     MOVE  "190797210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190798110"
                     MOVE  "190797310"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190803510"
                     MOVE  "190802810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803610"
                     MOVE  "190802910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803710"
                     MOVE  "190803010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803810"
                     MOVE  "190803110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803910"
                     MOVE  "190803210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190804010"
                     MOVE  "190803310"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190804110"
                     MOVE  "190803410"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190091210"
                     MOVE  "190090210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190091310"
                     MOVE  "190090310"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190109670"
                     MOVE  "190097010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190808870"
                     MOVE  "190805710"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190117070"
                     MOVE  "190097010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190813270"
                     MOVE  "190805710"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190117670"
                     MOVE  "190097010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190813870"
                     MOVE  "190805710"      TO   WRK-GAIHAKU-SRYCD
      *        ø«Œ≈ΩÍœ∑øÕ∞ÂŒ≈¥…Õ˝Œ¡
               WHEN  "190774210"
                     MOVE  "190774110"      TO   WRK-GAIHAKU-SRYCD
      *        œ∑øÕ¿≠«ß√Œæ…º¿¥µº£Œ≈…¬≈Ô∆˛±°Œ¡£±
               WHEN  "190740010"
                     MOVE  "190739910"      TO   WRK-GAIHAKU-SRYCD
      *        œ∑øÕ¿≠«ß√Œæ…º¿¥µº£Œ≈…¬≈Ô∆˛±°Œ¡£≤
               WHEN  "190813510"
                     MOVE  "190813410"      TO   WRK-GAIHAKU-SRYCD
           END-EVALUATE
      *
           IF   (LNK-SROUNYUIN-SANTEIYM  >= "200604"   )
           AND  (WRK-GAIHAKU-SRYCD        = "190774110" OR
                                            "190739910" OR
                                            "190813410")
               MOVE    SPACE    TO    WRK-GAIHAKU-SRYCD
           END-IF
      *
           .
       2101-GAIHAKU-NYUINKHN-SET-EXT.
           EXIT.
      *
      **************************************************************
      *    Œ¨æŒ§Œ ‘Ω∏
      **************************************************************
       210-RYAKUSYO-HENSYU-SEC           SECTION.
      *
           IF   (LNK-SROUNYUIN-SANTEIYM   >=   "201404")
           AND  (LNK-SROUNYUIN-SANTEIYM   <=   "201406")
              CONTINUE
           ELSE
              MOVE   ZERO   TO  WRK-TOKUREI-FLG
           END-IF
      *
           IF   LNK-SROUNYUIN-RYAKUKBN   =   "1"
      *
              MOVE    SPACE         TO   NYURYAKUSHO-REC
              INITIALIZE                 NYURYAKUSHO-REC
      *
              MOVE WRK-RYAKU-SRYCD        TO NYURYAKUSHO-SRYCD
      ***     MOVE LNK-SROUNYUIN-HOSPNUM  TO NYURYAKUSHO-HOSPNUM
              MOVE LNK-SROUNYUIN-SANTEIYM TO NYURYAKUSHO-YUKOSTYMD(1:6)
              MOVE "01"                   TO NYURYAKUSHO-YUKOSTYMD(7:2)
              MOVE LNK-SROUNYUIN-SANTEIYM TO NYURYAKUSHO-YUKOEDYMD(1:6)
              MOVE "01"                   TO NYURYAKUSHO-YUKOEDYMD(7:2)
              PERFORM  910-NYURYAKUSHO-READ-SEC
      *
              IF       FLG-NYURYAKUSHO    =    ZERO
      *
                EVALUATE   WRK-ZAISKBKBN
      *
                 WHEN     1
      *
                  IF   WRK-SRYKBN  =  "90"
      *
                    MOVE     ZERO     TO    FLG-ARI
                    PERFORM  VARYING  ID1    FROM    1   BY  1
                          UNTIL   ID1                          > 20
                            OR    LNK-SROUNYUIN-RYAKUSYO2(ID1) = SPACE
                            OR    FLG-ARI                      = 1 
                       IF  LNK-SROUNYUIN-RYAKUSYO2(ID1)
                                           =  NYURYAKUSHO-RYAKUSHO
                           MOVE  1   TO   FLG-ARI
                       END-IF
                    END-PERFORM  
                    IF  FLG-ARI  =  ZERO  
                    AND ID1     <=   20
                           MOVE   NYURYAKUSHO-RYAKUSHO
                                     TO   LNK-SROUNYUIN-RYAKUSYO2(ID1)
                    END-IF
      *
                    IF  LNK-SROUNYUIN-SANTEIYM  >= "200604"
                     CONTINUE
                    ELSE
                     IF  WRK-RYAKU-SRYCD  =  "190109670" OR "190808870"
      *
                       MOVE     ZERO     TO    FLG-ARI
                       PERFORM  VARYING  ID1    FROM    1   BY  1
                           UNTIL   ID1                          > 20
                             OR    LNK-SROUNYUIN-RYAKUSYO2(ID1) = SPACE
                             OR    FLG-ARI                      = 1 
                         IF LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:4) = "Õ≠¥«"
                              MOVE  1   TO   FLG-ARI
                         END-IF
                       END-PERFORM  
                       IF  FLG-ARI  =  ZERO  
                       AND ID1     <=   20
                            MOVE  "Õ≠¥«"
                                TO   LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:4)
                       END-IF
      *
                     END-IF
                     IF  WRK-RYAKU-SRYCD  =  "190117070" OR "190813270"
      *
                       MOVE     ZERO     TO    FLG-ARI
                       PERFORM  VARYING  ID1    FROM    1   BY  1
                           UNTIL   ID1                          > 20
                             OR    LNK-SROUNYUIN-RYAKUSYO2(ID1) = SPACE
                             OR    FLG-ARI                      = 1 
                         IF LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:4) = "Õ≠∞Â"
                              MOVE  1   TO   FLG-ARI
                         END-IF
                       END-PERFORM  
                       IF  FLG-ARI  =  ZERO  
                       AND ID1     <=   20
                            MOVE  "Õ≠∞Â"
                                TO   LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:4)
                       END-IF
      *
                     END-IF
                     IF  WRK-RYAKU-SRYCD  =  "190117670" OR "190813870"
      *
                       MOVE     ZERO     TO    FLG-ARI
                       PERFORM  VARYING  ID1    FROM    1   BY  1
                           UNTIL   ID1                          > 20
                             OR    LNK-SROUNYUIN-RYAKUSYO2(ID1) = SPACE
                             OR    FLG-ARI                      = 1 
                         IF LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:6) = "Õ≠¥«∞Â"
                              MOVE  1   TO   FLG-ARI
                         END-IF
                       END-PERFORM  
                       IF  FLG-ARI  =  ZERO  
                       AND ID1     <=   20
                            MOVE  "Õ≠¥«∞Â"
                                TO   LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:6)
                       END-IF
      *
                     END-IF
                    END-IF
      *
                  ELSE
      *
                    MOVE    NYURYAKUSHO-RYAKUSHO   TO  WRK-RYAKUSHO
      *
                  END-IF
      *
                 WHEN   6
      *
                    MOVE     ZERO     TO    FLG-ARI
                    PERFORM  VARYING  ID1    FROM    1   BY  1
                          UNTIL   ID1                          > 20
                            OR    LNK-SROUNYUIN-RYAKUSYO2(ID1) = SPACE
                            OR    FLG-ARI                      = 1 
                       IF  LNK-SROUNYUIN-RYAKUSYO2(ID1)
                                           =  NYURYAKUSHO-RYAKUSHO
                           MOVE  1   TO   FLG-ARI
                       END-IF
                    END-PERFORM  
                    IF  FLG-ARI  =  ZERO  
                    AND ID1     <=   20
                           MOVE   NYURYAKUSHO-RYAKUSHO
                                     TO   LNK-SROUNYUIN-RYAKUSYO2(ID1)
                    END-IF
      *
                END-EVALUATE
              ELSE
                IF   ( LNK-SROUNYUIN-SANTEIYM  >= "200604"    )
                AND  ( WRK-RYAKU-SRYCD          = "190123250" OR
                                                  "190123350" OR
                                                  "190123450" OR
                                                  "190123550" )
      *
                    MOVE     ZERO     TO    FLG-ARI
                    PERFORM  VARYING  ID1    FROM    1   BY  1
                          UNTIL   ID1                          > 20
                            OR    LNK-SROUNYUIN-RYAKUSYO2(ID1) = SPACE
                            OR    FLG-ARI                      = 1
                      EVALUATE   WRK-RYAKU-SRYCD
                        WHEN     "190123250"
                         IF  LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:8)
                                                         = "∞Ï»Ã∑–≤·"
                             MOVE  1   TO   FLG-ARI
                         END-IF
                        WHEN     "190123350"
                         IF  LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:8)
                                                         = "∑Î≥À∑–≤·"
                             MOVE  1   TO   FLG-ARI
                         END-IF
                        WHEN     "190123450"
                         IF  LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:8)
                                                         = "¿∫ø¿∑–≤·"
                             MOVE  1   TO   FLG-ARI
                         END-IF
                        WHEN     "190123550"
                         IF  LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:8)
                                                         = "æ„≥≤∑–≤·"
                             MOVE  1   TO   FLG-ARI
                         END-IF
                      END-EVALUATE
                    END-PERFORM
                    IF  FLG-ARI  =  ZERO
                    AND ID1     <=   20
                      EVALUATE   WRK-RYAKU-SRYCD
                        WHEN     "190123250"
                           MOVE   "∞Ï»Ã∑–≤·"
                                 TO   LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:8)
                        WHEN     "190123350"
                           MOVE   "∑Î≥À∑–≤·"
                                 TO   LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:8)
                        WHEN     "190123450"
                           MOVE   "¿∫ø¿∑–≤·"
                                 TO   LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:8)
                        WHEN     "190123550"
                           MOVE   "æ„≥≤∑–≤·"
                                 TO   LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:8)
                      END-EVALUATE
                    END-IF
      *
                END-IF
                IF   ( LNK-SROUNYUIN-SANTEIYM  >= "200807"    )
                AND  ( WRK-RYAKU-SRYCD          = "190130810" OR
                                                  "190131010" OR
                                                  "190131110" )
      *
                    MOVE     ZERO     TO    FLG-ARI
                    PERFORM  VARYING  ID1    FROM    1   BY  1
                          UNTIL   ID1                          > 20
                            OR    LNK-SROUNYUIN-RYAKUSYO2(ID1) = SPACE
                            OR    FLG-ARI                      = 1
                      EVALUATE   WRK-RYAKU-SRYCD
                        WHEN     "190130810"
                         IF  LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:8)
                                                         = "∞Ï»Ã£±£∞"
                             MOVE  1   TO   FLG-ARI
                         END-IF
                        WHEN     "190131010"
                         IF  LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:6)
                                                         = "∑Î£±£∞"
                             MOVE  1   TO   FLG-ARI
                         END-IF
                        WHEN     "190131110"
                         IF  LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:6)
                                                         = "¿Ï£±£∞"
                             MOVE  1   TO   FLG-ARI
                         END-IF
                      END-EVALUATE
                    END-PERFORM
                    IF  FLG-ARI  =  ZERO
                    AND ID1     <=   20
                      EVALUATE   WRK-RYAKU-SRYCD
                        WHEN     "190130810"
                           MOVE   "∞Ï»Ã£±£∞"
                                 TO   LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:8)
                        WHEN     "190131010"
                           MOVE   "∑Î£±£∞"
                                 TO   LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:6)
                        WHEN     "190131110"
                           MOVE   "¿Ï£±£∞"
                                 TO   LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:6)
                      END-EVALUATE
                    END-IF
      *
                END-IF
              END-IF
      *
           END-IF
      *
           .
       210-RYAKUSYO-HENSYU-EXT.
           EXIT.
      *
      **************************************************************
      *    Œ¨æŒ§Œ ‘Ω∏° ÃÎ¥÷¥«∏Ó¬Œ¿©∆√ƒÍ∆¸∏∫ªª°À
      **************************************************************
       210-RYAKUSYO-YAGEN-HENSYU-SEC        SECTION.
      *
           IF   LNK-SROUNYUIN-RYAKUKBN   =   "1"
      *
              MOVE     ZERO     TO    FLG-ARI
              PERFORM  VARYING  ID1    FROM    1   BY  1
                        UNTIL   ID1                          > 20
                         OR     LNK-SROUNYUIN-RYAKUSYO2(ID1) = SPACE
                         OR     FLG-ARI                      = 1
                EVALUATE   WRK-RYAKU-SRYCD
      *           ÃÎ¥÷¥«∏Ó¬Œ¿©∆√ƒÍ∆¸∏∫ªª° ∞Ï»Ã…¬≈Ô∆˛±°¥À‹Œ¡°À
                  WHEN     "190200970"
                    IF  LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:14)
                                                = "∞Ï»ÃÃÎ¥«∆√ƒÍ∏∫"
                             MOVE  1   TO   FLG-ARI
                    END-IF
      *           Œ≈Õ‹…¬≈Ô°¶ÃÎ¥÷¥«∏Ó¬Œ¿©∆√ƒÍ∆¸∏∫ªª° ∞Ï»Ã…¬≈Ô∆˛±°¥À‹Œ¡°À
                  WHEN     "190214470"
                    IF  LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:14)
                                                = "∞Ï»ÃÃÎ¥«∆√ƒÍ∏∫"
                             MOVE  1   TO   FLG-ARI
                    END-IF
      *           ÃÎ¥÷¥«∏Ó¬Œ¿©∆√ƒÍ∆¸∏∫ªª° ∑Î≥À…¬≈Ô∆˛±°¥À‹Œ¡°À
                  WHEN     "190205270"
                    IF  LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:12)
                                                = "∑ÎÃÎ¥«∆√ƒÍ∏∫"
                             MOVE  1   TO   FLG-ARI
                    END-IF
      *           ÃÎ¥÷¥«∏Ó¬Œ¿©∆√ƒÍ∆¸∏∫ªª° ¿∫ø¿…¬≈Ô∆˛±°¥À‹Œ¡°À
                  WHEN     "190205370"
                    IF  LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:12)
                                                = "¿∫ÃÎ¥«∆√ƒÍ∏∫"
                             MOVE  1   TO   FLG-ARI
                    END-IF
      *           ÃÎ¥÷¥«∏Ó¬Œ¿©∆√ƒÍ∆¸∏∫ªª° ¿ÏÃÁ…¬±°∆˛±°¥À‹Œ¡°À
                  WHEN     "190205470"
                    IF  LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:12)
                                                = "¿ÏÃÎ¥«∆√ƒÍ∏∫"
                             MOVE  1   TO   FLG-ARI
                    END-IF
      *           ÃÎ¥÷¥«∏Ó¬Œ¿©∆√ƒÍ∆¸∏∫ªª° æ„≥≤º‘ª‹¿ﬂ≈˘∆˛±°¥À‹Œ¡°À
                  WHEN     "190205870"
                    IF  LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:12)
                                                = "æ„ÃÎ¥«∆√ƒÍ∏∫"
                             MOVE  1   TO   FLG-ARI
                    END-IF
      *           ÃÎ¥÷¥«∏Ó¬Œ¿©∆√ƒÍ∆¸∏∫ªª° √œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°Œ¡°À
                  WHEN     "190212670"
                    IF  LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:12)
                                                = " ÒÃÎ¥«∆√ƒÍ∏∫"
                             MOVE  1   TO   FLG-ARI
                    END-IF
                END-EVALUATE
              END-PERFORM
              IF  FLG-ARI  =  ZERO
              AND ID1     <=   20
                EVALUATE   WRK-RYAKU-SRYCD
      *           ÃÎ¥÷¥«∏Ó¬Œ¿©∆√ƒÍ∆¸∏∫ªª° ∞Ï»Ã…¬≈Ô∆˛±°¥À‹Œ¡°À
                  WHEN     "190200970"
                    MOVE   "∞Ï»ÃÃÎ¥«∆√ƒÍ∏∫"
                               TO   LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:14)
      *           Œ≈Õ‹…¬≈Ô°¶ÃÎ¥÷¥«∏Ó¬Œ¿©∆√ƒÍ∆¸∏∫ªª° ∞Ï»Ã…¬≈Ô∆˛±°¥À‹Œ¡°À
                  WHEN     "190214470"
                    MOVE   "∞Ï»ÃÃÎ¥«∆√ƒÍ∏∫"
                               TO   LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:14)
      *           ÃÎ¥÷¥«∏Ó¬Œ¿©∆√ƒÍ∆¸∏∫ªª° ∑Î≥À…¬≈Ô∆˛±°¥À‹Œ¡°À
                  WHEN     "190205270"
                    MOVE   "∑ÎÃÎ¥«∆√ƒÍ∏∫"
                               TO   LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:12)
      *           ÃÎ¥÷¥«∏Ó¬Œ¿©∆√ƒÍ∆¸∏∫ªª° ¿∫ø¿…¬≈Ô∆˛±°¥À‹Œ¡°À
                  WHEN     "190205370"
                    MOVE   "¿∫ÃÎ¥«∆√ƒÍ∏∫"
                               TO   LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:12)
      *           ÃÎ¥÷¥«∏Ó¬Œ¿©∆√ƒÍ∆¸∏∫ªª° ¿ÏÃÁ…¬±°∆˛±°¥À‹Œ¡°À
                  WHEN     "190205470"
                    MOVE   "¿ÏÃÎ¥«∆√ƒÍ∏∫"
                               TO   LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:12)
      *           ÃÎ¥÷¥«∏Ó¬Œ¿©∆√ƒÍ∆¸∏∫ªª° æ„≥≤º‘ª‹¿ﬂ≈˘∆˛±°¥À‹Œ¡°À
                  WHEN     "190205870"
                    MOVE   "æ„ÃÎ¥«∆√ƒÍ∏∫"
                               TO   LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:12)
      *           ÃÎ¥÷¥«∏Ó¬Œ¿©∆√ƒÍ∆¸∏∫ªª° √œ∞Ë Ò≥Á•±•¢…¬≈Ô∆˛±°Œ¡°À
                  WHEN     "190212670"
                    MOVE   " ÒÃÎ¥«∆√ƒÍ∏∫"
                               TO   LNK-SROUNYUIN-RYAKUSYO2(ID1)(1:12)
                END-EVALUATE
              END-IF
      *
           END-IF
      *
           .
       210-RYAKUSYO-YAGEN-HENSYU-EXT.
           EXIT.
      *
      **************************************************************
      *    ø«Œ≈π‘∞Ÿ§Œ∆…§ﬂπ˛§ﬂΩËÕ˝
      **************************************************************
       210-SRYACT-HENSYU-SEC           SECTION.
      *
           IF   LNK-SROUNYUIN-RYAKUKBN   =   "1"
      *
              INITIALIZE                           SRYACT-REC
              MOVE    ACCT-HOSPNUM         TO      SRY-HOSPNUM
              MOVE    ACCT-NYUGAIKBN       TO      SRY-NYUGAIKBN
              MOVE    ACCT-PTID            TO      SRY-PTID
              MOVE    ACCT-SRYYM           TO      SRY-SRYYM
              MOVE    ACCT-ZAINUM          TO      SRY-ZAINUM
              MOVE    SRYACT-REC           TO      MCPDATA-REC
              MOVE    "DBSELECT"           TO      MCP-FUNC
              MOVE    "tbl_sryact"         TO      MCP-TABLE
              MOVE    "key9"               TO      MCP-PATHNAME
              CALL    "ORCDBMAIN"          USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
              IF      MCP-RC               =       ZERO
                  MOVE    "tbl_sryact"     TO      MCP-TABLE
                  MOVE    "key9"           TO      MCP-PATHNAME
                  PERFORM  910-SRYACT-READ-SEC
                  PERFORM  UNTIL  FLG-SRYACT    =    1
                      PERFORM   VARYING   SRYACT-IDX  FROM  1   BY   1
                                UNTIL    (SRYACT-IDX    >   5)  OR
                                         (SRY-SRYCD(SRYACT-IDX)
                                                            =   SPACE)
      *
                          MOVE    SRY-SRYCD(SRYACT-IDX)
                                                  TO  WRK-RYAKU-SRYCD
                          MOVE    "6"             TO  WRK-ZAISKBKBN
      *
                          PERFORM   210-RYAKUSYO-HENSYU-SEC
      *
                      END-PERFORM
                      MOVE     "tbl_sryact"       TO  MCP-TABLE
                      MOVE     "key9"             TO  MCP-PATHNAME
                      PERFORM   910-SRYACT-READ-SEC
                  END-PERFORM
              END-IF
      *
              MOVE     "tbl_sryact"               TO  MCP-TABLE
              MOVE     "key9"                     TO  MCP-PATHNAME
              PERFORM   910-DB-CLOSE-SEC
      *
           END-IF
      *
           .
       210-SRYACT-HENSYU-EXT.
           EXIT.
      *
      ******************************************************************
      *    ∑Ó∆‚§À £øÙ§Œ∞ÂŒ≈¥¸¥÷æ Û§¨§ §§§´∏°∫˜° ∞ÂŒ≈µ°¥ÿ∏∫ªª§Œ≥Œ«ß°À
      ******************************************************************
       210-SYS-5000-CHK-SEC                 SECTION.
      *
           MOVE    ZERO                TO  WRK-SYS5000-COUNT
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    LNK-SROUNYUIN-HOSPNUM
                                       TO  SYS-HOSPNUM
           MOVE    LNK-SROUNYUIN-SANTEIYM
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "__"                TO  SYS-STYUKYMD(7:2)
           MOVE    LNK-SROUNYUIN-SANTEIYM
                                       TO  SYS-EDYUKYMD(1:6)
           MOVE    "__"                TO  SYS-EDYUKYMD(7:2)
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key11"         TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               PERFORM  UNTIL    FLG-SYSKANRI   =   1
                   ADD     1     TO        WRK-SYS5000-COUNT
                   MOVE    "tbl_syskanri"  TO  MCP-TABLE
                   MOVE    "key11"         TO  MCP-PATHNAME
                   PERFORM  910-SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE     "tbl_syskanri"     TO  MCP-TABLE
           MOVE     "key11"            TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
      *
       210-SYS-5000-CHK-EXT.
           EXIT.
      *
      ******************************************************************
      *    ∑Ó∆‚§Œ∞ÂŒ≈¥¸¥÷æ ÛºË∆¿° ∞ÂŒ≈µ°¥ÿ∏∫ªªæ Û°À
      ******************************************************************
       210-SYS-5000-GET-SEC                 SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    LNK-SROUNYUIN-HOSPNUM
                                       TO  SYS-HOSPNUM
           MOVE    LNK-SROUNYUIN-SANTEIYM
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "__"                TO  SYS-STYUKYMD(7:2)
           MOVE    LNK-SROUNYUIN-SANTEIYM
                                       TO  SYS-EDYUKYMD(1:6)
           MOVE    "__"                TO  SYS-EDYUKYMD(7:2)
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key11"         TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               PERFORM  UNTIL    FLG-SYSKANRI   =   1
                 MOVE   SYS-5000-STYUKYMD(7:2)  TO  IDV
                 PERFORM  VARYING   GENSAN-IDX  FROM  1  BY  1
                            UNTIL     GENSAN-IDX   >   4
                    IF    SYS-5000-GENKBN(GENSAN-IDX)   =   "2"
                        EVALUATE  GENSAN-IDX
                            WHEN  2
                              MOVE  "190080570"  TO  WRK-GENSAN2
      **                        IF   FLG-RJN    =    ZERO
      **                            MOVE  "190080570"  TO  WRK-GENSAN2
      **                        ELSE
      **                            MOVE  "190799770"  TO  WRK-GENSAN2
      **                        END-IF
                              PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                                  UNTIL     GENSAN-IDX2   >   31
                                MOVE   "1"  TO
                                        WRK-GENSAN-INNAI(GENSAN-IDX2)
                              END-PERFORM
                            WHEN  3
                              MOVE  "190111690"  TO  WRK-GENSAN3
      **                        IF   FLG-RJN    =    ZERO
      **                            MOVE  "190111690"  TO  WRK-GENSAN3
      **                        ELSE
      **                            MOVE  "190809090"  TO  WRK-GENSAN3
      **                        END-IF
                              PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                                  UNTIL     GENSAN-IDX2   >   31
                                MOVE   "1"  TO
                                        WRK-GENSAN-IRYO(GENSAN-IDX2)
                              END-PERFORM
                            WHEN  4
                              MOVE  "190111790"  TO  WRK-GENSAN4
      **                        IF   FLG-RJN    =    ZERO
      **                            MOVE  "190111790"  TO  WRK-GENSAN4
      **                        ELSE
      **                            MOVE  "190809190"  TO  WRK-GENSAN4
      **                        END-IF
                              PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                                  UNTIL     GENSAN-IDX2   >   31
                                MOVE   "1"  TO
                                        WRK-GENSAN-JYOKU(GENSAN-IDX2)
                              END-PERFORM
                        END-EVALUATE
                    ELSE
                        EVALUATE  GENSAN-IDX
                            WHEN  2
                              PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                                  UNTIL     GENSAN-IDX2   >   31
                                MOVE   SPACE  TO
                                        WRK-GENSAN-INNAI(GENSAN-IDX2)
                              END-PERFORM
                            WHEN  3
                              PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                                  UNTIL     GENSAN-IDX2   >   31
                                MOVE   SPACE  TO
                                        WRK-GENSAN-IRYO(GENSAN-IDX2)
                              END-PERFORM
                            WHEN  4
                              PERFORM VARYING GENSAN-IDX2 FROM IDV BY 1
                                  UNTIL     GENSAN-IDX2   >   31
                                MOVE   SPACE  TO
                                        WRK-GENSAN-JYOKU(GENSAN-IDX2)
                              END-PERFORM
                        END-EVALUATE
                    END-IF
                 END-PERFORM
                 MOVE    "tbl_syskanri"       TO  MCP-TABLE
                 MOVE    "key11"              TO  MCP-PATHNAME
                 PERFORM  910-SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE     "tbl_syskanri"            TO  MCP-TABLE
           MOVE     "key11"                   TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
      *
       210-SYS-5000-GET-EXT.
           EXIT.
      *
      ******************************************************************
      *    •∑•π•∆•‡¥…Õ˝ ‘Ω∏
      ******************************************************************
       210-SYSKAN-HENSYU-SEC                 SECTION.
      *
           INITIALIZE                         SYSKANRI-REC
           MOVE    "1001"                 TO  SYS-KANRICD
           MOVE    "*"                    TO  SYS-KBNCD
           MOVE    LNK-SROUNYUIN-HOSPNUM  TO  SYS-HOSPNUM
           MOVE    LNK-SROUNYUIN-SANTEIYM TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                   TO  SYS-STYUKYMD(7:2)
           MOVE    LNK-SROUNYUIN-SANTEIYM TO  SYS-EDYUKYMD(1:6)
           MOVE    "01"                   TO  SYS-EDYUKYMD(7:2)
           MOVE    SYSKANRI-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"             TO  MCP-FUNC
           MOVE    "tbl_syskanri"         TO  MCP-TABLE
           MOVE    "key10"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING   MCPAREA
                                                  MCPDATA-REC
                                                  SPA-AREA
           IF      MCP-RC              =  ZERO
               MOVE    "tbl_syskanri"     TO  MCP-TABLE
               MOVE    "key10"            TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               IF       FLG-SYSKANRI   =  ZERO
                 MOVE   SYS-1001-HOSPSBT  TO  WRK-HOSPSBT
               END-IF
           END-IF
           MOVE     "tbl_syskanri"        TO  MCP-TABLE
           MOVE     "key10"               TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
      *
       210-SYSKAN-HENSYU-EXT.
           EXIT.
      *
      ******************************************************************
      *    œ´∫“∆˛±°§Œœ´∫“æËøÙ° ¥¸¥÷≤√ªª°À§À¥ﬁ§·§Î√Ì≤√ªª§ŒºË∆¿
      ******************************************************************
       210-CHUKSN-GET-SEC                 SECTION.
      *
      *≈¿øÙ•ﬁ•π•ø§Ë§Í√Ì≤√ªª§Œ¬–æ›ø«Œ≈•≥°º•…§ÚºË∆¿
      *
           MOVE   ZERO                     TO  IDX1
           INITIALIZE                          TNS-TENSU-REC
           MOVE   LNK-SROUNYUIN-HOSPNUM    TO  TNS-HOSPNUM
           MOVE   LNK-SROUNYUIN-SANTEIYM   TO  TNS-YUKOSTYMD(1:6)
           MOVE   "01"                     TO  TNS-YUKOSTYMD(7:2)
           MOVE   LNK-SROUNYUIN-SANTEIYM   TO  TNS-YUKOEDYMD(1:6)
           MOVE  "01"                      TO  TNS-YUKOEDYMD(7:2)
           MOVE  "tbl_tensu"               TO  MCP-TABLE
           MOVE  "key30"                   TO  MCP-PATHNAME
           PERFORM 910-TENSU-SELECT-SEC
           IF      FLG-TENSU               =   ZERO
               MOVE     "tbl_tensu"        TO  MCP-TABLE
               MOVE     "key30"            TO  MCP-PATHNAME
               PERFORM   910-TENSU-FETCH-SEC
               PERFORM   UNTIL   (FLG-TENSU  =   1)   OR
                                 (IDX1      >= 100)
                 IF   TNS-SRYCD  =  "190134270"
                                 OR "190135470"
                                 OR "190135670"
                                 OR "190142970"
                                 OR "190143370"
                                 OR "190142870"
                                 OR "190143270"
                                 OR "190143470"
                                 OR "190144770"
                                 OR "190144870"
                                 OR "190144970"
                                 OR "190145070"
                                 OR "190145170"
                                 OR "190145270"
                                 OR "190135070"
      *
                                 OR "190161770"
                                 OR "190162170"
                                 OR "190161470"
                                 OR "190161570"
                                 OR "190161670"
                                 OR "190162070"
                                 OR "190161970"
                                 OR "190161870"
      *
                                 OR "190166070"
                                 OR "190169070"
                                 OR "190169170"
      *
                                 OR "190198570"
                                 OR "190198670"
      *
                                 OR "190201770"
                                 OR "190201870"
      *
                                 OR "190205570"
                                 OR "190205670"
                                 OR "190205770"
      *
                                 OR "190205970"
                                 OR "190206070"
                   CONTINUE
                 ELSE
                   ADD     1        TO  IDX1
                   MOVE  TNS-SRYCD  TO  LNK-SROUNYUIN-CHUKSNSRYCD(IDX1)
                 END-IF
                 MOVE   "tbl_tensu"      TO  MCP-TABLE
                 MOVE   "key30"          TO  MCP-PATHNAME
                 PERFORM 910-TENSU-FETCH-SEC
               END-PERFORM
           END-IF
      *    ≈¿øÙ•ﬁ•π•ø•Ø•Ì°º•∫
           MOVE     "tbl_tensu"       TO  MCP-TABLE
           MOVE     "key30"           TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *    œ´∫“Õ—•≥°º•… ‘Ω∏
           ADD     1          TO  IDX1
           MOVE  "101900100"  TO  LNK-SROUNYUIN-CHUKSNSRYCD(IDX1)
           IF    LNK-SROUNYUIN-SANTEIYM  <=  "201803"
            ADD    1          TO  IDX1
            MOVE "101900110"  TO  LNK-SROUNYUIN-CHUKSNSRYCD(IDX1)
           END-IF
           ADD     1          TO  IDX1
           MOVE  "101900120"  TO  LNK-SROUNYUIN-CHUKSNSRYCD(IDX1)
      *
           ADD     1          TO  IDX1
           MOVE  "101900130"  TO  LNK-SROUNYUIN-CHUKSNSRYCD(IDX1)
           ADD     1          TO  IDX1
           MOVE  "101900140"  TO  LNK-SROUNYUIN-CHUKSNSRYCD(IDX1)
      *
           ADD     1          TO  IDX1
           MOVE  "101900150"  TO  LNK-SROUNYUIN-CHUKSNSRYCD(IDX1)
           ADD     1          TO  IDX1
           MOVE  "101900160"  TO  LNK-SROUNYUIN-CHUKSNSRYCD(IDX1)
      *
           ADD     1          TO  IDX1
           MOVE  "190182690"  TO  LNK-SROUNYUIN-CHUKSNSRYCD(IDX1)
      *
           ADD     1          TO  IDX1
           MOVE  "190212670"  TO  LNK-SROUNYUIN-CHUKSNSRYCD(IDX1)
      *
           ADD     1          TO  IDX1
           MOVE  "101900170"  TO  LNK-SROUNYUIN-CHUKSNSRYCD(IDX1)
      *
           .
      *
       210-CHUKSN-GET-EXT.
           EXIT.
      *
      ******************************************************************
      *    ¬Ëª∞º‘π‘∞Ÿ§Œ¡™ƒÍ∆˛±°Œ¡ ‘Ω∏
      ******************************************************************
       220-DAISAN-HENSYU-SEC                 SECTION.
      *
           INITIALIZE                     SENTEICDCHG-REC
           MOVE    NSRY-SRYCD(1)      TO  SENCHG-IPNSRYCD
           MOVE    SENTEICDCHG-REC    TO  MCPDATA-REC
           MOVE    "DBSELECT"         TO  MCP-FUNC
           MOVE    "tbl_senteicdchg"  TO  MCP-TABLE
           MOVE    "key2"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING  MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
           IF      MCP-RC  =  ZERO
             MOVE  "DBFETCH"          TO  MCP-FUNC
             MOVE  "tbl_senteicdchg"  TO  MCP-TABLE
             MOVE  "key2"             TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"        USING  MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
             IF    MCP-RC  =  ZERO
               MOVE    MCPDATA-REC    TO  SENTEICDCHG-REC
               MOVE    ZERO           TO  FLG-SENTEICDCHG
             ELSE
               INITIALIZE                 SENTEICDCHG-REC
               MOVE    1              TO  FLG-SENTEICDCHG
             END-IF
           ELSE
               INITIALIZE                 SENTEICDCHG-REC
               MOVE    1              TO  FLG-SENTEICDCHG
           END-IF
      *
           IF  FLG-SENTEICDCHG   =   ZERO
             IF    SENCHG-IPNSRYCD  =  SENCHG-SENTEISRYCD
      *        ÃÎ∂–ª˛¥÷∑–≤·∏∫ªª§Œ¡™ƒÍ¬–±˛
      *        "190131410","190165810","190165910","190131310"
               IF  LNK-SROUNYUIN-SANTEIYM  >=  "201404"
                 MOVE  SENCHG-SENTEISRYCD  TO   WRK-SENTEI-SRYCD1
                 MOVE  "190122890"         TO   WRK-SENTEI-SRYCD2
               END-IF
             ELSE
                 MOVE  SENCHG-SENTEISRYCD  TO   WRK-SENTEI-SRYCD1
                 MOVE  SPACE               TO   WRK-SENTEI-SRYCD2
             END-IF
           END-IF
           MOVE    "tbl_senteicdchg"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM  910-DB-CLOSE-SEC
      *
           .
      *
       220-DAISAN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ∆˛±°Œ¨æŒ° ∑–¡º°À•¡•ß•√•Ø
      *****************************************************************
       500-RYAKUSYO-KEISO-CHECK-SEC                SECTION.
      *
           IF   (LNK-SROUNYUIN-SANTEIYM   >=   "201404")
           AND  (LNK-SROUNYUIN-SANTEIYM   <=   "201406")
           AND  (WRK-HOSPSBT               =    1      )
      *
             INITIALIZE    ORCSEIYOUKKS-AREA
             MOVE   LNK-SROUNYUIN-SANTEIYM
                                          TO  LNK-EIYOUKKS-SANTEIYM
             MOVE   WRK-TOKUREI-SRYCD2    TO  LNK-EIYOUKKS-SRYCD
             CALL   "ORCSEIYOUKKS" USING  ORCSEIYOUKKS-AREA
                                          SPA-AREA
             IF     LNK-EIYOUKKS-RECE-CHGSRYCD  NOT = SPACE
               MOVE LNK-EIYOUKKS-RECE-CHGSRYCD
                                          TO  WRK-TOKUREI-SRYCD
             END-IF
             IF     LNK-EIYOUKKS-RC  =  9
               IF   WRK-TOKUREI-SRYCD2(1:3)  =  "188"
                  CONTINUE
               ELSE
                  MOVE    1               TO  WRK-TOKUREI-FLG
               END-IF
             END-IF
      *
           END-IF
      *
           .
       500-RYAKUSYO-KEISO-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    •∑•π•∆•‡¥…Õ˝•ﬁ•π•ø°º∆…π˛
      *****************************************************************
       910-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-5000-REC
                                               SYS-1001-REC
                                               SYSKANRI-REC
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                      SYS-5000-REC
                                               SYS-1001-REC
                                               SYSKANRI-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           .
       910-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥µº‘•ﬁ•π•ø°º∆…π˛
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_ptinf"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE  MCPDATA-REC   TO  PTINF-REC
                   MOVE  ZERO          TO  FLG-PTINF
               ELSE
                   INITIALIZE              PTINF-REC
                   MOVE  1             TO  FLG-PTINF
               END-IF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           MOVE     "tbl_ptinf"        TO  MCP-TABLE
           MOVE     "key"              TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ∆˛±°≤Ò∑◊•ﬁ•π•ø°º∏°∫˜
      *****************************************************************
       910-NYUINACCT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-NYUINACCT
           ELSE
               MOVE    1                  TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ∆˛±°≤Ò∑◊•ﬁ•π•ø°º∆…π˛
      *****************************************************************
       910-NYUINACCT-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  NYUINACCT-REC
               MOVE    ZERO               TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                     NYUINACCT-REC
               MOVE    1                  TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ∆˛±°ø«Œ≈π‘∞Ÿ•ﬁ•π•ø°º∏°∫˜
      *****************************************************************
       910-NYUINACT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-NYUINACT
           ELSE
               MOVE    1                  TO  FLG-NYUINACT
           END-IF
      *
           .
       910-NYUINACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ∆˛±°ø«Œ≈π‘∞Ÿ•ﬁ•π•ø°º∆…π˛
      *****************************************************************
       910-NYUINACT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  NYUINACT-REC
               MOVE    ZERO                TO  FLG-NYUINACT
           ELSE
               INITIALIZE                      NYUINACT-REC
               MOVE    1                   TO  FLG-NYUINACT
           END-IF
      *
           .
       910-NYUINACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ø«Œ≈≤Ò∑◊•ﬁ•π•ø°º∏°∫˜
      *****************************************************************
       910-SRYACCT-SELECT-SEC        SECTION.
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-SRYACCT
           ELSE
               MOVE    1                  TO  FLG-SRYACCT
           END-IF
      *
           .
       910-SRYACCT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ø«Œ≈≤Ò∑◊•ﬁ•π•ø°º∆…π˛
      *****************************************************************
       910-SRYACCT-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  SRYACCT-REC
               MOVE    ZERO               TO  FLG-SRYACCT
           ELSE
               INITIALIZE                     SRYACCT-REC
               MOVE    1                  TO  FLG-SRYACCT
           END-IF
      *
           .
       910-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ø«Œ≈π‘∞Ÿ∆…π˛
      *****************************************************************
       910-SRYACT-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       910-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *     ›∏±¡»πÁ§ª•ﬁ•π•ø°º∏°∫˜
      *****************************************************************
       910-HKNCOMBI-SELECT-SEC        SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-HKNCOMBI
           ELSE
               MOVE    1                  TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     ›∏±¡»πÁ§ª•ﬁ•π•ø°º∆…π˛
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
               MOVE    ZERO                TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                      HKNCOMBI-REC
               MOVE    1                   TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *****************************************************************
      *    ≈¿øÙ•ﬁ•π•ø°º∆…π˛° ºÁ•≠°º°À
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   MOVE    1                   TO  FLG-TENSU
                   INITIALIZE                  TNS-TENSU-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           MOVE     "tbl_tensu"            TO  MCP-TABLE
           MOVE     "key"                  TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *     
      *****************************************************************
      *    ≈¿øÙ•ﬁ•π•ø°º∏°∫˜
      *****************************************************************
       910-TENSU-SELECT-SEC        SECTION.
      *
           MOVE    TNS-TENSU-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"             TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING  MCPAREA
                                              MCPDATA-REC
                                              SPA-AREA
           IF      MCP-RC                 =   ZERO
               MOVE    ZERO               TO  FLG-TENSU
           ELSE
               MOVE    1                  TO  FLG-TENSU
           END-IF
      *
           .
       910-TENSU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ≈¿øÙ•ﬁ•π•ø°º∆…π˛
      *****************************************************************
       910-TENSU-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"              TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING  MCPAREA
                                              MCPDATA-REC
                                              SPA-AREA
           IF      MCP-RC                 =   ZERO
               MOVE    MCPDATA-REC        TO  TNS-TENSU-REC
               MOVE    ZERO               TO  FLG-TENSU
           ELSE
               INITIALIZE                     TNS-TENSU-REC
               MOVE    1                  TO  FLG-TENSU
           END-IF
      *
           .
       910-TENSU-FETCH-EXT.
           EXIT.
      *     
      *****************************************************************
      *    ∆˛±°Œ¨æŒ•ﬁ•π•ø°º∆…π˛° ºÁ•≠°º°À
      *****************************************************************
       910-NYURYAKUSHO-READ-SEC        SECTION.
      *
           MOVE    NYURYAKUSHO-REC     TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_nyuinryakusho" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_nyuinryakusho" TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  NYURYAKUSHO-REC
                   IF  WRK-TOKUREI-FLG  =  1
                      MOVE    SPACE   TO   WRK-TOKUREI-RYAKUSHO
                      STRING  NYURYAKUSHO-RYAKUSHO
                                DELIMITED   BY  SPACE
                              "∑–¡º"
                                DELIMITED   BY  SIZE
                              INTO  WRK-TOKUREI-RYAKUSHO
                      END-STRING
                      MOVE    SPACE   TO   NYURYAKUSHO-RYAKUSHO
                      MOVE    WRK-TOKUREI-RYAKUSHO
                                      TO   NYURYAKUSHO-RYAKUSHO
                   END-IF
                   MOVE    ZERO                TO  FLG-NYURYAKUSHO
               ELSE
      *            µﬂÃøµﬂµﬁ∆˛±°Œ¡£±° £¥∆¸∞ æÂ£∑∆¸∞ ∆‚°À
      *            µﬂÃøµﬂµﬁ∆˛±°Œ¡£≤° £¥∆¸∞ æÂ£∑∆¸∞ ∆‚°À
      *            §Œ∆˛±°Œ¨æŒ•∆°º•÷•Î¬–±˛
                   IF      NYURYAKUSHO-SRYCD   =  "190128610"  OR
                                                  "190128710"
                       MOVE   "20080401"  TO  NYURYAKUSHO-YUKOSTYMD
                       MOVE   "99999999"  TO  NYURYAKUSHO-YUKOEDYMD
                       EVALUATE  NYURYAKUSHO-SRYCD
                          WHEN   "190128610"
                              MOVE  "µﬂÃø£±" TO NYURYAKUSHO-RYAKUSHO
                          WHEN   "190128710"
                              MOVE  "µﬂÃø£≤" TO NYURYAKUSHO-RYAKUSHO
                       END-EVALUATE
                       MOVE   ZERO          TO  FLG-NYURYAKUSHO
                   ELSE
                       MOVE    1            TO  FLG-NYURYAKUSHO
                       INITIALIZE               NYURYAKUSHO-REC
                   END-IF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-NYURYAKUSHO
               INITIALIZE                  NYURYAKUSHO-REC
           END-IF
      *
           MOVE     "tbl_nyuinryakusho"    TO  MCP-TABLE
           MOVE     "key"                  TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       910-NYURYAKUSHO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    £ƒ£¬•Ø•Ì°º•∫ΩËÕ˝
      *****************************************************************
       910-DB-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"          USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
      *
           .
      *
       910-DB-CLOSE-EXT.
           EXIT.
