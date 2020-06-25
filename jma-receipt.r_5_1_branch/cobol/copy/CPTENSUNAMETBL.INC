      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
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
      *----------------------------------------------------------------*
      *    �����ơ��֥����̾�Υơ��֥� COPY (CPTENSUNAMETBL.INC)
      *----------------------------------------------------------------*
      *****************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       01  TBL-TENSU-NAME-AREA.
           03  FILLER                      PIC X(41)   VALUE
                       "001 srycd              ���Ź԰٥�����    ".
           03  FILLER                      PIC X(41)   VALUE
                       "002 yukostymd          ͭ��������        ".
           03  FILLER                      PIC X(41)   VALUE
                       "003 yukoedymd          ͭ����λ��        ".
           03  FILLER                      PIC X(41)   VALUE
                       "004 srykbn             ���Ź԰ٶ�ʬ      ".
           03  FILLER                      PIC X(41)   VALUE
                       "005 srysyukbn          ���ż��̶�ʬ      ".
           03  FILLER                      PIC X(41)   VALUE
                       "006 yukoketa           ����̾�η��      ".
           03  FILLER                      PIC X(41)   VALUE
                       "007 name               ����̾��          ".
           03  FILLER                      PIC X(41)   VALUE
                       "008 kanayukoketa       ����̾�η��      ".
           03  FILLER                      PIC X(41)   VALUE
                       "009 kananame           ����̾��          ".
           03  FILLER                      PIC X(41)   VALUE
                       "010 formalyukoketa     ����̾�η��      ".
      *
           03  FILLER                      PIC X(41)   VALUE
                       "011 formalname         ����̾��          ".
           03  FILLER                      PIC X(41)   VALUE
                       "012 tensikibetu        ��������          ".
           03  FILLER                      PIC X(41)   VALUE
                       "013 ten                ���������        ".
           03  FILLER                      PIC X(41)   VALUE
                       "014 tanicd             ñ�̥�����        ".
           03  FILLER                      PIC X(41)   VALUE
                       "015 tanimojisu         ñ��̾�η��      ".
           03  FILLER                      PIC X(41)   VALUE
                       "016 taniname           ñ��̾��          ".
           03  FILLER                      PIC X(41)   VALUE
                       "017 datakbn            �ǡ�����ʬ        ".
           03  FILLER                      PIC X(41)   VALUE
                       "018 hkntekkbn          �ݸ�Ŭ�Ѷ�ʬ      ".
           03  FILLER                      PIC X(41)   VALUE
                       "019 nyugaitekkbn       ����Ŭ�Ѷ�ʬ      ".
           03  FILLER                      PIC X(41)   VALUE
                       "020 routekkbn          ��������Ŭ�Ѷ�ʬ".
      *
           03  FILLER                      PIC X(41)   VALUE
                       "021 hospsrykbn         �±������Ž��ʬ  ".
           03  FILLER                      PIC X(41)   VALUE
                       "022 osinkbn            ���Ƕ�ʬ��̤���ѡ�".
           03  FILLER                      PIC X(41)   VALUE
                       "023 houksnkbn          ����оݸ���      ".
           03  FILLER                      PIC X(41)   VALUE
                       "024 byokanrenkbn       ����̾��Ϣ��ʬ    ".
           03  FILLER                      PIC X(41)   VALUE
                       "025 sdokanryo          ��ش�����        ".
           03  FILLER                      PIC X(41)   VALUE
                       "026 jituday            ������            ".
           03  FILLER                      PIC X(41)   VALUE
                       "027 daycnt             ���������        ".
           03  FILLER                      PIC X(41)   VALUE
                       "028 ykzknrkbn          �����ʴ�Ϣ��ʬ    ".
           03  FILLER                      PIC X(41)   VALUE
                       "029 kzmcompsikibetu    �������ͷ׻�����  ".
           03  FILLER                      PIC X(41)   VALUE
                       "030 kzmkgn             �������Ͳ�����    ".
      *
           03  FILLER                      PIC X(41)   VALUE
                       "031 kzmjgn             �������;����    ".
           03  FILLER                      PIC X(41)   VALUE
                       "032 kzm                ��������          ".
           03  FILLER                      PIC X(41)   VALUE
                       "033 kzmten             ����������        ".
           03  FILLER                      PIC X(41)   VALUE
                       "034 kzmerr             �������ͥ��顼����".
           03  FILLER                      PIC X(41)   VALUE
                       "035 jgncnt             ��²������      ".
           03  FILLER                      PIC X(41)   VALUE
                       "036 jgncnt1d           ��²������      ".
           03  FILLER                      PIC X(41)   VALUE
                       "037 jgncnt1w           ��²������      ".
           03  FILLER                      PIC X(41)   VALUE
                       "038 jgncntetcm         ��²��¾���    ".
           03  FILLER                      PIC X(41)   VALUE
                       "039 jgncntetc          ��²��¾��      ".
           03  FILLER                      PIC X(41)   VALUE
                       "040 jgncnterr          ��²�����顼����".
      *
           03  FILLER                      PIC X(41)   VALUE
                       "041 chuksncd           ���û�������      ".
           03  FILLER                      PIC X(41)   VALUE
                       "042 chuksntsuban       ���û�����        ".
           03  FILLER                      PIC X(41)   VALUE
                       "043 tsusokuage         ��§ǯ��          ".
           03  FILLER                      PIC X(41)   VALUE
                       "044 kgnage             ����ǯ��          ".
           03  FILLER                      PIC X(41)   VALUE
                       "045 jgnage             ���ǯ��          ".
           03  FILLER                      PIC X(41)   VALUE
                       "046 timeksnkbn         ���ֲû���ʬ      ".
           03  FILLER                      PIC X(41)   VALUE
                       "047 kijunteigenkbn     ���Ŭ���ʬ      ".
           03  FILLER                      PIC X(41)   VALUE
                       "048 kijunteigencd      ���Ŭ����ߴ��  ".
           03  FILLER                      PIC X(41)   VALUE
                       "049 laserksn           �������Ļ��û���ʬ".
           03  FILLER                      PIC X(41)   VALUE
                       "050 chpmesuksn         ��������νŻ��û�".
      *
           03  FILLER                      PIC X(41)   VALUE
                       "051 micmesuksn         ����������������  ".
           03  FILLER                      PIC X(41)   VALUE
                       "052 donorkbn           �ɥʡ�ʬ���׶�ʬ  ".
           03  FILLER                      PIC X(41)   VALUE
                       "053 knsjiskbn          �������»�Ƚ�Ƕ�ʬ".
           03  FILLER                      PIC X(41)   VALUE
                       "054 knsjisgrpkbn       �������»�Ƚ�ǣ�  ".
           03  FILLER                      PIC X(41)   VALUE
                       "055 teigenkbn          �����оݶ�ʬ      ".
           03  FILLER                      PIC X(41)   VALUE
                       "056 sekizuisokutei     �Կ�Ͷȯ�Ű�¬��  ".
           03  FILLER                      PIC X(41)   VALUE
                       "057 keibujyutu         ����������ʻ�ܲû�".
           03  FILLER                      PIC X(41)   VALUE
                       "058 autohougo          ��ư˥���û���ʬ".
           03  FILLER                      PIC X(41)   VALUE
                       "059 gaikanrikbn        ��������û���ʬ  ".
           03  FILLER                      PIC X(41)   VALUE
                       "060 tusokugaikbn       ��§�û���������  ".
      *
           03  FILLER                      PIC X(41)   VALUE
                       "061 hokatuteigenkbn    ���������ʬ      ".
           03  FILLER                      PIC X(41)   VALUE
                       "062 chpnaisiksn        Ķ���������û�  ".
           03  FILLER                      PIC X(41)   VALUE
                       "063 autofungo          ��ưʭ���û���ʬ".
           03  FILLER                      PIC X(41)   VALUE
                       "064 chiikiksn          �ϰ�û�          ".
           03  FILLER                      PIC X(41)   VALUE
                       "065 byosyokbn          �¾�����ʬ        ".
           03  FILLER                      PIC X(41)   VALUE
                       "066 chpgyokosotiksn    Ķ���ȶŸ��ڳ�����".
           03  FILLER                      PIC X(41)   VALUE
                       "067 shortstayope       û���ں߼��      ".
           03  FILLER                      PIC X(41)   VALUE
                       "068 buikbn             ���̶�ʬ          ".
           03  FILLER                      PIC X(41)   VALUE
                       "069 santeirrkkbn       ���������ʬ      ".
           03  FILLER                      PIC X(41)   VALUE
                       "070 sstkijuncd1        ���ߴ�ࣱ        ".
      *
           03  FILLER                      PIC X(41)   VALUE
                       "071 sstkijuncd2        ���ߴ�ࣲ        ".
           03  FILLER                      PIC X(41)   VALUE
                       "072 sstkijuncd3        ���ߴ�ࣳ        ".
           03  FILLER                      PIC X(41)   VALUE
                       "073 sstkijuncd4        ���ߴ�ࣴ        ".
           03  FILLER                      PIC X(41)   VALUE
                       "074 sstkijuncd5        ���ߴ�ࣵ        ".
           03  FILLER                      PIC X(41)   VALUE
                       "075 sstkijuncd6        ���ߴ�ࣶ        ".
           03  FILLER                      PIC X(41)   VALUE
                       "076 sstkijuncd7        ���ߴ�ࣷ        ".
           03  FILLER                      PIC X(41)   VALUE
                       "077 sstkijuncd8        ���ߴ�ࣸ        ".
           03  FILLER                      PIC X(41)   VALUE
                       "078 sstkijuncd9        ���ߴ�ࣹ        ".
           03  FILLER                      PIC X(41)   VALUE
                       "079 sstkijuncd10       ���ߴ�ࣱ��      ".
           03  FILLER                      PIC X(41)   VALUE
                       "080 ageksnkgn1         ǯ��û�����ǯ��".
      *
           03  FILLER                      PIC X(41)   VALUE
                       "081 ageksnjgn1         ǯ��û����ǯ��".
           03  FILLER                      PIC X(41)   VALUE
                       "082 ageksnsrycd1       ǯ��û������ɣ�  ".
           03  FILLER                      PIC X(41)   VALUE
                       "083 ageksnkgn2         ǯ��û�����ǯ��".
           03  FILLER                      PIC X(41)   VALUE
                       "084 ageksnjgn2         ǯ��û����ǯ��".
           03  FILLER                      PIC X(41)   VALUE
                       "085 ageksnsrycd2       ǯ��û������ɣ�  ".
           03  FILLER                      PIC X(41)   VALUE
                       "086 ageksnkgn3         ǯ��û�����ǯ��".
           03  FILLER                      PIC X(41)   VALUE
                       "087 ageksnjgn3         ǯ��û����ǯ��".
           03  FILLER                      PIC X(41)   VALUE
                       "088 ageksnsrycd3       ǯ��û������ɣ�  ".
           03  FILLER                      PIC X(41)   VALUE
                       "089 ageksnkgn4         ǯ��û�����ǯ��".
           03  FILLER                      PIC X(41)   VALUE
                       "090 ageksnjgn4         ǯ��û����ǯ��".
      *
           03  FILLER                      PIC X(41)   VALUE
                       "091 ageksnsrycd4       ǯ��û������ɣ�  ".
           03  FILLER                      PIC X(41)   VALUE
                       "092 kentaicomment      ���θ���������  ".
           03  FILLER                      PIC X(41)   VALUE
                       "093 nyukhnkbn          ������������ʬ    ".
           03  FILLER                      PIC X(41)   VALUE
                       "094 nyukhnksnkbn       �����������û���ʬ".
           03  FILLER                      PIC X(41)   VALUE
                       "095 kangoksn           �Ǹ�û�          ".
           03  FILLER                      PIC X(41)   VALUE
                       "096 oldtenskb          ����������        ".
           03  FILLER                      PIC X(41)   VALUE
                       "097 oldten             �����������      ".
           03  FILLER                      PIC X(41)   VALUE
                       "098 madokukbn          ��������������ʬ  ".
           03  FILLER                      PIC X(41)   VALUE
                       "099 sinkeihakaikbn     �����˲���        ".
           03  FILLER                      PIC X(41)   VALUE
                       "100 seibutugakukbn     ��ʪ��Ū����      ".
      *
           03  FILLER                      PIC X(41)   VALUE
                       "101 zoeizaikbn         ¤�ơ�����˺�    ".
           03  FILLER                      PIC X(41)   VALUE
                       "102 csyyouryo          ��������          ".
           03  FILLER                      PIC X(41)   VALUE
                       "103 ykzkbn             ���޶�ʬ          ".
           03  FILLER                      PIC X(41)   VALUE
                       "104 zaigatakbn         �޷�              ".
           03  FILLER                      PIC X(41)   VALUE
                       "105 kouhatukbn         ��ȯ��            ".
           03  FILLER                      PIC X(41)   VALUE
                       "106 longtoyokbn        Ĺ����Ϳ��̤���ѡ�".
           03  FILLER                      PIC X(41)   VALUE
                       "107 meiuseskb          ̾�λ��Ѽ���      ".
           03  FILLER                      PIC X(41)   VALUE
                       "108 tokukizaiageksnkbn ǯ��û���ʬ      ".
           03  FILLER                      PIC X(41)   VALUE
                       "109 sansokbn           ��������ʬ        ".
           03  FILLER                      PIC X(41)   VALUE
                       "110 tokukizaisbt1      ���������      ".
      *
           03  FILLER                      PIC X(41)   VALUE
                       "111 tokukizaisbt2      ��²���          ".
           03  FILLER                      PIC X(41)   VALUE
                       "112 jgnten             �������          ".
           03  FILLER                      PIC X(41)   VALUE
                       "113 gaitenttlkbn       �����󽸷��衦����".
           03  FILLER                      PIC X(41)   VALUE
                       "114 nyutenttlkbn       �����󽸷��衦����".
           03  FILLER                      PIC X(41)   VALUE
                       "115 cdkbn_kbn          ������ɽ����ʬ    ".
           03  FILLER                      PIC X(41)   VALUE
                       "116 cdkbn_syo          ������ɽ����      ".
           03  FILLER                      PIC X(41)   VALUE
                       "117 cdkbn_bu           ������ɽ����      ".
           03  FILLER                      PIC X(41)   VALUE
                       "118 cdkbn_kbnnum       ������ɽ����ʬ�ֹ�".
           03  FILLER                      PIC X(41)   VALUE
                       "119 cdkbn_kbnnum_eda   ������ɽ������    ".
           03  FILLER                      PIC X(41)   VALUE
                       "120 cdkbn_kouban       ������ɽ������    ".
      *
           03  FILLER                      PIC X(41)   VALUE
                       "121 kokuchi_kbn        �������Ρ���ʬ    ".
           03  FILLER                      PIC X(41)   VALUE
                       "122 kokuchi_syo        �������Ρ���      ".
           03  FILLER                      PIC X(41)   VALUE
                       "123 kokuchi_bu         �������Ρ���      ".
           03  FILLER                      PIC X(41)   VALUE
                       "124 kokuchi_kbnnum     �������Ρ���ʬ�ֹ�".
           03  FILLER                      PIC X(41)   VALUE
                       "125 kokuchi_kbnnum_eda �������Ρ�����    ".
           03  FILLER                      PIC X(41)   VALUE
                       "126 kokuchi_kouban     �������Ρ�����    ".
           03  FILLER                      PIC X(41)   VALUE
                       "127 kokujiskbkbn1      �������̶�ʬ��  ".
           03  FILLER                      PIC X(41)   VALUE
                       "128 kokujiskbkbn2      �������̶�ʬ��  ".
           03  FILLER                      PIC X(41)   VALUE
                       "129 namechgkbn         ����̾���ѹ���ʬ  ".
           03  FILLER                      PIC X(41)   VALUE
                       "130 kananamechgkbn     ����̾���ѹ���ʬ  ".
      *
           03  FILLER                      PIC X(41)   VALUE
                       "131 idokanren          ��ư��Ϣ          ".
           03  FILLER                      PIC X(41)   VALUE
                       "132 kouhyojyunnum      ��ɽ����ֹ�      ".
           03  FILLER                      PIC X(41)   VALUE
                       "133 yakkakjncd         ������ॳ����    ".
           03  FILLER                      PIC X(41)   VALUE
                       "134 syusaiskb          ��������������    ".
           03  FILLER                      PIC X(41)   VALUE
                       "135 syomeikanren       ����̾����Ϣ      ".
           03  FILLER                      PIC X(41)   VALUE
                       "136 chgymd             �ѹ�ǯ����        ".
           03  FILLER                      PIC X(41)   VALUE
                       "137 haisiymd           �ѻ�ǯ����        ".
           03  FILLER                      PIC X(41)   VALUE
                       "138 keikasochiymd      �в�����ǯ����    ".
           03  FILLER                      PIC X(41)   VALUE
                       "139 termid             ü���ɣ�          ".
           03  FILLER                      PIC X(41)   VALUE
                       "140 opid               ���ڥ졼�����ɣ�  ".
      *
           03  FILLER                      PIC X(41)   VALUE
                       "141 creymd             ����ǯ����        ".
           03  FILLER                      PIC X(41)   VALUE
                       "142 upymd              ����ǯ����        ".
           03  FILLER                      PIC X(41)   VALUE
                       "143 uphms              ��������          ".
           03  FILLER                      PIC X(41)   VALUE
                       "144 hospnum            ���ŵ��ؼ����ֹ�  ".
           03  FILLER                      PIC X(41)   VALUE
                       "145 gazoopesup         ��������ѻٱ�û�".
           03  FILLER                      PIC X(41)   VALUE
                       "146 iryokansatukbn     ���Ŵѻ�ˡ�оݶ�ʬ".
           03  FILLER                      PIC X(41)   VALUE
                       "147 masuiskbkbn        ��켱�̶�ʬ      ".
      *H24.4�ɲã���
           03  FILLER                      PIC X(41)   VALUE
                       "148 fukubikunaikbn     ��ɡ������      ".
           03  FILLER                      PIC X(41)   VALUE
                       "149 fukubikukotukbn    ��ɡ�й�����      ".
           03  FILLER                      PIC X(41)   VALUE
                       "150 timemasuikbn       Ĺ�������        ".
           03  FILLER                      PIC X(41)   VALUE
                       "151 dpckbn             �ģУö�ʬ        ".
      *H28.4�ɲã���
           03  FILLER                      PIC X(41)   VALUE
                       "152 hisinsyumonitorksn �󿯽�Ū���ư�֥�".
           03  FILLER                      PIC X(41)   VALUE
                       "153 touketuhozonksn    �����¸Ʊ���ȿ���".
           03  FILLER                      PIC X(41)   VALUE
                       "154 kubunbangou        ����ɽ��ʬ�ֹ�    ".
           03  FILLER                      PIC X(41)   VALUE
                       "155 rosaikbn           ϫ�һ����Բ�      ".
           03  FILLER                      PIC X(41)   VALUE
                       "156 sisiksn            �ͻ�û���ϫ�ҡ�  ".
      *H30.4�ɲã���
           03  FILLER                      PIC X(41)   VALUE
                       "157 akuseibyoriksn     �������������ȿ�  ".
      *R02.4�ɲã���
           03  FILLER                      PIC X(41)   VALUE
                       "158 sogaikoteiksn      �ϳ������û�    ".
           03  FILLER                      PIC X(41)   VALUE
                       "159 chpsessakuksn      Ķ�����ں��û�  ".
           03  FILLER                      PIC X(41)   VALUE
                       "160 sasinjiheisikbn    �������ĺ���ʻ�ܶ�".
      *
       01  TBL-TENSU-NAME-AREA-R   REDEFINES   TBL-TENSU-NAME-AREA.
           03  TBL-TENSU-NAME-OCC  OCCURS  160 INDEXED  TBL-TNM.
               05  TBL-TNM-NUM                 PIC 9(03).
               05  TBL-TNM-FILLER1             PIC X(01).
               05  TBL-TNM-CNAME               PIC X(18).
               05  TBL-TNM-FILLER2             PIC X(01).
               05  TBL-TNM-NAME                PIC X(18).
      *
       01  TBL-TENSU-NAME-MAX          PIC 9(04)  VALUE  160.