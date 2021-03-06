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
      ************************************************************
      *    �ݸ��ȹ礻��    �� COPY  (CPHKNCOMBI.INC)             *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-¿��Ǽ  02/09/09  ��ʧ��ʬ�ɲ�
      *  04.01.00    NACL-¿��Ǽ  07/11/XX  ɽ����ʬ�ɲ�
      ************************************************************
      *01  HKNCOMBI-REC.
      *����
           03 COMB-KEY.
      *���ŵ��أɣ�
               05 COMB-HOSPNUM          PIC 9(2).
      *���ԣɣ�
               05 COMB-PTID            PIC  9(10).
      *�ȹ礻�ֹ�
               05 COMB-HKNCOMBINUM     PIC  9(04).
      *���ݸ��ݣɣ�
           03 COMB-HKNID               PIC  9(10).
      *���ݸ����ݸ��ֹ�
           03 COMB-HKNNUM              PIC  X(03).
      *���ݸ����ܿͲ�²��ʬ
           03 COMB-HONKZKKBN           PIC  X(01).
      *���ݸ��������ʬ
           03 COMB-HOJOKBN             PIC  X(01).
      *���ݸ��ݷ�³��ʬ
           03 COMB-CONTKBN             PIC  X(01).
      *�����ΰ�
           03 COMB-KOHI-AREA.
      *���񣱡ݣɣ�
               05 COMB-KOH1ID              PIC  9(10).
      *���񣱡��ݸ��ֹ�
               05 COMB-KOH1HKNNUM          PIC  X(03).
      *���񣱡ݻ�ʧ��ʬ
               05 COMB-KOH1PAYKBN          PIC  X(02).
      *���񣲡ݣɣ�
               05 COMB-KOH2ID              PIC  9(10).
      *���񣲡��ݸ��ֹ�
               05 COMB-KOH2HKNNUM          PIC  X(03).
      *���񣲡ݻ�ʧ��ʬ
               05 COMB-KOH2PAYKBN          PIC  X(02).
      *���񣳡ݣɣ�
               05 COMB-KOH3ID              PIC  9(10).
      *���񣳡��ݸ��ֹ�
               05 COMB-KOH3HKNNUM          PIC  X(03).
      *���񣳡ݻ�ʧ��ʬ
               05 COMB-KOH3PAYKBN          PIC  X(02).
      *���񣴡ݣɣ�
               05 COMB-KOH4ID              PIC  9(10).
      *���񣴡��ݸ��ֹ�
               05 COMB-KOH4HKNNUM          PIC  X(03).
      *���񣴡ݻ�ʧ��ʬ
               05 COMB-KOH4PAYKBN          PIC  X(02).
           03 COMB-KOHI-AREA-R     REDEFINES  COMB-KOHI-AREA
                                           OCCURS   4.
      *����ݣɣ�
               05 COMB-KOHID              PIC  9(10).
      *������ݸ��ֹ�
               05 COMB-KOHHKNNUM          PIC  X(03).
      *����ݻ�ʧ��ʬ
               05 COMB-KOHPAYKBN          PIC  X(02).
      *Ŭ�ѳ�����
           03 COMB-TEKSTYMD               PIC  X(08).
      *Ŭ�ѽ�λ��
           03 COMB-TEKEDYMD               PIC  X(08).
      *���ݸ���û������̾
           03 COMB-SYU-TANSEIDONAME       PIC  X(40).
      *���ݸ����ݸ��������
           03 COMB-SYU-HKNKOHSBTKBN       PIC  X(01).
      *���ݸ��ݳ���ݾ�¶�ʬ
           03 COMB-SYU-GAIJGNKBN          PIC  X(01).
      *���ݸ��������ݾ�¶�ʬ
           03 COMB-SYU-NYUJGNKBN          PIC  X(01).
      *�����û������̾
           03 COMB-KOHTANSEIDO.
      *���񣱡�û������̾
               05 COMB-KOH1-TANSEIDONAME  PIC  X(20).
      *���񣱡��ݸ��������
               05 COMB-KOH1-HKNKOHSBTKBN  PIC  X(01).
      *���񣱡ݳ���ݾ�¶�ʬ
               05 COMB-KOH1-GAIJGNKBN     PIC  X(01).
      *���񣱡������ݾ�¶�ʬ
               05 COMB-KOH1-NYUJGNKBN     PIC  X(01).
      *���񣲡�û������̾
               05 COMB-KOH2-TANSEIDONAME  PIC  X(20).
      *���񣲡��ݸ��������
               05 COMB-KOH2-HKNKOHSBTKBN  PIC  X(01).
      *���񣲡ݳ���ݾ�¶�ʬ
               05 COMB-KOH2-GAIJGNKBN     PIC  X(01).
      *���񣲡������ݾ�¶�ʬ
               05 COMB-KOH2-NYUJGNKBN     PIC  X(01).
      *���񣳡�û������̾
               05 COMB-KOH3-TANSEIDONAME  PIC  X(20).
      *���񣳡��ݸ��������
               05 COMB-KOH3-HKNKOHSBTKBN  PIC  X(01).
      *���񣳡ݳ���ݾ�¶�ʬ
               05 COMB-KOH3-GAIJGNKBN     PIC  X(01).
      *���񣳡������ݾ�¶�ʬ
               05 COMB-KOH3-NYUJGNKBN     PIC  X(01).
      *���񣴡�û������̾
               05 COMB-KOH4-TANSEIDONAME  PIC  X(20).
      *���񣴡��ݸ��������
               05 COMB-KOH4-HKNKOHSBTKBN  PIC  X(01).
      *���񣴡ݳ���ݾ�¶�ʬ
               05 COMB-KOH4-GAIJGNKBN     PIC  X(01).
      *���񣴡������ݾ�¶�ʬ
               05 COMB-KOH4-NYUJGNKBN     PIC  X(01).
      *�����û������̾
           03 COMB-KOHTANSEIDO-R  REDEFINES  COMB-KOHTANSEIDO
                                           OCCURS  4.
      *�����û������̾
               05 COMB-KOH-TANSEIDONAME   PIC  X(20).
      *������ݸ��������
               05 COMB-KOH-HKNKOHSBTKBN   PIC  X(01).
      *����ݳ���ݾ�¶�ʬ
               05 COMB-KOH-GAIJGNKBN      PIC  X(01).
      *����������ݾ�¶�ʬ
               05 COMB-KOH-NYUJGNKBN      PIC  X(01).
      *���������
           03 COMB-GAINYU.
      *�������ô��ʬ
             05 COMB-GAI-FTNKBN           PIC  X(01).
      *  ���������
             05 COMB-GAI-NAIYO.
      *    ����ݲ����ô���
               07 COMB-GAI-KFTNRATE           PIC  9(03).
      *    ����ݲ�ݸ����
               07 COMB-GAI-KKOTGAK            PIC  9(06).
      *    ����ݲ�ݾ�³�
               07 COMB-GAI-KJGNGAK            PIC  9(06).
      *    ��������ݾ�³�
               07 COMB-GAI-DJGNGAK            PIC  9(06).
      *    ��������ݾ�²��
               07 COMB-GAI-DJGNCNT            PIC  9(02).
      *    ����ݷ�ݱ���ݾ�³�
               07 COMB-GAI-MNAIJGNGAK         PIC  9(06).
      *    ����ݷ�ݱ����ݾ�³�
               07 COMB-GAI-MGAIJGNGAK         PIC  9(06).
      *    ����ݷ�ݾ�²��
               07 COMB-GAI-MJGNCNT            PIC  9(02).
      *  �����������ô��ʬ
             05 COMB-GAI-YKZFTNKBN        PIC  X(01).
      *  ��������ô��ʬ
             05 COMB-NYU-FTNKBN           PIC  X(01).
      *  ����������
             05 COMB-NYU-NAIYO.
      *    �����ݲ����ô���
               07 COMB-NYU-KFTNRATE            PIC  9(03).
      *    �����ݲ�ݸ����
               07 COMB-NYU-KKOTGAK             PIC  9(06).
      *    �����ݲ�ݾ�³�
               07 COMB-NYU-KJGNGAK             PIC  9(06).
      *    ���������ݾ�³�
               07 COMB-NYU-DJGNGAK             PIC  9(06).
      *    ���������ݾ�²��
               07 COMB-NYU-DJGNCNT             PIC  9(02).
      *    �����ݷ�ݾ�³�
               07 COMB-NYU-MJGNGAK             PIC  9(06).
      *    �����ݷ�ݾ�²��
               07 COMB-NYU-MJGNCNT             PIC  9(02).
      *  �����ݿ����������ʬ
             05 COMB-NYU-SKJRYOKBN        PIC  X(01).
      *�����
           03 COMB-TSY.
      *  ������ݳ������ô��ʬ
             05 COMB-TSY-GAIFTNKBN        PIC  X(01).
      *  �����������
             05 COMB-TSY-GAI-NAIYO.
      *    ������ݳ���ݲ����ô���
               07 COMB-TSY-GAIKFTNRATE         PIC  9(03).
      *    ������ݳ���ݲ�ݸ����
               07 COMB-TSY-GAIKKOTGAK          PIC  9(06).
      *    ������ݳ���ݲ�ݾ�³�
               07 COMB-TSY-GAIKJGNGAK          PIC  9(06).
      *    ������ݳ�������ݾ�³�
               07 COMB-TSY-GAIDJGNGAK          PIC  9(06).
      *    ������ݳ�������ݾ�²��
               07 COMB-TSY-GAIDJGNCNT          PIC  9(02).
      *    ������ݳ���ݷ�ݱ���ݾ�³�
               07 COMB-TSY-GAIMNAIJGNGAK       PIC  9(06).
      *    ������ݳ���ݷ�ݱ����ݾ�³�
               07 COMB-TSY-GAIMGAIJGNGAK       PIC  9(06).
      *    ������ݳ���ݷ�ݾ�²��
               07 COMB-TSY-GAIMJGNCNT          PIC  9(02).
      *  ������ݳ����������ô��ʬ
             05 COMB-TSY-GAIYKZFTNKBN     PIC  X(01).
      *  ���������������ô��ʬ
             05 COMB-TSY-NYUFTNKBN        PIC  X(01).
      *  �����������������
             05 COMB-TSY-NYU-NAIYO.
      *    ������������ݲ����ô���
               07 COMB-TSY-NYUKFTNRATE        PIC  9(03).
      *    ������������ݲ�ݸ����
               07 COMB-TSY-NYUKKOTGAK         PIC  9(06).
      *    ������������ݲ�ݾ�³�
               07 COMB-TSY-NYUKJGNGAK         PIC  9(06).
      *    ����������������ݾ�³�
               07 COMB-TSY-NYUDJGNGAK         PIC  9(06).
      *    ����������������ݾ�²��
               07 COMB-TSY-NYUDJGNCNT         PIC  9(02).
      *    ������������ݷ�ݾ�³�
               07 COMB-TSY-NYUMJGNGAK         PIC  9(06).
      *    ������������ݷ�ݾ�²��
               07 COMB-TSY-NYUMJGNCNT         PIC  9(02).
      *  ������������ݿ������ܶ�ʬ
             05 COMB-TSY-NYUSKJRYOKBN     PIC  X(01).
      *��ǯ��
           03 COMB-TNK.
      *  ��ǯ��ݳ������ô��ʬ
             05 COMB-TNK-GAIFTNKBN        PIC  X(01).
      *  ��ǯ��ݳ��������
             05 COMB-TNK-GAI-NAIYO.
      *    ��ǯ��ݳ���ݲ����ô���
               07 COMB-TNK-GAIKFTNRATE      PIC  9(03).
      *    ��ǯ��ݳ���ݲ�ݸ����
               07 COMB-TNK-GAIKKOTGAK       PIC  9(06).
      *    ��ǯ��ݳ���ݲ�ݾ�³�
               07 COMB-TNK-GAIKJGNGAK       PIC  9(06).
      *    ��ǯ��ݳ�������ݾ�³�
               07 COMB-TNK-GAIDJGNGAK       PIC  9(06).
      *    ��ǯ��ݳ�������ݾ�²��
               07 COMB-TNK-GAIDJGNCNT       PIC  9(02).
      *    ��ǯ��ݳ���ݷ�ݱ���ݾ�³�
               07 COMB-TNK-GAIMNAIJGNGAK    PIC  9(06).
      *    ��ǯ��ݳ���ݷ�ݱ����ݾ�³�
               07 COMB-TNK-GAIMGAIJGNGAK    PIC  9(06).
      *    ��ǯ��ݳ���ݷ�ݾ�²��
               07 COMB-TNK-GAIMJGNCNT       PIC  9(02).
      *  ��ǯ��ݳ����������ô��ʬ
             05 COMB-TNK-GAIYKZFTNKBN     PIC  X(01).
      *  ��ǯ�����������ô��ʬ
             05 COMB-TNK-NYUFTNKBN        PIC  X(01).
      *  ��ǯ�������������
             05 COMB-TNK-NYU-NAIYO.
      *    ��ǯ��������ݲ����ô���
               07 COMB-TNK-NYUKFTNRATE        PIC  9(03).
      *    ��ǯ��������ݲ�ݸ����
               07 COMB-TNK-NYUKKOTGAK         PIC  9(06).
      *    ��ǯ��������ݲ�ݾ�³�
               07 COMB-TNK-NYUKJGNGAK         PIC  9(06).
      *    ��ǯ������������ݾ�³�
               07 COMB-TNK-NYUDJGNGAK         PIC  9(06).
      *    ��ǯ������������ݾ�²��
               07 COMB-TNK-NYUDJGNCNT         PIC  9(02).
      *    ��ǯ��������ݷ�ݾ�³�
               07 COMB-TNK-NYUMJGNGAK         PIC  9(06).
      *    ��ǯ��������ݷ�ݾ�²��
               07 COMB-TNK-NYUMJGNCNT         PIC  9(02).
      *  ��ǯ��������ݿ������ܶ�ʬ
             05 COMB-TNK-NYUSKJRYOKBN     PIC  X(01).
      *����ñ��
           03 COMB-TENTNK                 PIC  9(04).
      *�����Ƕ�ʬ
           03 COMB-TAXKBN                 PIC  X(01).
      *�ѹ���ʬ
           03 COMB-CHGKBN                 PIC  X(01).
      *�����ʬ
           03 COMB-DELKBN                 PIC  X(01).
      *ɽ����ʬ
           03 COMB-HYOJIKBN               PIC  X(01).
      *ü���ɣ�
           03 COMB-TERMID                 PIC  X(16).
      *���ڡ��졼���ɣ�
           03 COMB-OPID                   PIC  X(16).
      *����ǯ����
           03 COMB-CREYMD                 PIC  X(08).
      *����ǯ����
           03 COMB-UPYMD                  PIC  X(08).
      *��������
           03 COMB-UPHMS                  PIC  X(06).
