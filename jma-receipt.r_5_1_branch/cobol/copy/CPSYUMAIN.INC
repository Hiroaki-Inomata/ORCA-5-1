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
      *    ����Ǽ�ޥ���         COPY (CPSYUMAIN.INC)             *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  SYUMAIN-REC.
           03  SYUMAIN-KEY.
      *���ŵ��أɣ�
               05  SYUMAIN-HOSPNUM          PIC 9(2).
      *������ʬ
               05  SYUMAIN-NYUGAIKBN       PIC  X(01).
      *���ԣɣ�
               05  SYUMAIN-PTID            PIC  9(10).
      *��ɼ�ֹ�
               05  SYUMAIN-DENPNUM         PIC  9(07).
      *��ɼ���ֺǽ��ֹ�
           03  SYUMAIN-DENPLASTNUM         PIC  9(02).
      *���Ų�
           03  SYUMAIN-SRYKA               PIC  X(02).
      *������
           03  SYUMAIN-SRYYMD              PIC  X(08).
      *��ɼ���ֶ�ʬ
           03  SYUMAIN-DENPJTIKBN          PIC  X(01).
      *��ɼ������ʬ�ʶ��򡧳��衡�����ౡ�������������������
      *              ���������쥻�ץ��ѥ��ߡ���
           03  SYUMAIN-CREATEKBN           PIC  X(01).
      *��ɼȯ����
           03  SYUMAIN-DENPPRTYMD          PIC  X(08).
      *�������
           03  SYUMAIN-SKY-KIKAN.
      *���ᳫ����
               05  SYUMAIN-SKYSTYMD        PIC  X(08).
      *���Ὢλ��
               05  SYUMAIN-SKYEDYMD        PIC  X(08).
      *�ݸ��ȹ礻�ֹ�
           03  SYUMAIN-HKNCOMBINUM         PIC  X(04).
      *���ݸ����ݸ��ֹ�
           03  SYUMAIN-SYUHKNNUM           PIC  X(03).
      *���ݸ��ݴ�����ô���
           03  SYUMAIN-SYUHKNFTNMONEY      PIC S9(07).
      *���ݸ��ݷ׻���ô��
           03  SYUMAIN-SYUCOMPFTN          PIC S9(07).
      *���ݸ��ݷ׻���ô��ʱ�ñ�̵�Ͽ�ѡ�
           03  SYUMAIN-SYUCOMPFTN-ENTANI   PIC S9(07)V9(01).
      *���ݸ��ݷ׻����ް�����ô��
           03  SYUMAIN-SYUCOMPYKZFTN       PIC S9(07).
      *���ݸ��ݷ׻���ô���
           03  SYUMAIN-SYUCOMPTOTAL        PIC S9(07).
      *���ݸ���������ô��ʬ
           03  SYUMAIN-SYUYKZFTNKBN        PIC  X(01).
      *�������ô����
           03  SYUMAIN-KOH-AREA.
      *���񣱡��ݸ��ֹ�
               05  SYUMAIN-KOH1HKNNUM          PIC  X(03).
      *���񣱡ݴ�����ô���
               05  SYUMAIN-KOH1HKNFTNMONEY     PIC S9(07).
      *���񣱡ݷ׻���ô��
               05  SYUMAIN-KOH1COMPFTN         PIC S9(07).
      *���񣱡ݷ׻���ô��ʱ�ñ�̵�Ͽ�ѡ�
               05  SYUMAIN-KOH1COMPFTN-ENTANI  PIC S9(07)V9(01).
      *���񣱡ݷ׻����ް�����ô��
               05  SYUMAIN-KOH1COMPYKZFTN      PIC S9(07).
      *���񣱡ݷ׻���ô���
               05  SYUMAIN-KOH1COMPTOTAL       PIC S9(07).
      *���񣱡�������ô��ʬ
               05  SYUMAIN-KOH1YKZFTNKBN       PIC  X(01).
      *���񣲡��ݸ��ֹ�
               05  SYUMAIN-KOH2HKNNUM          PIC  X(03).
      *���񣲡ݴ�����ô���
               05  SYUMAIN-KOH2HKNFTNMONEY     PIC S9(07).
      *���񣲡ݷ׻���ô��
               05  SYUMAIN-KOH2COMPFTN         PIC S9(07).
      *���񣲡ݷ׻���ô��ʱ�ñ�̵�Ͽ�ѡ�
               05  SYUMAIN-KOH2COMPFTN-ENTANI  PIC S9(07)V9(01).
      *���񣲡ݷ׻����ް�����ô��
               05  SYUMAIN-KOH2COMPYKZFTN      PIC S9(07).
      *���񣲡ݷ׻���ô���
               05  SYUMAIN-KOH2COMPTOTAL       PIC S9(07).
      *���񣲡�������ô��ʬ
               05  SYUMAIN-KOH2YKZFTNKBN       PIC  X(01).
      *���񣳡��ݸ��ֹ�
               05  SYUMAIN-KOH3HKNNUM          PIC  X(03).
      *���񣳡ݴ�����ô���
               05  SYUMAIN-KOH3HKNFTNMONEY     PIC S9(07).
      *���񣳡ݷ׻���ô��
               05  SYUMAIN-KOH3COMPFTN         PIC S9(07).
      *���񣳡ݷ׻���ô��ʱ�ñ�̵�Ͽ�ѡ�
               05  SYUMAIN-KOH3COMPFTN-ENTANI  PIC S9(07)V9(01).
      *���񣳡ݷ׻����ް�����ô��
               05  SYUMAIN-KOH3COMPYKZFTN      PIC S9(07).
      *���񣳡ݷ׻���ô���
               05  SYUMAIN-KOH3COMPTOTAL       PIC S9(07).
      *���񣳡�������ô��ʬ
               05  SYUMAIN-KOH3YKZFTNKBN       PIC  X(01).
      *���񣴡��ݸ��ֹ�
               05  SYUMAIN-KOH4HKNNUM          PIC  X(03).
      *���񣴡ݴ�����ô���
               05  SYUMAIN-KOH4HKNFTNMONEY     PIC S9(07).
      *���񣴡ݷ׻���ô��
               05  SYUMAIN-KOH4COMPFTN         PIC S9(07).
      *���񣴡ݷ׻���ô��ʱ�ñ�̵�Ͽ�ѡ�
               05  SYUMAIN-KOH4COMPFTN-ENTANI  PIC S9(07)V9(01).
      *���񣴡ݷ׻����ް�����ô��
               05  SYUMAIN-KOH4COMPYKZFTN      PIC S9(07).
      *���񣴡ݷ׻���ô���
               05  SYUMAIN-KOH4COMPTOTAL       PIC S9(07).
      *���񣴡�������ô��ʬ
               05  SYUMAIN-KOH4YKZFTNKBN       PIC  X(01).
      *������ô��������
           03  SYUMAIN-KOHFTN-TABLE  REDEFINES   SYUMAIN-KOH-AREA.
               05  SYUMAIN-KOHFTN-TBL          OCCURS  4.
      *������ݸ��ֹ�
                 07  SYUMAIN-KOH-HKNNUM          PIC  X(03).
      *����ݴ�����ô���
                 07  SYUMAIN-KOH-HKNFTNMONEY     PIC S9(07).
      *����ݷ׻���ô��
                 07  SYUMAIN-KOH-COMPFTN         PIC S9(07).
      *����ݷ׻���ô��ʱ�ñ�̵�Ͽ�ѡ�
                 07  SYUMAIN-KOH-COMPFTN-ENTANI  PIC S9(07)V9(01).
      *����ݷ׻����ް�����ô��
                 07  SYUMAIN-KOH-COMPYKZFTN      PIC S9(07).
      *����ݷ׻���ô���
                 07  SYUMAIN-KOH-COMPTOTAL       PIC S9(07).
      *�����������ô��ʬ
                 07  SYUMAIN-KOH-YKZFTNKBN       PIC  X(01).
      *������ôΨ
           03  SYUMAIN-PTFTNRATE           PIC  9(03).
      *����������ʬ
           03  SYUMAIN-SKYKUKBN            PIC  X(01).
      *�������оݶ��
           03  SYUMAIN-TAX-TAISHOU         PIC S9(07).
      *�����ǡ����ǡ�
           03  SYUMAIN-TAX-MONEY           PIC S9(07).
      *�����ǡʹ�ס�
           03  SYUMAIN-SKYGK               PIC S9(07).
      *��������
           03  SYUMAIN-SKY-NAIYOU.
               05  SYUMAIN-SKY-TBL             OCCURS   17.
      *    �ݸ�����
                   09  SYUMAIN-HKNTEN          PIC S9(07).
      *    ���
                   09  SYUMAIN-MONEY           PIC S9(07).
      *    Ŭ�ѳ����
                   09  SYUMAIN-TGMONEY         PIC S9(07).
      *    Ŭ�ѳ���ۡʾ����Ǥ����
                   09  SYUMAIN-TGMONEY-TAX     PIC S9(07).
      *��������
           03  SYUMAIN-SKY-NAIYOU-R    REDEFINES   SYUMAIN-SKY-NAIYOU.
      *������(01)
              05  SYUMAIN-SSU.
                  09  SYUMAIN-SSU-HKNTEN          PIC S9(07).
                  09  SYUMAIN-SSU-MONEY           PIC S9(07).
                  09  SYUMAIN-SSU-TGMONEY         PIC S9(07).
                  09  SYUMAIN-SSU-TGMONEY-TAX     PIC S9(07).
      *��Ƴ��(02)
              05  SYUMAIN-SDO.
                  09  SYUMAIN-SDO-HKNTEN          PIC S9(07).
                  09  SYUMAIN-SDO-MONEY           PIC S9(07).
                  09  SYUMAIN-SDO-TGMONEY         PIC S9(07).
                  09  SYUMAIN-SDO-TGMONEY-TAX     PIC S9(07).
      *������(03)
              05  SYUMAIN-ZTK.
                  09  SYUMAIN-ZTK-HKNTEN          PIC S9(07).
                  09  SYUMAIN-ZTK-MONEY           PIC S9(07).
                  09  SYUMAIN-ZTK-TGMONEY         PIC S9(07).
                  09  SYUMAIN-ZTK-TGMONEY-TAX     PIC S9(07).
      *������(04)
              05  SYUMAIN-TYK.
                  09  SYUMAIN-TYK-HKNTEN          PIC S9(07).
                  09  SYUMAIN-TYK-MONEY           PIC S9(07).
                  09  SYUMAIN-TYK-TGMONEY         PIC S9(07).
                  09  SYUMAIN-TYK-TGMONEY-TAX     PIC S9(07).
      *������(05)
              05  SYUMAIN-CSY.
                  09  SYUMAIN-CSY-HKNTEN          PIC S9(07).
                  09  SYUMAIN-CSY-MONEY           PIC S9(07).
                  09  SYUMAIN-CSY-TGMONEY         PIC S9(07).
                  09  SYUMAIN-CSY-TGMONEY-TAX     PIC S9(07).
      *������(06)
              05  SYUMAIN-SYC.
                  09  SYUMAIN-SYC-HKNTEN          PIC S9(07).
                  09  SYUMAIN-SYC-MONEY           PIC S9(07).
                  09  SYUMAIN-SYC-TGMONEY         PIC S9(07).
                  09  SYUMAIN-SYC-TGMONEY-TAX     PIC S9(07).
      *�����(07)
              05  SYUMAIN-SJT.
                  09  SYUMAIN-SJT-HKNTEN          PIC S9(07).
                  09  SYUMAIN-SJT-MONEY           PIC S9(07).
                  09  SYUMAIN-SJT-TGMONEY         PIC S9(07).
                  09  SYUMAIN-SJT-TGMONEY-TAX     PIC S9(07).
      *���(08)
              05  SYUMAIN-MSI.
                  09  SYUMAIN-MSI-HKNTEN          PIC S9(07).
                  09  SYUMAIN-MSI-MONEY           PIC S9(07).
                  09  SYUMAIN-MSI-TGMONEY         PIC S9(07).
                  09  SYUMAIN-MSI-TGMONEY-TAX     PIC S9(07).
      *������(09)
              05  SYUMAIN-KNS.
                  09  SYUMAIN-KNS-HKNTEN          PIC S9(07).
                  09  SYUMAIN-KNS-MONEY           PIC S9(07).
                  09  SYUMAIN-KNS-TGMONEY         PIC S9(07).
                  09  SYUMAIN-KNS-TGMONEY-TAX     PIC S9(07).
      *������(10)
              05  SYUMAIN-GZU.
                  09  SYUMAIN-GZU-HKNTEN          PIC S9(07).
                  09  SYUMAIN-GZU-MONEY           PIC S9(07).
                  09  SYUMAIN-GZU-TGMONEY         PIC S9(07).
                  09  SYUMAIN-GZU-TGMONEY-TAX     PIC S9(07).
      *���(11)
              05  SYUMAIN-ETC.
                  09  SYUMAIN-ETC-HKNTEN          PIC S9(07).
                  09  SYUMAIN-ETC-MONEY           PIC S9(07).
                  09  SYUMAIN-ETC-TGMONEY         PIC S9(07).
                  09  SYUMAIN-ETC-TGMONEY-TAX     PIC S9(07).
      *������������ˡ(12)
              05  SYUMAIN-SSN.
                  09  SYUMAIN-SSN-HKNTEN          PIC S9(07).
                  09  SYUMAIN-SSN-MONEY           PIC S9(07).
                  09  SYUMAIN-SSN-TGMONEY         PIC S9(07).
                  09  SYUMAIN-SSN-TGMONEY-TAX     PIC S9(07).
      *������(13)
              05  SYUMAIN-HOU.
                  09  SYUMAIN-HOU-HKNTEN          PIC S9(07).
                  09  SYUMAIN-HOU-MONEY           PIC S9(07).
                  09  SYUMAIN-HOU-TGMONEY         PIC S9(07).
                  09  SYUMAIN-HOU-TGMONEY-TAX     PIC S9(07).
      *��������(14)
              05  SYUMAIN-BYR.
                  09  SYUMAIN-BYR-HKNTEN          PIC S9(07).
                  09  SYUMAIN-BYR-MONEY           PIC S9(07).
                  09  SYUMAIN-BYR-TGMONEY         PIC S9(07).
                  09  SYUMAIN-BYR-TGMONEY-TAX     PIC S9(07).
      *������(15)
              05  SYUMAIN-NYN.
                  09  SYUMAIN-NYN-HKNTEN          PIC S9(07).
                  09  SYUMAIN-NYN-MONEY           PIC S9(07).
                  09  SYUMAIN-NYN-TGMONEY         PIC S9(07).
                  09  SYUMAIN-NYN-TGMONEY-TAX     PIC S9(07).
      *����ô������(16)
              05  SYUMAIN-RYO.
                  09  SYUMAIN-RYO-HKNTEN          PIC S9(07).
                  09  SYUMAIN-RYO-MONEY           PIC S9(07).
                  09  SYUMAIN-RYO-TGMONEY         PIC S9(07).
                  09  SYUMAIN-RYO-TGMONEY-TAX     PIC S9(07).
      *���  (17)
              05  SYUMAIN-TOTAL.
                  09  SYUMAIN-TOTAL-HKNTEN        PIC S9(07).
                  09  SYUMAIN-TOTAL-MONEY         PIC S9(07).
                  09  SYUMAIN-TOTAL-TGMONEY       PIC S9(07).
                  09  SYUMAIN-TOTAL-TGMONEY-TAX   PIC S9(07).
      *�����������Ʒ�
           03  SYUMAIN-SHOHOU-SAI                 PIC S9(07).
      *�ݸ�Ŭ�ѳ���ۡݾ����ǡʺƷǡ�
           03  SYUMAIN-TGMONEY-TAX-SAI         PIC S9(07).
      *���񣱤��飱��
           03  SYUMAIN-JIHI-NAIYOU.
               05  SYUMAIN-JIHI-1              PIC S9(07). 
               05  SYUMAIN-JIHI-1-TAX          PIC S9(07). 
               05  SYUMAIN-JIHI-2              PIC S9(07). 
               05  SYUMAIN-JIHI-2-TAX          PIC S9(07). 
               05  SYUMAIN-JIHI-3              PIC S9(07). 
               05  SYUMAIN-JIHI-3-TAX          PIC S9(07). 
               05  SYUMAIN-JIHI-4              PIC S9(07). 
               05  SYUMAIN-JIHI-4-TAX          PIC S9(07). 
               05  SYUMAIN-JIHI-5              PIC S9(07). 
               05  SYUMAIN-JIHI-5-TAX          PIC S9(07). 
               05  SYUMAIN-JIHI-6              PIC S9(07). 
               05  SYUMAIN-JIHI-6-TAX          PIC S9(07). 
               05  SYUMAIN-JIHI-7              PIC S9(07). 
               05  SYUMAIN-JIHI-7-TAX          PIC S9(07). 
               05  SYUMAIN-JIHI-8              PIC S9(07). 
               05  SYUMAIN-JIHI-8-TAX          PIC S9(07). 
               05  SYUMAIN-JIHI-9              PIC S9(07). 
               05  SYUMAIN-JIHI-9-TAX          PIC S9(07). 
               05  SYUMAIN-JIHI-10             PIC S9(07). 
               05  SYUMAIN-JIHI-10-TAX         PIC S9(07). 
           03  SYUMAIN-JIHI-NAIYOU-R   REDEFINES  SYUMAIN-JIHI-NAIYOU.
               05  SYUMAIN-JIHI-TBL            OCCURS   10.
                 07  SYUMAIN-JIHI-TAXNASI        PIC S9(07).
                 07  SYUMAIN-JIHI-TAXARI         PIC S9(07).
      *���񾮷סʾ����Ǥʤ���
           03  SYUMAIN-JIHI-TOTAL          PIC S9(07).
      *���񾮷סʾ����Ǥ����
           03  SYUMAIN-JIHI-TOTAL-TAX      PIC S9(07).
      *�����۾�����
           03  SYUMAIN-JIHI-TAX            PIC S9(07).
      *Ϸ�Ͱ�����ô��
           03  SYUMAIN-RJN-FTN             PIC S9(07).
      *���������ô��
           03  SYUMAIN-KOH-FTN             PIC S9(07).
      *���������ô��ʱ�ñ�̡�
           03  SYUMAIN-KOH-FTN-ENTANI      PIC S9(07)V9(01).
      *���ް�����ô��
           03  SYUMAIN-YKZ-FTN             PIC S9(07).
      *�쥻�ץȽ��������ް�����ô��
           03  SYUMAIN-RESE-YKZ-FTN        PIC S9(07).
      *�����о����޶�ʬ
           03  SYUMAIN-KOHTAIYKZKBN        PIC  X(01).
      *Ĵ����
           03  SYUMAIN-CHOSEI              PIC S9(07).
           03  SYUMAIN-CHOSEI-NAIYOU.
      *Ĵ����������
               05  SYUMAIN-CHOSEI1         PIC S9(07).
      *Ĵ����������
               05  SYUMAIN-CHOSEI2         PIC S9(07).
           03  SYUMAIN-CHOSEI-NAIYOU-R REDEFINES  SYUMAIN-CHOSEI-NAIYOU.
               05  SYUMAIN-RCHOSEI         PIC S9(07)
                                           OCCURS  2.
      *�ޤȤ�������ɼ�ֹ�
           03  SYUMAIN-GRP-DENPNUM         PIC 9(07).
      *�ޤȤ�����Ϣ��
           03  SYUMAIN-GRP-RENNUM          PIC 9(02).
      *�ޤȤ����Ϻ���
           03  SYUMAIN-GRP-SGKMONEY        PIC S9(07).
      *�ޤȤ�ȯ����ˡ�ե饰
           03  SYUMAIN-GRP-HAKHOUFLG       PIC  9(01).
      *�ݾڶ��
           03  SYUMAIN-SECMONEY            PIC S9(07).
      *�ݸ�Ŭ�Ѷ��
           03  SYUMAIN-HKNTEKMONEY         PIC S9(07).
      *�±����Ȼ�ͳ��ʬ
           03  SYUMAIN-DISCOUNT-KBN        PIC  X(02).
      *�±����Ȼ�ͳ
           03  SYUMAIN-DISCOUNT-BODY       PIC  X(160).
      *���Ψ��ʬ
           03  SYUMAIN-DISCOUNT-RATEKBN    PIC  X(02).
      *��Ψ��۶�ʬ
           03  SYUMAIN-DISCOUNT-TEIRITU    PIC  X(01).
      *���Ψ
           03  SYUMAIN-DISCOUNT-RATE       PIC S9(07).
      *���ȶ��
           03  SYUMAIN-DISCOUNT-MONEY      PIC S9(07).
      *ϫ���ݸ��ݽ����
           03  SYUMAIN-RSISHOSHIN-MONEY    PIC S9(07).
      *ϫ���ݸ��ݺƿ���
           03  SYUMAIN-RSISAISHIN-MONEY    PIC S9(07).
      *ϫ���ݸ��ݻ�Ƴ��
           03  SYUMAIN-RSISDO-MONEY        PIC S9(07).
      *ϫ���ݸ��ݤ���¾
           03  SYUMAIN-RSIETC-MONEY        PIC S9(07).
      *ϫ���ݸ��ݾ����ǡʺƷǡ�
           03  SYUMAIN-RSI-TAX-SAI         PIC S9(07).
      *ϫ���ݸ��ݴ�������ʬ�߷��ƹ�׳�
           03  SYUMAIN-RSI-TOTAL           PIC S9(07).
      *ϫ�Ҽ���������ۡ�����ɽ�����ѡ�
           03  SYUMAIN-RSIJIBAI-SKYMONEY   PIC S9(07).
      *�����ۡݾ����ǡʺƷǡ�
           03  SYUMAIN-SKYMONEY-TAX-SAI    PIC S9(07).
      *������
           03  SYUMAIN-SKYMONEY            PIC S9(07).
      *�����׳�
           03  SYUMAIN-NYUKIN-TOTAL        PIC S9(07).
      *������
           03  SYUMAIN-NYUKIN-KAISU        PIC S9(07).
      *̤����ͳ
           03  SYUMAIN-MISYURIYU           PIC  X(02).
      *�ɥ�����
           03  SYUMAIN-DRCD-G.
               05  SYUMAIN-DRCD-SIK            PIC  X(01).
               05  SYUMAIN-DRCD                PIC  X(04).
      *������ǲ��
           03  SYUMAIN-NYNSHOKAISU         PIC S9(02).
      *����ȯ���ֹ�
           03  SYUMAIN-YKZKENNUM           PIC  9(05).
      *������ô����ô���̴����ѡ�
           03  SYUMAIN-SKJ-PTFTN           OCCURS 3.
               05  SYUMAIN-SKJ-FTNGAKU     PIC S9(05).
               05  SYUMAIN-SKJ-FTNDAY      PIC S9(02).
      *�Ʒ׻���ʬ
           03  SYUMAIN-SAIKEISANKBN        PIC  X(01).
      *����������ʬ
           03  SYUMAIN-INGAISHOHOKBN       PIC  X(01).
      *Ʊ��������ɼ�ֹ�
           03  SYUMAIN-DOUJI-DENPNUM       PIC  9(07).
      *��³��ʬ
           03  SYUMAIN-CONTKBN             PIC  X(01).
      *ʣ���ʤޤȤἱ����ɼ�ֹ�
           03  SYUMAIN-FUKU-DENPNUM        PIC  9(07).
      *ʣ���ʤޤȤ��ʬ�ʣ�����³�����������
           03  SYUMAIN-FUKU-KBN            PIC  X(01).
      *�����Ƕ�ʬ
           03  SYUMAIN-ZAITAKU             PIC  X(01).
      *���Ŷ�ʬ�̵��ճ�����
           03  SYUMAIN-KYUFUGAI.
               05  SYUMAIN-KYUFUGAI-TBL          OCCURS   6.
      *    �����оݳ�����
                   09  SYUMAIN-KYUFUGAI-TEN      PIC S9(07).
      *���Ŷ�ʬ�̵��ճ����������
           03  SYUMAIN-KYUFUGAI-R      REDEFINES   SYUMAIN-KYUFUGAI.
      *�����оݳ������ʽ�ǡ�
              05  SYUMAIN-KYUFUGAI-SHOSHIN-TEN    PIC S9(07).
      *�����оݳ������ʻ�Ƴ��
              05  SYUMAIN-KYUFUGAI-SIDOU-TEN      PIC S9(07).
      *�����оݳ������ʱ��ǡ�
              05  SYUMAIN-KYUFUGAI-OSIN1-TEN      PIC S9(07).
      *�����оݳ������ʱ��ǡ���֡�
              05  SYUMAIN-KYUFUGAI-OSIN2-TEN      PIC S9(07).
      *�����оݳ������ʱ��ǡ����롿�۵ޡ�
              05  SYUMAIN-KYUFUGAI-OSIN3-TEN      PIC S9(07).
      *�����оݳ������ʹ�ס�
              05  SYUMAIN-KYUFUGAI-GOKEI-TEN      PIC S9(07).
      *���̼�Ǽ�ե饰
           03  SYUMAIN-DAYINFFLG           PIC  X(01).
      *ʬ�ڴ����ե饰
           03  SYUMAIN-BUNBENINFFLG        PIC  X(01).
      *����ѹ���ʬ
           03  SYUMAIN-ACCT-UPDKBN         PIC  X(01).
      *��§�����ݴ�����ô���
           03  SYUMAIN-JYO-HKNFTNMONEY     PIC S9(07).
      *��§�����ݷ׻���ô��
           03  SYUMAIN-JYO-COMPFTN         PIC S9(07).
      *��§�����ݷ׻���ô��ʱ�ñ�̵�Ͽ�ѡ�
           03  SYUMAIN-JYO-COMPFTN-ENTANI  PIC S9(07)V9(01).
      *���������ʬ�ʣ��������ۡ������׳ۡ����������ۡ������׳ۡ�
           03  SYUMAIN-SKYNYUKINKBN        PIC  X(01).
      *�ݸ�Ŭ�Ѷ�ʬ
           03  SYUMAIN-HKNTEKKBN           PIC  X(01).
      *ü���ɣ�
           03  SYUMAIN-TERMID              PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYUMAIN-OPID                PIC  X(16).
      *����ǯ����
           03  SYUMAIN-CREYMD              PIC  X(08).
      *����ǯ����
           03  SYUMAIN-UPYMD               PIC  X(08).
      *��������
           03  SYUMAIN-UPHMS               PIC  X(06).
      *
      *����ǯ�����ʸ����ѡ�
           03  SYUMAIN-UPSRYYMD            PIC  X(08).
      *
      *���������ֹ�
           03  SYUMAIN-SELNYUIN-RRKNUM     PIC  9(03).
      *����ǯ�����ʸ����ѡ�
           03  SYUMAIN-SELKOHID            PIC  9(10).
      *������ʬ�ʸ����ѡ�
           03  SYUMAIN-SELNYUGAIKBN        PIC  X(01).