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
      *    ��Ǽ�ޥ���         COPY (CPSYUNOU.INC)       *
      *  01.00.01    NACL-��Ʀ��   04/07/04  �ޤȤ����Ϻ����ɲ�
      *                                      �ޤȤ�������ɼ�ֹ��ɲ�
      *                                      �ޤȤ�����Ϣ���ɲ�
      *  01.00.02    NACL-��Ʀ��   05/07/13  ������ܤ��ɲ�
      *                                      ���ȶ�ۤ��ɲ�
      *                                      ��§�������ܤ��ɲ�
      *  01.00.03    NACL-��Ʀ��   06/07/06  ���̤ι����ɲ�
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  SYUNOU-REC.
           03  SYU-KEY.
      *���ŵ��أɣ�
               05  SYU-HOSPNUM          PIC 9(2).
      *������ʬ
               05  SYU-NYUGAIKBN       PIC  X(01).
      *���ԣɣ�
               05  SYU-PTID            PIC  9(10).
      *��ɼ�ֹ�
               05  SYU-DENPNUM         PIC  9(07).
      *��ɼ���ֺǽ��ֹ�
           03  SYU-DENPLASTNUM         PIC  9(02).
      *���Ų�
           03  SYU-SRYKA               PIC  X(02).
      *������
           03  SYU-SRYYMD              PIC  X(08).
      *��ɼ���ֶ�ʬ
           03  SYU-DENPJTIKBN          PIC  X(01).
      *��ɼ������ʬ�ʶ��򡧳��衡�����ౡ�������������������
      *              ���������쥻�ץ��ѥ��ߡ���
           03  SYU-CREATEKBN           PIC  X(01).
      *��ɼȯ����
           03  SYU-DENPPRTYMD          PIC  X(08).
      *�������
           03  SYU-SKY-KIKAN.
      *���ᳫ����
               05  SYU-SKYSTYMD        PIC  X(08).
      *���Ὢλ��
               05  SYU-SKYEDYMD        PIC  X(08).
      *�ݸ��ȹ礻�ֹ�
           03  SYU-HKNCOMBINUM         PIC  X(04).
      *���ݸ����ݸ��ֹ�
           03  SYU-SYUHKNNUM           PIC  X(03).
      *���ݸ��ݴ�����ô���
           03  SYU-SYUHKNFTNMONEY      PIC S9(07).
      *���ݸ��ݷ׻���ô��
           03  SYU-SYUCOMPFTN          PIC S9(07).
      *���ݸ��ݷ׻���ô��ʱ�ñ�̵�Ͽ�ѡ�
           03  SYU-SYUCOMPFTN-ENTANI   PIC S9(07)V9(01).
      *���ݸ��ݷ׻����ް�����ô��
           03  SYU-SYUCOMPYKZFTN       PIC S9(07).
      *���ݸ��ݷ׻���ô���
           03  SYU-SYUCOMPTOTAL        PIC S9(07).
      *���ݸ���������ô��ʬ
           03  SYU-SYUYKZFTNKBN        PIC  X(01).
      *�������ô����
           03  SYU-KOH-AREA.
      *���񣱡��ݸ��ֹ�
               05  SYU-KOH1HKNNUM          PIC  X(03).
      *���񣱡ݴ�����ô���
               05  SYU-KOH1HKNFTNMONEY     PIC S9(07).
      *���񣱡ݷ׻���ô��
               05  SYU-KOH1COMPFTN         PIC S9(07).
      *���񣱡ݷ׻���ô��ʱ�ñ�̵�Ͽ�ѡ�
               05  SYU-KOH1COMPFTN-ENTANI  PIC S9(07)V9(01).
      *���񣱡ݷ׻����ް�����ô��
               05  SYU-KOH1COMPYKZFTN      PIC S9(07).
      *���񣱡ݷ׻���ô���
               05  SYU-KOH1COMPTOTAL       PIC S9(07).
      *���񣱡�������ô��ʬ
               05  SYU-KOH1YKZFTNKBN       PIC  X(01).
      *���񣲡��ݸ��ֹ�
               05  SYU-KOH2HKNNUM          PIC  X(03).
      *���񣲡ݴ�����ô���
               05  SYU-KOH2HKNFTNMONEY     PIC S9(07).
      *���񣲡ݷ׻���ô��
               05  SYU-KOH2COMPFTN         PIC S9(07).
      *���񣲡ݷ׻���ô��ʱ�ñ�̵�Ͽ�ѡ�
               05  SYU-KOH2COMPFTN-ENTANI  PIC S9(07)V9(01).
      *���񣲡ݷ׻����ް�����ô��
               05  SYU-KOH2COMPYKZFTN      PIC S9(07).
      *���񣲡ݷ׻���ô���
               05  SYU-KOH2COMPTOTAL       PIC S9(07).
      *���񣲡�������ô��ʬ
               05  SYU-KOH2YKZFTNKBN       PIC  X(01).
      *���񣳡��ݸ��ֹ�
               05  SYU-KOH3HKNNUM          PIC  X(03).
      *���񣳡ݴ�����ô���
               05  SYU-KOH3HKNFTNMONEY     PIC S9(07).
      *���񣳡ݷ׻���ô��
               05  SYU-KOH3COMPFTN         PIC S9(07).
      *���񣳡ݷ׻���ô��ʱ�ñ�̵�Ͽ�ѡ�
               05  SYU-KOH3COMPFTN-ENTANI  PIC S9(07)V9(01).
      *���񣳡ݷ׻����ް�����ô��
               05  SYU-KOH3COMPYKZFTN      PIC S9(07).
      *���񣳡ݷ׻���ô���
               05  SYU-KOH3COMPTOTAL       PIC S9(07).
      *���񣳡�������ô��ʬ
               05  SYU-KOH3YKZFTNKBN       PIC  X(01).
      *���񣴡��ݸ��ֹ�
               05  SYU-KOH4HKNNUM          PIC  X(03).
      *���񣴡ݴ�����ô���
               05  SYU-KOH4HKNFTNMONEY     PIC S9(07).
      *���񣴡ݷ׻���ô��
               05  SYU-KOH4COMPFTN         PIC S9(07).
      *���񣴡ݷ׻���ô��ʱ�ñ�̵�Ͽ�ѡ�
               05  SYU-KOH4COMPFTN-ENTANI  PIC S9(07)V9(01).
      *���񣴡ݷ׻����ް�����ô��
               05  SYU-KOH4COMPYKZFTN      PIC S9(07).
      *���񣴡ݷ׻���ô���
               05  SYU-KOH4COMPTOTAL       PIC S9(07).
      *���񣴡�������ô��ʬ
               05  SYU-KOH4YKZFTNKBN       PIC  X(01).
      *������ô��������
           03  SYU-KOHFTN-TABLE  REDEFINES   SYU-KOH-AREA.
               05  SYU-KOHFTN-TBL          OCCURS  4.
      *������ݸ��ֹ�
                 07  SYU-KOH-HKNNUM          PIC  X(03).
      *����ݴ�����ô���
                 07  SYU-KOH-HKNFTNMONEY     PIC S9(07).
      *����ݷ׻���ô��
                 07  SYU-KOH-COMPFTN         PIC S9(07).
      *����ݷ׻���ô��ʱ�ñ�̵�Ͽ�ѡ�
                 07  SYU-KOH-COMPFTN-ENTANI  PIC S9(07)V9(01).
      *����ݷ׻����ް�����ô��
                 07  SYU-KOH-COMPYKZFTN      PIC S9(07).
      *����ݷ׻���ô���
                 07  SYU-KOH-COMPTOTAL       PIC S9(07).
      *�����������ô��ʬ
                 07  SYU-KOH-YKZFTNKBN       PIC  X(01).
      *������ôΨ
           03  SYU-PTFTNRATE           PIC  9(03).
      *�����Σ�
           03  SYU-ROOMNUM             PIC  X(08).
      *���拾����
           03  SYU-BYOTONUM            PIC  X(02).
      *����������ʬ
           03  SYU-SKYKUKBN            PIC  X(01).
      *�������оݶ��
           03  SYU-TAX-TAISHOU         PIC S9(07).
      *�����ǡ����ǡ�
           03  SYU-TAX-MONEY           PIC S9(07).
      *�����ǡʹ�ס�
           03  SYU-SKYGK               PIC S9(07).
      *��������
           03  SYU-SKY-NAIYOU.
               05  SYU-SKY-TBL             OCCURS   17.
      *    �ݸ�����
                   09  SYU-HKNTEN          PIC S9(07).
      *    ���
                   09  SYU-MONEY           PIC S9(07).
      *    Ŭ�ѳ����
                   09  SYU-TGMONEY         PIC S9(07).
      *    Ŭ�ѳ���ۡʾ����Ǥ����
                   09  SYU-TGMONEY-TAX     PIC S9(07).
      *��������
           03  SYU-SKY-NAIYOU-R        REDEFINES   SYU-SKY-NAIYOU.
      *������(01)
              05  SYU-SSU.
                  09  SYU-SSU-HKNTEN          PIC S9(07).
                  09  SYU-SSU-MONEY           PIC S9(07).
                  09  SYU-SSU-TGMONEY         PIC S9(07).
                  09  SYU-SSU-TGMONEY-TAX     PIC S9(07).
      *��Ƴ��(02)
              05  SYU-SDO.
                  09  SYU-SDO-HKNTEN          PIC S9(07).
                  09  SYU-SDO-MONEY           PIC S9(07).
                  09  SYU-SDO-TGMONEY         PIC S9(07).
                  09  SYU-SDO-TGMONEY-TAX     PIC S9(07).
      *������(03)
              05  SYU-ZTK.
                  09  SYU-ZTK-HKNTEN          PIC S9(07).
                  09  SYU-ZTK-MONEY           PIC S9(07).
                  09  SYU-ZTK-TGMONEY         PIC S9(07).
                  09  SYU-ZTK-TGMONEY-TAX     PIC S9(07).
      *������(04)
              05  SYU-TYK.
                  09  SYU-TYK-HKNTEN          PIC S9(07).
                  09  SYU-TYK-MONEY           PIC S9(07).
                  09  SYU-TYK-TGMONEY         PIC S9(07).
                  09  SYU-TYK-TGMONEY-TAX     PIC S9(07).
      *������(05)
              05  SYU-CSY.
                  09  SYU-CSY-HKNTEN          PIC S9(07).
                  09  SYU-CSY-MONEY           PIC S9(07).
                  09  SYU-CSY-TGMONEY         PIC S9(07).
                  09  SYU-CSY-TGMONEY-TAX     PIC S9(07).
      *������(06)
              05  SYU-SYC.
                  09  SYU-SYC-HKNTEN          PIC S9(07).
                  09  SYU-SYC-MONEY           PIC S9(07).
                  09  SYU-SYC-TGMONEY         PIC S9(07).
                  09  SYU-SYC-TGMONEY-TAX     PIC S9(07).
      *�����(07)
              05  SYU-SJT.
                  09  SYU-SJT-HKNTEN          PIC S9(07).
                  09  SYU-SJT-MONEY           PIC S9(07).
                  09  SYU-SJT-TGMONEY         PIC S9(07).
                  09  SYU-SJT-TGMONEY-TAX     PIC S9(07).
      *���(08)
              05  SYU-MSI.
                  09  SYU-MSI-HKNTEN          PIC S9(07).
                  09  SYU-MSI-MONEY           PIC S9(07).
                  09  SYU-MSI-TGMONEY         PIC S9(07).
                  09  SYU-MSI-TGMONEY-TAX     PIC S9(07).
      *������(09)
              05  SYU-KNS.
                  09  SYU-KNS-HKNTEN          PIC S9(07).
                  09  SYU-KNS-MONEY           PIC S9(07).
                  09  SYU-KNS-TGMONEY         PIC S9(07).
                  09  SYU-KNS-TGMONEY-TAX     PIC S9(07).
      *������(10)
              05  SYU-GZU.
                  09  SYU-GZU-HKNTEN          PIC S9(07).
                  09  SYU-GZU-MONEY           PIC S9(07).
                  09  SYU-GZU-TGMONEY         PIC S9(07).
                  09  SYU-GZU-TGMONEY-TAX     PIC S9(07).
      *���(11)
              05  SYU-ETC.
                  09  SYU-ETC-HKNTEN          PIC S9(07).
                  09  SYU-ETC-MONEY           PIC S9(07).
                  09  SYU-ETC-TGMONEY         PIC S9(07).
                  09  SYU-ETC-TGMONEY-TAX     PIC S9(07).
      *������������ˡ(12)
              05  SYU-SSN.
                  09  SYU-SSN-HKNTEN          PIC S9(07).
                  09  SYU-SSN-MONEY           PIC S9(07).
                  09  SYU-SSN-TGMONEY         PIC S9(07).
                  09  SYU-SSN-TGMONEY-TAX     PIC S9(07).
      *������(13)
              05  SYU-HOU.
                  09  SYU-HOU-HKNTEN          PIC S9(07).
                  09  SYU-HOU-MONEY           PIC S9(07).
                  09  SYU-HOU-TGMONEY         PIC S9(07).
                  09  SYU-HOU-TGMONEY-TAX     PIC S9(07).
      *��������(14)
              05  SYU-BYR.
                  09  SYU-BYR-HKNTEN          PIC S9(07).
                  09  SYU-BYR-MONEY           PIC S9(07).
                  09  SYU-BYR-TGMONEY         PIC S9(07).
                  09  SYU-BYR-TGMONEY-TAX     PIC S9(07).
      *������(15)
              05  SYU-NYN.
                  09  SYU-NYN-HKNTEN          PIC S9(07).
                  09  SYU-NYN-MONEY           PIC S9(07).
                  09  SYU-NYN-TGMONEY         PIC S9(07).
                  09  SYU-NYN-TGMONEY-TAX     PIC S9(07).
      *����ô������(16)
              05  SYU-RYO.
                  09  SYU-RYO-HKNTEN          PIC S9(07).
                  09  SYU-RYO-MONEY           PIC S9(07).
                  09  SYU-RYO-TGMONEY         PIC S9(07).
                  09  SYU-RYO-TGMONEY-TAX     PIC S9(07).
      *���  (17)
              05  SYU-TOTAL.
                  09  SYU-TOTAL-HKNTEN        PIC S9(07).
                  09  SYU-TOTAL-MONEY         PIC S9(07).
                  09  SYU-TOTAL-TGMONEY       PIC S9(07).
                  09  SYU-TOTAL-TGMONEY-TAX   PIC S9(07).
      *�����������Ʒ�
           03  SYU-SHOHOU-SAI                 PIC S9(07).
      *�ݸ�Ŭ�ѳ���ۡݾ����ǡʺƷǡ�
           03  SYU-TGMONEY-TAX-SAI         PIC S9(07).
      *���񣱤��飱��
           03  SYU-JIHI-NAIYOU.
               05  SYU-JIHI-1              PIC S9(07). 
               05  SYU-JIHI-1-TAX          PIC S9(07). 
               05  SYU-JIHI-2              PIC S9(07). 
               05  SYU-JIHI-2-TAX          PIC S9(07). 
               05  SYU-JIHI-3              PIC S9(07). 
               05  SYU-JIHI-3-TAX          PIC S9(07). 
               05  SYU-JIHI-4              PIC S9(07). 
               05  SYU-JIHI-4-TAX          PIC S9(07). 
               05  SYU-JIHI-5              PIC S9(07). 
               05  SYU-JIHI-5-TAX          PIC S9(07). 
               05  SYU-JIHI-6              PIC S9(07). 
               05  SYU-JIHI-6-TAX          PIC S9(07). 
               05  SYU-JIHI-7              PIC S9(07). 
               05  SYU-JIHI-7-TAX          PIC S9(07). 
               05  SYU-JIHI-8              PIC S9(07). 
               05  SYU-JIHI-8-TAX          PIC S9(07). 
               05  SYU-JIHI-9              PIC S9(07). 
               05  SYU-JIHI-9-TAX          PIC S9(07). 
               05  SYU-JIHI-10             PIC S9(07). 
               05  SYU-JIHI-10-TAX         PIC S9(07). 
           03  SYU-JIHI-NAIYOU-R           REDEFINES  SYU-JIHI-NAIYOU.
               05  SYU-JIHI-TBL            OCCURS   10.
                 07  SYU-JIHI-TAXNASI        PIC S9(07).
                 07  SYU-JIHI-TAXARI         PIC S9(07).
      *���񾮷סʾ����Ǥʤ���
           03  SYU-JIHI-TOTAL          PIC S9(07).
      *���񾮷סʾ����Ǥ����
           03  SYU-JIHI-TOTAL-TAX      PIC S9(07).
      *�����۾�����
           03  SYU-JIHI-TAX            PIC S9(07).
      *Ϸ�Ͱ�����ô��
           03  SYU-RJN-FTN             PIC S9(07).
      *���������ô��
           03  SYU-KOH-FTN             PIC S9(07).
      *���������ô��ʱ�ñ�̡�
           03  SYU-KOH-FTN-ENTANI      PIC S9(07)V9(01).
      *���ް�����ô��
           03  SYU-YKZ-FTN             PIC S9(07).
      *�쥻�ץȽ��������ް�����ô��
           03  SYU-RESE-YKZ-FTN        PIC S9(07).
      *�����о����޶�ʬ
           03  SYU-KOHTAIYKZKBN        PIC  X(01).
      *Ĵ����
           03  SYU-CHOSEI              PIC S9(07).
           03  SYU-CHOSEI-NAIYOU.
      *Ĵ����������
               05  SYU-CHOSEI1         PIC S9(07).
      *Ĵ����������
               05  SYU-CHOSEI2         PIC S9(07).
           03  SYU-CHOSEI-NAIYOU-R REDEFINES  SYU-CHOSEI-NAIYOU.
               05  SYU-RCHOSEI         PIC S9(07)
                                       OCCURS  2.
      *�ޤȤ�������ɼ�ֹ�
           03  SYU-GRP-DENPNUM         PIC 9(07).
      *�ޤȤ�����Ϣ��
           03  SYU-GRP-RENNUM          PIC 9(02).
      *�ޤȤ����Ϻ���
           03  SYU-GRP-SGKMONEY        PIC S9(07).
      *�ޤȤ�ȯ����ˡ�ե饰
           03  SYU-GRP-HAKHOUFLG       PIC  9(01).
      *�ݾڶ��
           03  SYU-SECMONEY            PIC S9(07).
      *�ݸ�Ŭ�Ѷ��
           03  SYU-HKNTEKMONEY         PIC S9(07).
      *�±����Ȼ�ͳ��ʬ
           03  SYU-DISCOUNT-KBN        PIC  X(02).
      *�±����Ȼ�ͳ
           03  SYU-DISCOUNT-BODY       PIC  X(160).
      *���Ψ��ʬ
           03  SYU-DISCOUNT-RATEKBN    PIC  X(02).
      *��Ψ��۶�ʬ
           03  SYU-DISCOUNT-TEIRITU    PIC  X(01).
      *���Ψ
           03  SYU-DISCOUNT-RATE       PIC S9(07).
      *���ȶ��
           03  SYU-DISCOUNT-MONEY      PIC S9(07).
      *ϫ���ݸ��ݽ����
           03  SYU-RSISHOSHIN-MONEY    PIC S9(07).
      *ϫ���ݸ��ݺƿ���
           03  SYU-RSISAISHIN-MONEY    PIC S9(07).
      *ϫ���ݸ��ݻ�Ƴ��
           03  SYU-RSISDO-MONEY        PIC S9(07).
      *ϫ���ݸ��ݤ���¾
           03  SYU-RSIETC-MONEY        PIC S9(07).
      *ϫ���ݸ��ݾ����ǡʺƷǡ�
           03  SYU-RSI-TAX-SAI         PIC S9(07).
      *ϫ���ݸ��ݴ�������ʬ�߷��ƹ�׳�
           03  SYU-RSI-TOTAL           PIC S9(07).
      *ϫ�Ҽ���������ۡ�����ɽ�����ѡ�
           03  SYU-RSIJIBAI-SKYMONEY   PIC S9(07).
      *�����ۡݾ����ǡʺƷǡ�
           03  SYU-SKYMONEY-TAX-SAI    PIC S9(07).
      *������
           03  SYU-SKYMONEY            PIC S9(07).
      *�����׳�
           03  SYU-NYUKIN-TOTAL        PIC S9(07).
      *������
           03  SYU-NYUKIN-KAISU        PIC S9(07).
      *̤����ͳ
           03  SYU-MISYURIYU           PIC  X(02).
      *�ɥ�����
           03  SYU-DRCD-G.
               05  SYU-DRCD-SIK            PIC  X(01).
               05  SYU-DRCD                PIC  X(04).
      *������ǲ��
           03  SYU-NYNSHOKAISU         PIC S9(02).
      *����ȯ���ֹ�
           03  SYU-YKZKENNUM           PIC  9(05).
      *��������
           03  SYU-RMSAGAKU            PIC S9(07).
      *�������ۡݾ����ǡʺƷǡ�
           03  SYU-RMSAGAKU-TAX-SAI    PIC S9(07).
      *���������鿩������
           03  SYU-SHOKUJI-NAIYOU.
               05  SYU-SHOKUJI1-NISSU  PIC S9(02).
               05  SYU-SHOKUJI1        PIC S9(07).
               05  SYU-SHOKUJI2-NISSU  PIC S9(02).
               05  SYU-SHOKUJI2        PIC S9(07).
               05  SYU-SHOKUJI3-NISSU  PIC S9(02).
               05  SYU-SHOKUJI3        PIC S9(07).
               05  SYU-SHOKUJI4-NISSU  PIC S9(02).
               05  SYU-SHOKUJI4        PIC S9(07).
               05  SYU-SHOKUJI5-NISSU  PIC S9(02).
               05  SYU-SHOKUJI5        PIC S9(07).
               05  SYU-SHOKUJI6-NISSU  PIC S9(02).
               05  SYU-SHOKUJI6        PIC S9(07).
               05  SYU-SHOKUJI7-NISSU  PIC S9(02).
               05  SYU-SHOKUJI7        PIC S9(07).
               05  SYU-SHOKUJI8-NISSU  PIC S9(02).
               05  SYU-SHOKUJI8        PIC S9(07).
               05  SYU-SHOKUJI9-NISSU  PIC S9(02).
               05  SYU-SHOKUJI9        PIC S9(07).
               05  SYU-SHOKUJI10-NISSU PIC S9(02).
               05  SYU-SHOKUJI10       PIC S9(07).
               05  SYU-SHOKUJI11-NISSU PIC S9(02).
               05  SYU-SHOKUJI11       PIC S9(07).
               05  SYU-SHOKUJI12-NISSU PIC S9(02).
               05  SYU-SHOKUJI12       PIC S9(07).
               05  SYU-SHOKUJI13-NISSU PIC S9(02).
               05  SYU-SHOKUJI13       PIC S9(07).
               05  SYU-SHOKUJI14-NISSU PIC S9(02).
               05  SYU-SHOKUJI14       PIC S9(07).
           03  SYU-SYOKUJI-NAIYOUR     REDEFINES   SYU-SHOKUJI-NAIYOU.
               05  SYU-SYOKUJI-TBL     OCCURS   14.
                 07  SYU-SHOKUJI-NISSU   PIC S9(02).
                 07  SYU-SHOKUJI         PIC S9(07).
      *�������������ʿ�������������������ɸ����ô�ۡ�
           03  SYU-SKJ-AREA.
               05  SYU-SYUSKJ-NISSU      PIC S9(02).
               05  SYU-SYUSKJ-RYOYOHI    PIC S9(07).
               05  SYU-SYUSKJ-FTN        PIC S9(07).
               05  SYU-SYUSKJ-FTN-RECE   PIC S9(07).
               05  SYU-SYUSKJ-FTNKBN     PIC  X(01).
           03  SYU-SKJ-AREA2.
               05  SYU-KOH1SKJ-NISSU     PIC S9(02).
               05  SYU-KOH1SKJ-RYOYOHI   PIC S9(07).
               05  SYU-KOH1SKJ-FTN       PIC S9(07).
               05  SYU-KOH1SKJ-FTN-RECE  PIC S9(07).
               05  SYU-KOH1SKJ-FTNKBN    PIC  X(01).
               05  SYU-KOH2SKJ-NISSU     PIC S9(02).
               05  SYU-KOH2SKJ-RYOYOHI   PIC S9(07).
               05  SYU-KOH2SKJ-FTN       PIC S9(07).
               05  SYU-KOH2SKJ-FTN-RECE  PIC S9(07).
               05  SYU-KOH2SKJ-FTNKBN    PIC  X(01).
               05  SYU-KOH3SKJ-NISSU     PIC S9(02).
               05  SYU-KOH3SKJ-RYOYOHI   PIC S9(07).
               05  SYU-KOH3SKJ-FTN       PIC S9(07).
               05  SYU-KOH3SKJ-FTN-RECE  PIC S9(07).
               05  SYU-KOH3SKJ-FTNKBN    PIC  X(01).
               05  SYU-KOH4SKJ-NISSU     PIC S9(02).
               05  SYU-KOH4SKJ-RYOYOHI   PIC S9(07).
               05  SYU-KOH4SKJ-FTN       PIC S9(07).
               05  SYU-KOH4SKJ-FTN-RECE  PIC S9(07).
               05  SYU-KOH4SKJ-FTNKBN    PIC  X(01).
           03  SYU-SKJ-AREA2R          REDEFINES   SYU-SKJ-AREA2.
               05  SYU-SKJ-TBL         OCCURS   4.
                 07  SYU-SKJ-NISSU       PIC S9(02).
                 07  SYU-SKJ-RYOYOHI     PIC S9(07).
                 07  SYU-SKJ-FTN         PIC S9(07).
                 07  SYU-SKJ-FTN-RECE    PIC S9(07).
                 07  SYU-SKJ-FTNKBN      PIC  X(01).
      *������������ݸ���
           03  SYU-RYOYOHI-SKJ         PIC S9(07).
      *������ô�ۡ��ݸ���������ô��
           03  SYU-SKYMONEY-SKJ        PIC S9(07).
      *������ô�ۡ��ݸ���������ô�����ǡ�
           03  SYU-SKYMONEY-SKJ-TAX    PIC S9(07).
      *������ô�ۡ��ݸ���������ô��ס�
           03  SYU-SKYMONEY-SKJ-KEI    PIC S9(07).
      *����������ʼ����
           03  SYU-RYOYOHI-SKJ-JIHI    PIC S9(07).
      *������ô�ۡʼ��񡧼�����ô��
           03  SYU-SKYMONEY-SKJ-JIHI   PIC S9(07).
      *������ô�ۡʼ��񡧼�����ô�����ǡ�
           03  SYU-SKYMONEY-SKJ-JIHI-TAX
                                       PIC S9(07).
      *������ô�ۡʼ��񡧼�����ô��ס�
           03  SYU-SKYMONEY-SKJ-JIHI-KEI
                                       PIC S9(07).
      *������������������ݸ���
           03  SYU-RYOYOHI-LIFE        PIC S9(07).
      *������������������ô�ۡ��ݸ���������ô��
           03  SYU-SKYMONEY-LIFE       PIC S9(07).
      *������������������ô�ۡ��ݸ���������ô�����ǡ�
           03  SYU-SKYMONEY-LIFE-TAX   PIC S9(07).
      *������������������ô�ۡ��ݸ���������ô��ס�
           03  SYU-SKYMONEY-LIFE-KEI   PIC S9(07).
      *������������������ʼ����
           03  SYU-RYOYOHI-LIFE-JIHI   PIC S9(07).
      *������������������ô�ۡʼ��񡧼�����ô��
           03  SYU-SKYMONEY-LIFE-JIHI  PIC S9(07).
      *������������������ô�ۡʼ��񡧼�����ô�����ǡ�
           03  SYU-SKYMONEY-LIFE-JIHI-TAX
                                       PIC S9(07).
      *������������������ô�ۡʼ��񡧼�����ô��ס�
           03  SYU-SKYMONEY-LIFE-JIHI-KEI
                                       PIC S9(07).
      *������ô����ô���̴����ѡ�
           03  SYU-SKJ-PTFTN           OCCURS 3.
               05  SYU-SKJ-FTNGAKU     PIC S9(05).
               05  SYU-SKJ-FTNDAY      PIC S9(02).
      *�Ʒ׻���ʬ
           03  SYU-SAIKEISANKBN        PIC  X(01).
      *����������ʬ
           03  SYU-INGAISHOHOKBN       PIC  X(01).
      *Ʊ��������ɼ�ֹ�
           03  SYU-DOUJI-DENPNUM       PIC  9(07).
      *��³��ʬ
           03  SYU-CONTKBN             PIC  X(01).
      *���ťե饰�ơ��֥�
           03  SYU-DAY-AREA.
      *        ���ťե饰�ơ��֥�
             04  SYU-DAY-TBL.
      *        ���ťե饰�ʣ������飳������
               05  SYU-DAY-1           PIC  X(01).
               05  SYU-DAY-2           PIC  X(01).
               05  SYU-DAY-3           PIC  X(01).
               05  SYU-DAY-4           PIC  X(01).
               05  SYU-DAY-5           PIC  X(01).
               05  SYU-DAY-6           PIC  X(01).
               05  SYU-DAY-7           PIC  X(01).
               05  SYU-DAY-8           PIC  X(01).
               05  SYU-DAY-9           PIC  X(01).
               05  SYU-DAY-10          PIC  X(01).
               05  SYU-DAY-11          PIC  X(01).
               05  SYU-DAY-12          PIC  X(01).
               05  SYU-DAY-13          PIC  X(01).
               05  SYU-DAY-14          PIC  X(01).
               05  SYU-DAY-15          PIC  X(01).
               05  SYU-DAY-16          PIC  X(01).
               05  SYU-DAY-17          PIC  X(01).
               05  SYU-DAY-18          PIC  X(01).
               05  SYU-DAY-19          PIC  X(01).
               05  SYU-DAY-20          PIC  X(01).
               05  SYU-DAY-21          PIC  X(01).
               05  SYU-DAY-22          PIC  X(01).
               05  SYU-DAY-23          PIC  X(01).
               05  SYU-DAY-24          PIC  X(01).
               05  SYU-DAY-25          PIC  X(01).
               05  SYU-DAY-26          PIC  X(01).
               05  SYU-DAY-27          PIC  X(01).
               05  SYU-DAY-28          PIC  X(01).
               05  SYU-DAY-29          PIC  X(01).
               05  SYU-DAY-30          PIC  X(01).
               05  SYU-DAY-31          PIC  X(01).
           03  SYU-DAY-AREA-R          REDEFINES   SYU-DAY-AREA.
      *        ���Ų���ơ��֥�
               05  SYU-DAY-TBL2.
                   07  SYU-DAY         PIC  X(01)
                                         OCCURS   31.
      *ʣ���ʤޤȤἱ����ɼ�ֹ�
           03  SYU-FUKU-DENPNUM        PIC  9(07).
      *ʣ���ʤޤȤ��ʬ�ʣ�����³�����������
           03  SYU-FUKU-KBN            PIC  X(01).
      *�����Ƕ�ʬ
           03  SYU-ZAITAKU             PIC  X(01).
      *���Ŷ�ʬ�̵��ճ�����
           03  SYU-KYUFUGAI.
               05  SYU-KYUFUGAI-TBL          OCCURS   6.
      *    �����оݳ�����
                   09  SYU-KYUFUGAI-TEN      PIC S9(07).
      *���Ŷ�ʬ�̵��ճ����������
           03  SYU-KYUFUGAI-R      REDEFINES   SYU-KYUFUGAI.
      *�����оݳ������ʽ�ǡ�
              05  SYU-KYUFUGAI-SHOSHIN-TEN    PIC S9(07).
      *�����оݳ������ʻ�Ƴ��
              05  SYU-KYUFUGAI-SIDOU-TEN      PIC S9(07).
      *�����оݳ������ʱ��ǡ�
              05  SYU-KYUFUGAI-OSIN1-TEN      PIC S9(07).
      *�����оݳ������ʱ��ǡ���֡�
              05  SYU-KYUFUGAI-OSIN2-TEN      PIC S9(07).
      *�����оݳ������ʱ��ǡ����롿�۵ޡ�
              05  SYU-KYUFUGAI-OSIN3-TEN      PIC S9(07).
      *�����оݳ������ʹ�ס�
              05  SYU-KYUFUGAI-GOKEI-TEN      PIC S9(07).
      *���̼�Ǽ�ե饰
           03  SYU-DAYINFFLG           PIC  X(01).
      *ʬ�ڴ����ե饰
           03  SYU-BUNBENINFFLG        PIC  X(01).
      *���������ֹ�
           03  SYU-NYUIN-RRKNUM        PIC  9(03).
      *����ѹ���ʬ
           03  SYU-ACCT-UPDKBN         PIC  X(01).
      *��§�����ݴ�����ô���
           03  SYU-JYO-HKNFTNMONEY     PIC S9(07).
      *��§�����ݷ׻���ô��
           03  SYU-JYO-COMPFTN         PIC S9(07).
      *��§�����ݷ׻���ô��ʱ�ñ�̵�Ͽ�ѡ�
           03  SYU-JYO-COMPFTN-ENTANI  PIC S9(07)V9(01).
      *�ݸ�Ŭ�Ѷ�ʬ
           03  SYU-HKNTEKKBN           PIC  X(01).
      *��������¾�����飵
           03  SYU-NYUINETC-INF.
               05  SYU-NYUINETC1       PIC S9(07).
               05  SYU-NYUINETC2       PIC S9(07).
               05  SYU-NYUINETC3       PIC S9(07).
               05  SYU-NYUINETC4       PIC S9(07).
               05  SYU-NYUINETC5       PIC S9(07).
      *ü���ɣ�
           03  SYU-TERMID              PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYU-OPID                PIC  X(16).
      *����ǯ����
           03  SYU-CREYMD              PIC  X(08).
      *����ǯ����
           03  SYU-UPYMD               PIC  X(08).
      *��������
           03  SYU-UPHMS               PIC  X(06).
      *
      *����ǯ�����ʸ����ѡ�
           03  SYU-UPSRYYMD            PIC  X(08).
      *����ɣġʸ����ѡ�
           03  SYU-SELKOHID            PIC  9(10).
      *������ʬ�ʸ����ѡ�
           03  SYU-SELNYUGAIKBN        PIC  X(01).