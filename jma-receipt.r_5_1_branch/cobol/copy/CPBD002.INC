      ************************************************************
      *    ����ɽ�Ѽ�Ǽ�ޥ���         COPY (CPBD002.INC)     *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    MCC-ƣ��     02.02.27  ��Ǽ�ģ¤��ѹ��ˤ�꽤��
      *  01.00.02    NACL-��Ʀ��  04.01.21  ��Ǽ�ģ¤��ѹ��ˤ�꽤��
      *  01.00.03    NACL-����    04.03.17  ��Ǽ�ģ¤��ѹ��ˤ�꽤��
      *  01.00.04    NACL-����    05.08.24  ��Ǽ�ģ¤��ѹ��ˤ�꽤��
      *  01.00.05    NACL-��Ʀ��  06.07.06  ��Ǽ�ģ¤��ѹ��ˤ�꽤��
      *  01.00.06    NACL-����    06.09.21  ��Ǽ�ģ¤��ѹ��ˤ�꽤��
      *  04.04.00    NACL-����    09.03.03  ��Ǽ�ģ¤��ѹ��ˤ�꽤��
      ************************************************************
      *01  BD002-REC.
           03  BD002-KEY.
      *���ŵ��أɣ�
               05  BD002-HOSPNUM          PIC 9(2).
      *������ʬ
               05  BD002-NYUGAIKBN       PIC  X(01).
      *���ԣɣ�
               05  BD002-PTID            PIC  9(10).
      *��ɼ�ֹ�
               05  BD002-DENPNUM         PIC  9(07).
      *��ɼ���ֺǽ��ֹ�
           03  BD002-DENPLASTNUM         PIC  9(02).
      *���Ų�
           03  BD002-SRYKA               PIC  X(02).
      *������
           03  BD002-SRYYMD              PIC  X(08).
      *��ɼ���ֶ�ʬ
           03  BD002-DENPJTIKBN          PIC  X(01).
      *��ɼ������ʬ�ʶ��򡧳��衡�����ౡ�������������������
      *              ���������쥻�ץ��ѥ��ߡ���
           03  BD002-CREATEKBN           PIC  X(01).
      *��ɼȯ����
           03  BD002-DENPPRTYMD          PIC  X(08).
      *�������
           03  BD002-SKY-KIKAN.
      *���ᳫ����
               05  BD002-SKYSTYMD        PIC  X(08).
      *���Ὢλ��
               05  BD002-SKYEDYMD        PIC  X(08).
      *�ݸ��ȹ礻�ֹ�
           03  BD002-HKNCOMBINUM         PIC  X(04).
      *���ݸ����ݸ��ֹ�
           03  BD002-SYUHKNNUM           PIC  X(03).
      *���ݸ��ݴ�����ô���
           03  BD002-SYUHKNFTNMONEY      PIC  9(07).
      *���ݸ��ݷ׻���ô��
           03  BD002-SYUCOMPFTN          PIC  9(07).
      *���ݸ��ݷ׻���ô��ʱ�ñ�̵�Ͽ�ѡ�
           03  BD002-SYUCOMPFTN-ENTANI   PIC  9(07)V9(01).
      *���ݸ��ݷ׻����ް�����ô��
           03  BD002-SYUCOMPYKZFTN       PIC  9(07).
      *���ݸ��ݷ׻���ô���
           03  BD002-SYUCOMPTOTAL        PIC  9(07).
      *���ݸ���������ô��ʬ
           03  BD002-SYUYKZFTNKBN        PIC  X(01).
      *�������ô����
           03  BD002-KOH-AREA.
      *���񣱡��ݸ��ֹ�
               05  BD002-KOH1HKNNUM          PIC  X(03).
      *���񣱡ݴ�����ô���
               05  BD002-KOH1HKNFTNMONEY     PIC  9(07).
      *���񣱡ݷ׻���ô��
               05  BD002-KOH1COMPFTN         PIC  9(07).
      *���񣱡ݷ׻���ô��ʱ�ñ�̵�Ͽ�ѡ�
               05  BD002-KOH1COMPFTN-ENTANI  PIC  9(07)V9(01).
      *���񣱡ݷ׻����ް�����ô��
               05  BD002-KOH1COMPYKZFTN      PIC  9(07).
      *���񣱡ݷ׻���ô���
               05  BD002-KOH1COMPTOTAL       PIC  9(07).
      *���񣱡�������ô��ʬ
               05  BD002-KOH1YKZFTNKBN       PIC  X(01).
      *���񣲡��ݸ��ֹ�
               05  BD002-KOH2HKNNUM          PIC  X(03).
      *���񣲡ݴ�����ô���
               05  BD002-KOH2HKNFTNMONEY     PIC  9(07).
      *���񣲡ݷ׻���ô��
               05  BD002-KOH2COMPFTN         PIC  9(07).
      *���񣲡ݷ׻���ô��ʱ�ñ�̵�Ͽ�ѡ�
               05  BD002-KOH2COMPFTN-ENTANI  PIC  9(07)V9(01).
      *���񣲡ݷ׻����ް�����ô��
               05  BD002-KOH2COMPYKZFTN      PIC  9(07).
      *���񣲡ݷ׻���ô���
               05  BD002-KOH2COMPTOTAL       PIC  9(07).
      *���񣲡�������ô��ʬ
               05  BD002-KOH2YKZFTNKBN       PIC  X(01).
      *���񣳡��ݸ��ֹ�
               05  BD002-KOH3HKNNUM          PIC  X(03).
      *���񣳡ݴ�����ô���
               05  BD002-KOH3HKNFTNMONEY     PIC  9(07).
      *���񣳡ݷ׻���ô��
               05  BD002-KOH3COMPFTN         PIC  9(07).
      *���񣳡ݷ׻���ô��ʱ�ñ�̵�Ͽ�ѡ�
               05  BD002-KOH3COMPFTN-ENTANI  PIC  9(07)V9(01).
      *���񣳡ݷ׻����ް�����ô��
               05  BD002-KOH3COMPYKZFTN      PIC  9(07).
      *���񣳡ݷ׻���ô���
               05  BD002-KOH3COMPTOTAL       PIC  9(07).
      *���񣳡�������ô��ʬ
               05  BD002-KOH3YKZFTNKBN       PIC  X(01).
      *���񣴡��ݸ��ֹ�
               05  BD002-KOH4HKNNUM          PIC  X(03).
      *���񣴡ݴ�����ô���
               05  BD002-KOH4HKNFTNMONEY     PIC  9(07).
      *���񣴡ݷ׻���ô��
               05  BD002-KOH4COMPFTN         PIC  9(07).
      *���񣴡ݷ׻���ô��ʱ�ñ�̵�Ͽ�ѡ�
               05  BD002-KOH4COMPFTN-ENTANI  PIC  9(07)V9(01).
      *���񣴡ݷ׻����ް�����ô��
               05  BD002-KOH4COMPYKZFTN      PIC  9(07).
      *���񣴡ݷ׻���ô���
               05  BD002-KOH4COMPTOTAL       PIC  9(07).
      *���񣴡�������ô��ʬ
               05  BD002-KOH4YKZFTNKBN       PIC  X(01).
      *������ô��������
           03  BD002-KOHFTN-TABLE  REDEFINES   BD002-KOH-AREA.
               05  BD002-KOHFTN-TBL          OCCURS  4.
      *������ݸ��ֹ�
                 07  BD002-KOH-HKNNUM          PIC  X(03).
      *����ݴ�����ô���
                 07  BD002-KOH-HKNFTNMONEY     PIC  9(07).
      *����ݷ׻���ô��
                 07  BD002-KOH-COMPFTN         PIC  9(07).
      *����ݷ׻���ô��ʱ�ñ�̵�Ͽ�ѡ�
                 07  BD002-KOH-COMPFTN-ENTANI  PIC  9(07)V9(01).
      *����ݷ׻����ް�����ô��
                 07  BD002-KOH-COMPYKZFTN      PIC  9(07).
      *����ݷ׻���ô���
                 07  BD002-KOH-COMPTOTAL       PIC  9(07).
      *�����������ô��ʬ
                 07  BD002-KOH-YKZFTNKBN       PIC  X(01).
      *������ôΨ
           03  BD002-PTFTNRATE           PIC  9(03).
      *�����Σ�
           03  BD002-ROOMNUM             PIC  X(08).
      *���拾����
           03  BD002-BYOTONUM            PIC  X(02).
      *����������ʬ
           03  BD002-SKYKUKBN            PIC  X(01).
      *�������оݶ��
           03  BD002-TAX-TAISHOU         PIC  9(07).
      *�����ǡ����ǡ�
           03  BD002-TAX-MONEY           PIC  9(07).
      *�����ǡʹ�ס�
           03  BD002-SKYGK               PIC  9(07).
      *��������
           03  BD002-SKY-NAIYOU.
               05  BD002-SKY-TBL             OCCURS   17.
      *    �ݸ�����
                   09  BD002-HKNTEN          PIC  9(07).
      *    ���
                   09  BD002-MONEY           PIC  9(07).
      *    Ŭ�ѳ����
                   09  BD002-TGMONEY         PIC  9(07).
      *    Ŭ�ѳ���ۡʾ����Ǥ����
                   09  BD002-TGMONEY-TAX     PIC  9(07).
      *��������
           03  BD002-SKY-NAIYOU-R        REDEFINES   BD002-SKY-NAIYOU.
      *������(01)
              05  BD002-SSU.
                  09  BD002-SSU-HKNTEN          PIC  9(07).
                  09  BD002-SSU-MONEY           PIC  9(07).
                  09  BD002-SSU-TGMONEY         PIC  9(07).
                  09  BD002-SSU-TGMONEY-TAX     PIC  9(07).
      *��Ƴ��(02)
              05  BD002-SDO.
                  09  BD002-SDO-HKNTEN          PIC  9(07).
                  09  BD002-SDO-MONEY           PIC  9(07).
                  09  BD002-SDO-TGMONEY         PIC  9(07).
                  09  BD002-SDO-TGMONEY-TAX     PIC  9(07).
      *������(03)
              05  BD002-ZTK.
                  09  BD002-ZTK-HKNTEN          PIC  9(07).
                  09  BD002-ZTK-MONEY           PIC  9(07).
                  09  BD002-ZTK-TGMONEY         PIC  9(07).
                  09  BD002-ZTK-TGMONEY-TAX     PIC  9(07).
      *������(04)
              05  BD002-TYK.
                  09  BD002-TYK-HKNTEN          PIC  9(07).
                  09  BD002-TYK-MONEY           PIC  9(07).
                  09  BD002-TYK-TGMONEY         PIC  9(07).
                  09  BD002-TYK-TGMONEY-TAX     PIC  9(07).
      *������(05)
              05  BD002-CSY.
                  09  BD002-CSY-HKNTEN          PIC  9(07).
                  09  BD002-CSY-MONEY           PIC  9(07).
                  09  BD002-CSY-TGMONEY         PIC  9(07).
                  09  BD002-CSY-TGMONEY-TAX     PIC  9(07).
      *������(06)
              05  BD002-SYC.
                  09  BD002-SYC-HKNTEN          PIC  9(07).
                  09  BD002-SYC-MONEY           PIC  9(07).
                  09  BD002-SYC-TGMONEY         PIC  9(07).
                  09  BD002-SYC-TGMONEY-TAX     PIC  9(07).
      *�����(07)
              05  BD002-SJT.
                  09  BD002-SJT-HKNTEN          PIC  9(07).
                  09  BD002-SJT-MONEY           PIC  9(07).
                  09  BD002-SJT-TGMONEY         PIC  9(07).
                  09  BD002-SJT-TGMONEY-TAX     PIC  9(07).
      *���(08)
              05  BD002-MSI.
                  09  BD002-MSI-HKNTEN          PIC  9(07).
                  09  BD002-MSI-MONEY           PIC  9(07).
                  09  BD002-MSI-TGMONEY         PIC  9(07).
                  09  BD002-MSI-TGMONEY-TAX     PIC  9(07).
      *������(09)
              05  BD002-KNS.
                  09  BD002-KNS-HKNTEN          PIC  9(07).
                  09  BD002-KNS-MONEY           PIC  9(07).
                  09  BD002-KNS-TGMONEY         PIC  9(07).
                  09  BD002-KNS-TGMONEY-TAX     PIC  9(07).
      *������(10)
              05  BD002-GZU.
                  09  BD002-GZU-HKNTEN          PIC  9(07).
                  09  BD002-GZU-MONEY           PIC  9(07).
                  09  BD002-GZU-TGMONEY         PIC  9(07).
                  09  BD002-GZU-TGMONEY-TAX     PIC  9(07).
      *���(11)
              05  BD002-ETC.
                  09  BD002-ETC-HKNTEN          PIC  9(07).
                  09  BD002-ETC-MONEY           PIC  9(07).
                  09  BD002-ETC-TGMONEY         PIC  9(07).
                  09  BD002-ETC-TGMONEY-TAX     PIC  9(07).
      *������������ˡ(12)
              05  BD002-SSN.
                  09  BD002-SSN-HKNTEN          PIC  9(07).
                  09  BD002-SSN-MONEY           PIC  9(07).
                  09  BD002-SSN-TGMONEY         PIC  9(07).
                  09  BD002-SSN-TGMONEY-TAX     PIC  9(07).
      *������(13)
              05  BD002-HOU.
                  09  BD002-HOU-HKNTEN          PIC  9(07).
                  09  BD002-HOU-MONEY           PIC  9(07).
                  09  BD002-HOU-TGMONEY         PIC  9(07).
                  09  BD002-HOU-TGMONEY-TAX     PIC  9(07).
      *��������(14)
              05  BD002-BYR.
                  09  BD002-BYR-HKNTEN          PIC  9(07).
                  09  BD002-BYR-MONEY           PIC  9(07).
                  09  BD002-BYR-TGMONEY         PIC  9(07).
                  09  BD002-BYR-TGMONEY-TAX     PIC  9(07).
      *������(15)
              05  BD002-NYN.
                  09  BD002-NYN-HKNTEN          PIC  9(07).
                  09  BD002-NYN-MONEY           PIC  9(07).
                  09  BD002-NYN-TGMONEY         PIC  9(07).
                  09  BD002-NYN-TGMONEY-TAX     PIC  9(07).
      *����ô������(16)
              05  BD002-RYO.
                  09  BD002-RYO-HKNTEN          PIC  9(07).
                  09  BD002-RYO-MONEY           PIC  9(07).
                  09  BD002-RYO-TGMONEY         PIC  9(07).
                  09  BD002-RYO-TGMONEY-TAX     PIC  9(07).
      *���  (17)
              05  BD002-TOTAL.
                  09  BD002-TOTAL-HKNTEN        PIC  9(07).
                  09  BD002-TOTAL-MONEY         PIC  9(07).
                  09  BD002-TOTAL-TGMONEY       PIC  9(07).
                  09  BD002-TOTAL-TGMONEY-TAX   PIC  9(07).
      *�����������Ʒ�
           03  BD002-SHOHOU-SAI                 PIC  9(07).
      *�ݸ�Ŭ�ѳ���ۡݾ����ǡʺƷǡ�
           03  BD002-TGMONEY-TAX-SAI         PIC  9(07).
      *���񣱤��飱��
           03  BD002-JIHI-NAIYOU.
               05  BD002-JIHI-1              PIC  9(07). 
               05  BD002-JIHI-1-TAX          PIC  9(07). 
               05  BD002-JIHI-2              PIC  9(07). 
               05  BD002-JIHI-2-TAX          PIC  9(07). 
               05  BD002-JIHI-3              PIC  9(07). 
               05  BD002-JIHI-3-TAX          PIC  9(07). 
               05  BD002-JIHI-4              PIC  9(07). 
               05  BD002-JIHI-4-TAX          PIC  9(07). 
               05  BD002-JIHI-5              PIC  9(07). 
               05  BD002-JIHI-5-TAX          PIC  9(07). 
               05  BD002-JIHI-6              PIC  9(07). 
               05  BD002-JIHI-6-TAX          PIC  9(07). 
               05  BD002-JIHI-7              PIC  9(07). 
               05  BD002-JIHI-7-TAX          PIC  9(07). 
               05  BD002-JIHI-8              PIC  9(07). 
               05  BD002-JIHI-8-TAX          PIC  9(07). 
               05  BD002-JIHI-9              PIC  9(07). 
               05  BD002-JIHI-9-TAX          PIC  9(07). 
               05  BD002-JIHI-10             PIC  9(07). 
               05  BD002-JIHI-10-TAX         PIC  9(07). 
           03  BD002-JIHI-NAIYOU-R   REDEFINES  BD002-JIHI-NAIYOU.
               05  BD002-JIHI-TBL            OCCURS   10.
                 07  BD002-JIHI-TAXNASI        PIC  9(07).
                 07  BD002-JIHI-TAXARI         PIC  9(07).
      *���񾮷סʾ����Ǥʤ���
           03  BD002-JIHI-TOTAL          PIC  9(07).
      *���񾮷סʾ����Ǥ����
           03  BD002-JIHI-TOTAL-TAX      PIC  9(07).
      *�����۾�����
           03  BD002-JIHI-TAX            PIC  9(07).
      *Ϸ�Ͱ�����ô��
           03  BD002-RJN-FTN             PIC  9(07).
      *���������ô��
           03  BD002-KOH-FTN             PIC  9(07).
      *���������ô��ʱ�ñ�̡�
           03  BD002-KOH-FTN-ENTANI      PIC  9(07)V9(01).
      *���ް�����ô��
           03  BD002-YKZ-FTN             PIC  9(07).
      *�쥻�ץȽ��������ް�����ô��
           03  BD002-RESE-YKZ-FTN        PIC  9(07).
      *�����о����޶�ʬ
           03  BD002-KOHTAIYKZKBN        PIC  X(01).
      *Ĵ����
           03  BD002-CHOSEI              PIC S9(07).
           03  BD002-CHOSEI-NAIYOU.
      *Ĵ����������
               05  BD002-CHOSEI1         PIC S9(07).
      *Ĵ����������
               05  BD002-CHOSEI2         PIC S9(07).
           03  BD002-CHOSEI-NAIYOU-R REDEFINES  BD002-CHOSEI-NAIYOU.
               05  BD002-RCHOSEI         PIC S9(07)
                                         OCCURS  2.
      *�ޤȤ�������ɼ�ֹ�
           03  BD002-GRP-DENPNUM         PIC 9(07).
      *�ޤȤ�����Ϣ��
           03  BD002-GRP-RENNUM          PIC 9(02).
      *�ޤȤ����Ϻ���
           03  BD002-GRP-SGKMONEY        PIC S9(07).
      *�ޤȤ�ȯ����ˡ�ե饰
           03  BD002-GRP-HAKHOUFLG       PIC  9(01).
      *�ݾڶ��
           03  BD002-SECMONEY            PIC  9(07).
      *�ݸ�Ŭ�Ѷ��
           03  BD002-HKNTEKMONEY         PIC  9(07).
      *�±����Ȼ�ͳ��ʬ
           03  BD002-DISCOUNT-KBN        PIC  X(02).
      *�±����Ȼ�ͳ
           03  BD002-DISCOUNT-BODY       PIC  X(160).
      *���Ψ��ʬ
           03  BD002-DISCOUNT-RATEKBN    PIC  X(02).
      *��Ψ��۶�ʬ
           03  BD002-DISCOUNT-TEIRITU    PIC  X(01).
      *���Ψ
           03  BD002-DISCOUNT-RATE       PIC  9(07).
      *���ȶ��
           03  BD002-DISCOUNT-MONEY      PIC  9(07).
      *ϫ���ݸ��ݽ����
           03  BD002-RSISHOSHIN-MONEY    PIC  9(07).
      *ϫ���ݸ��ݺƿ���
           03  BD002-RSISAISHIN-MONEY    PIC  9(07).
      *ϫ���ݸ��ݻ�Ƴ��
           03  BD002-RSISDO-MONEY        PIC  9(07).
      *ϫ���ݸ��ݤ���¾
           03  BD002-RSIETC-MONEY        PIC  9(07).
      *ϫ���ݸ��ݾ����ǡʺƷǡ�
           03  BD002-RSI-TAX-SAI         PIC  9(07).
      *ϫ���ݸ��ݴ�������ʬ�߷��ƹ�׳�
           03  BD002-RSI-TOTAL           PIC  9(07).
      *ϫ�Ҽ���������ۡ�����ɽ�����ѡ�
           03  BD002-RSIJIBAI-SKYMONEY   PIC  9(07).
      *�����ۡݾ����ǡʺƷǡ�
           03  BD002-SKYMONEY-TAX-SAI    PIC  9(07).
      *������
           03  BD002-SKYMONEY            PIC  9(07).
      *�����׳�
           03  BD002-NYUKIN-TOTAL        PIC  9(07).
      *������
           03  BD002-NYUKIN-KAISU        PIC  9(07).
      *̤����ͳ
           03  BD002-MISYURIYU           PIC  X(02).
      *�ɥ�����
           03  BD002-DRCD-G.
               05  BD002-DRCD-SIK            PIC  X(01).
               05  BD002-DRCD                PIC  X(04).
      *������ǲ��
           03  BD002-NYNSHOKAISU         PIC  9(02).
      *����ȯ���ֹ�
           03  BD002-YKZKENNUM           PIC  9(05).
      *��������
           03  BD002-RMSAGAKU            PIC  9(07).
      *�������ۡݾ����ǡʺƷǡ�
           03  BD002-RMSAGAKU-TAX-SAI    PIC  9(07).
      *���������鿩������
           03  BD002-SHOKUJI-NAIYOU.
               05  BD002-SHOKUJI1-NISSU  PIC  9(02).
               05  BD002-SHOKUJI1        PIC  9(07).
               05  BD002-SHOKUJI2-NISSU  PIC  9(02).
               05  BD002-SHOKUJI2        PIC  9(07).
               05  BD002-SHOKUJI3-NISSU  PIC  9(02).
               05  BD002-SHOKUJI3        PIC  9(07).
               05  BD002-SHOKUJI4-NISSU  PIC  9(02).
               05  BD002-SHOKUJI4        PIC  9(07).
               05  BD002-SHOKUJI5-NISSU  PIC  9(02).
               05  BD002-SHOKUJI5        PIC  9(07).
               05  BD002-SHOKUJI6-NISSU  PIC  9(02).
               05  BD002-SHOKUJI6        PIC  9(07).
               05  BD002-SHOKUJI7-NISSU  PIC  9(02).
               05  BD002-SHOKUJI7        PIC  9(07).
               05  BD002-SHOKUJI8-NISSU  PIC  9(02).
               05  BD002-SHOKUJI8        PIC  9(07).
               05  BD002-SHOKUJI9-NISSU  PIC  9(02).
               05  BD002-SHOKUJI9        PIC  9(07).
               05  BD002-SHOKUJI10-NISSU PIC  9(02).
               05  BD002-SHOKUJI10       PIC  9(07).
           03  BD002-SYOKUJI-NAIYOUR     
                     REDEFINES   BD002-SHOKUJI-NAIYOU.
               05  BD002-SYOKUJI-TBL     OCCURS   10.
                 07  BD002-SHOKUJI-NISSU   PIC  9(02).
                 07  BD002-SHOKUJI         PIC  9(07).
      *�������������ʿ�������������������ɸ����ô�ۡ�
           03  BD002-SKJ-AREA.
               05  BD002-SYUSKJ-NISSU      PIC  9(02).
               05  BD002-SYUSKJ-RYOYOHI    PIC  9(07).
               05  BD002-SYUSKJ-FTN        PIC  9(07).
               05  BD002-SYUSKJ-FTN-RECE   PIC  9(07).
               05  BD002-SYUSKJ-FTNKBN     PIC  X(01).
           03  BD002-SKJ-AREA2.
               05  BD002-KOH1SKJ-NISSU     PIC  9(02).
               05  BD002-KOH1SKJ-RYOYOHI   PIC  9(07).
               05  BD002-KOH1SKJ-FTN       PIC  9(07).
               05  BD002-KOH1SKJ-FTN-RECE  PIC  9(07).
               05  BD002-KOH1SKJ-FTNKBN    PIC  X(01).
               05  BD002-KOH2SKJ-NISSU     PIC  9(02).
               05  BD002-KOH2SKJ-RYOYOHI   PIC  9(07).
               05  BD002-KOH2SKJ-FTN       PIC  9(07).
               05  BD002-KOH2SKJ-FTN-RECE  PIC  9(07).
               05  BD002-KOH2SKJ-FTNKBN    PIC  X(01).
               05  BD002-KOH3SKJ-NISSU     PIC  9(02).
               05  BD002-KOH3SKJ-RYOYOHI   PIC  9(07).
               05  BD002-KOH3SKJ-FTN       PIC  9(07).
               05  BD002-KOH3SKJ-FTN-RECE  PIC  9(07).
               05  BD002-KOH3SKJ-FTNKBN    PIC  X(01).
               05  BD002-KOH4SKJ-NISSU     PIC  9(02).
               05  BD002-KOH4SKJ-RYOYOHI   PIC  9(07).
               05  BD002-KOH4SKJ-FTN       PIC  9(07).
               05  BD002-KOH4SKJ-FTN-RECE  PIC  9(07).
               05  BD002-KOH4SKJ-FTNKBN    PIC  X(01).
           03  BD002-SKJ-AREA2R          REDEFINES   BD002-SKJ-AREA2.
               05  BD002-SKJ-TBL         OCCURS   4.
                 07  BD002-SKJ-NISSU       PIC  9(02).
                 07  BD002-SKJ-RYOYOHI     PIC  9(07).
                 07  BD002-SKJ-FTN         PIC  9(07).
                 07  BD002-SKJ-FTN-RECE    PIC  9(07).
                 07  BD002-SKJ-FTNKBN      PIC  X(01).
      *������������ݸ���
           03  BD002-RYOYOHI-SKJ         PIC  9(07).
      *������ô�ۡ��ݸ���������ô��
           03  BD002-SKYMONEY-SKJ        PIC  9(07).
      *������ô�ۡ��ݸ���������ô�����ǡ�
           03  BD002-SKYMONEY-SKJ-TAX    PIC  9(07).
      *������ô�ۡ��ݸ���������ô��ס�
           03  BD002-SKYMONEY-SKJ-KEI    PIC  9(07).
      *����������ʼ����
           03  BD002-RYOYOHI-SKJ-JIHI    PIC  9(07).
      *������ô�ۡʼ��񡧼�����ô��
           03  BD002-SKYMONEY-SKJ-JIHI   PIC  9(07).
      *������ô�ۡʼ��񡧼�����ô�����ǡ�
           03  BD002-SKYMONEY-SKJ-JIHI-TAX
                                       PIC  9(07).
      *������ô�ۡʼ��񡧼�����ô��ס�
           03  BD002-SKYMONEY-SKJ-JIHI-KEI
                                       PIC  9(07).
      *������������������ݸ���
           03  BD002-RYOYOHI-LIFE        PIC  9(07).
      *������������������ô�ۡ��ݸ���������ô��
           03  BD002-SKYMONEY-LIFE       PIC  9(07).
      *������������������ô�ۡ��ݸ���������ô�����ǡ�
           03  BD002-SKYMONEY-LIFE-TAX   PIC  9(07).
      *������������������ô�ۡ��ݸ���������ô��ס�
           03  BD002-SKYMONEY-LIFE-KEI   PIC  9(07).
      *������������������ʼ����
           03  BD002-RYOYOHI-LIFE-JIHI   PIC  9(07).
      *������������������ô�ۡʼ��񡧼�����ô��
           03  BD002-SKYMONEY-LIFE-JIHI  PIC  9(07).
      *������������������ô�ۡʼ��񡧼�����ô�����ǡ�
           03  BD002-SKYMONEY-LIFE-JIHI-TAX
                                       PIC  9(07).
      *������������������ô�ۡʼ��񡧼�����ô��ס�
           03  BD002-SKYMONEY-LIFE-JIHI-KEI
                                       PIC  9(07).
      *������ô����ô���̴����ѡ�
           03  BD002-SKJ-PTFTN           OCCURS 3.
               05  BD002-SKJ-FTNGAKU     PIC  9(05).
               05  BD002-SKJ-FTNDAY      PIC  9(02).
      *�Ʒ׻���ʬ
           03  BD002-SAIKEISANKBN        PIC  X(01).
      *����������ʬ
           03  BD002-INGAISHOHOKBN       PIC  X(01).
      *Ʊ��������ɼ�ֹ�
           03  BD002-DOUJI-DENPNUM       PIC  9(07).
      *��³��ʬ
           03  BD002-CONTKBN             PIC  X(01).
      *���ťե饰�ơ��֥�
           03  BD002-DAY-AREA.
      *        ���ťե饰�ơ��֥�
             04  BD002-DAY-TBL.
      *        ���ťե饰�ʣ������飳������
               05  BD002-DAY-1           PIC  X(01).
               05  BD002-DAY-2           PIC  X(01).
               05  BD002-DAY-3           PIC  X(01).
               05  BD002-DAY-4           PIC  X(01).
               05  BD002-DAY-5           PIC  X(01).
               05  BD002-DAY-6           PIC  X(01).
               05  BD002-DAY-7           PIC  X(01).
               05  BD002-DAY-8           PIC  X(01).
               05  BD002-DAY-9           PIC  X(01).
               05  BD002-DAY-10          PIC  X(01).
               05  BD002-DAY-11          PIC  X(01).
               05  BD002-DAY-12          PIC  X(01).
               05  BD002-DAY-13          PIC  X(01).
               05  BD002-DAY-14          PIC  X(01).
               05  BD002-DAY-15          PIC  X(01).
               05  BD002-DAY-16          PIC  X(01).
               05  BD002-DAY-17          PIC  X(01).
               05  BD002-DAY-18          PIC  X(01).
               05  BD002-DAY-19          PIC  X(01).
               05  BD002-DAY-20          PIC  X(01).
               05  BD002-DAY-21          PIC  X(01).
               05  BD002-DAY-22          PIC  X(01).
               05  BD002-DAY-23          PIC  X(01).
               05  BD002-DAY-24          PIC  X(01).
               05  BD002-DAY-25          PIC  X(01).
               05  BD002-DAY-26          PIC  X(01).
               05  BD002-DAY-27          PIC  X(01).
               05  BD002-DAY-28          PIC  X(01).
               05  BD002-DAY-29          PIC  X(01).
               05  BD002-DAY-30          PIC  X(01).
               05  BD002-DAY-31          PIC  X(01).
           03  BD002-DAY-AREA-R  REDEFINES   BD002-DAY-AREA.
      *        ���Ų���ơ��֥�
               05  BD002-DAY-TBL2.
                   07  BD002-DAY         PIC  X(01)
                                         OCCURS   31.
      *ʣ���ʤޤȤἱ����ɼ�ֹ�
           03  BD002-FUKU-DENPNUM        PIC  9(07).
      *ʣ���ʤޤȤ��ʬ�ʣ�����³�����������
           03  BD002-FUKU-KBN            PIC  X(01).
      *�����Ƕ�ʬ
           03  BD002-ZAITAKU             PIC  X(01).
      *���Ŷ�ʬ�̵��ճ�����
           03  BD002-KYUFUGAI.
               05  BD002-KYUFUGAI-TBL          OCCURS   6.
      *    �����оݳ�����
                   09  BD002-KYUFUGAI-TEN      PIC  9(07).
      *���Ŷ�ʬ�̵��ճ����������
           03  BD002-KYUFUGAI-R  REDEFINES   BD002-KYUFUGAI.
      *�����оݳ������ʽ�ǡ�
              05  BD002-KYUFUGAI-SHOSHIN-TEN    PIC  9(07).
      *�����оݳ������ʻ�Ƴ��
              05  BD002-KYUFUGAI-SIDOU-TEN      PIC  9(07).
      *�����оݳ������ʱ��ǡ�
              05  BD002-KYUFUGAI-OSIN1-TEN      PIC  9(07).
      *�����оݳ������ʱ��ǡ���֡�
              05  BD002-KYUFUGAI-OSIN2-TEN      PIC  9(07).
      *�����оݳ������ʱ��ǡ����롿�۵ޡ�
              05  BD002-KYUFUGAI-OSIN3-TEN      PIC  9(07).
      *�����оݳ������ʹ�ס�
              05  BD002-KYUFUGAI-GOKEI-TEN      PIC  9(07).
      *���̼�Ǽ�ե饰
           03  BD002-DAYINFFLG           PIC  X(01).
      *���������ֹ�
           03  BD002-NYUIN-RRKNUM        PIC  9(03).
      *����ѹ���ʬ
           03  BD002-ACCT-UPDKBN         PIC  X(01).
      *��§�����ݴ�����ô���
           03  BD002-JYO-HKNFTNMONEY     PIC  9(07).
      *��§�����ݷ׻���ô��
           03  BD002-JYO-COMPFTN         PIC  9(07).
      *��§�����ݷ׻���ô��ʱ�ñ�̵�Ͽ�ѡ�
           03  BD002-JYO-COMPFTN-ENTANI  PIC  9(07)V9(01).
      *ü���ɣ�
           03  BD002-TERMID              PIC  X(16).
      *���ڡ��졼���ɣ�
           03  BD002-OPID                PIC  X(16).
      *����ǯ����
           03  BD002-CREYMD              PIC  X(08).
      *����ǯ����
           03  BD002-UPYMD               PIC  X(08).
      *��������
           03  BD002-UPHMS               PIC  X(06).
      *�����ֹ�
           03  BD002-PTNUM               PIC  X(20).