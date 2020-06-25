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
      *    �������(����       COPY (CPSKYMAIN.INC)              *
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.05.01    NACL-ƣ��    09/11/13  �õ�����Ȳ��б�
      *                                     �õ�����ά���ɲ�
      *
      *  04.06.01    NACL-ƣ��    10/10/12  ����ñ�ȤΥ쥻ʬΥ�б�
      *                                     ������kohid �ɲ�
      *                                     Ĵ���Ѥ�crehms,updhms �ɲ�
      *  04.06.02    NACL-ƣ��    10/11/30  ����Ģɼ�Ѥι����ɲ�
      *                                     ����۾���ʰ����񡢿�����
      *                                     �ݸ��ȹ礻����
      *  04.06.03    NACL-ƣ��    12/10/26  �õ����ࣰ����Ϸʻ�˵ڤ�
      *                                     ������Ϸ��ˤΥ쥻�ץ��б�
      *
      *  04.07.01    NACL-ƣ��    12/02/13  �����ȹ�ؤ�ľ������ȿ���
      *                                     ��ʧ���������б�
      *                                     ľ������ե饰���ɲ�
      *
      *  04.08.01    NACL-ƣ��    17/04/10  ����ñ�̤Ǥ�������������
      *                                     ��ϫ�ҽ�����Ϸ��ݽ����б�
      *                                     �ݸ���ʬ�ʼ������ѡˤ��ɲ�
      ************************************************************
       01  SKYMAIN-REC.
      *�ˣţ�
           03  SKYMAIN-KEY.
      *���ŵ��أɣ�
               05  SKYMAIN-HOSPNUM          PIC 9(2).
      *����ǯ��
               05  SKYMAIN-SRYYM            PIC  9(06).
      *������ʬ
               05  SKYMAIN-NYUGAIKBN        PIC  X(01).
      *���ԣɣ�
               05  SKYMAIN-PTID             PIC  9(10).
      *�쥻�ץȲʡʶ���λ������򥻥åȡ�
               05  SKYMAIN-RECEKA           PIC  X(02).
      *������ʧ����
               05  SKYMAIN-TEISYUTUSAKI     PIC  9(01).
      *�쥻�ץȼ���
               05  SKYMAIN-RECESYUBETU      PIC  X(04).
      *�ݸ����ֹ�
               05  SKYMAIN-HKNJANUM         PIC  X(08).
      *�����ʬ�������ݸ��ΤȤ��Τߡ�
               05  SKYMAIN-HOJOKBN-KEY      PIC  X(01).
      *����ɣġʸ���ñ�ȤΤߡ�
               05  SKYMAIN-KOHID-KEY        PIC  9(10).
      *Ŭ�ѳ���ǯ����
               05  SKYMAIN-TEKSTYMD         PIC  X(08).
      *ʬ���ʬ
               05  SKYMAIN-RECEIPT-KBN      PIC  X(10).
      *�����ֹ�
           03  SKYMAIN-PTNUM                PIC  X(20).
      *���⡦������ʬ�ʣ������⡡������������
           03  SKYMAIN-PREFKBN              PIC  X(01).
      *���ֹ�
           03  SKYMAIN-PREFNUM              PIC  X(02).
      *ǯ��
           03  SKYMAIN-AGE                  PIC  9(03).
      *���ճ��
           03  SKYMAIN-KYURATE              PIC  9(03).
      *�����Ƕ�ʬ
           03  SKYMAIN-ZAITAKU              PIC  X(01).
      *����ǯ��
           03  SKYMAIN-SKYYM                PIC  X(06).
      *����ǯ��
           03  SKYMAIN-HENREIYM             PIC  X(06).
      *�����ʬ
           03  SKYMAIN-SKYKBN               PIC  X(01).
      *�����ʬ�ʥ쥻���оݡ�
           03  SKYMAIN-SKYKBN-RECEDEN       PIC  X(01).
      *������ʬ
           03  SKYMAIN-TEISEIKBN            PIC  X(01).
      *���ݸ����ݸ��ֹ�
           03  SKYMAIN-HKNNUM               PIC  X(03).
      *���ݸ����ݸ��ɣ�
           03  SKYMAIN-HKNID                PIC  9(10).
      *���ݸ����ܿͲ�²��ʬ
           03  SKYMAIN-HONKZKKBN            PIC  X(01).
      *���ݸ��������ʬ
           03  SKYMAIN-HOJOKBN              PIC  X(01).
      *���ݸ��ݷ�³��ʬ
           03  SKYMAIN-CONTKBN              PIC  X(01).
      *���ݸ��ݵ���
           03  SKYMAIN-KIGO                 PIC  X(80).
      *���ݸ����ֹ�
           03  SKYMAIN-NUM                  PIC  X(80).
      *Ϸ���ݸ����ݸ��ֹ�
           03  SKYMAIN-RJNHKNNUM            PIC  X(03).
      *Ϸ���ݸ����ݸ��ɣ�
           03  SKYMAIN-RJNHKNID             PIC  9(10).
      *�������
           03  SKYMAIN-KOHINF               OCCURS 4.
      *������ݸ��ֹ�
               05  SKYMAIN-KOHNUM           PIC  X(03).
      *����������
           03  SKYMAIN-JNISSUINF.
      *������
               05  SKYMAIN-JNISSU           PIC  9(02).
      *���Ŷ�ʬ����������
           03  SKYMAIN-SRYKBNINF           OCCURS  45.
      *��������
               05  SKYMAIN-KHNTEN          PIC  9(08).
      *���
               05  SKYMAIN-KAISU           PIC  9(05).
      *������������
               05  SKYMAIN-TENSUINF.
      *��������
                   07  SKYMAIN-TENSU       PIC  S9(08).
      *�����������
           03  SKYMAIN-TOTALTENINF.
      *�������
               05  SKYMAIN-TOTALTEN        PIC S9(08).
      *��ô��۾���
           03  SKYMAIN-FTNMONEYINF.
      *��ô���
               05  SKYMAIN-FTNMONEY        PIC S9(09).
      *��ô��ۡʰ�����ô�ۡ�
               05  SKYMAIN-FTNMONEY-FTN    PIC S9(09).
      *��ô��ۡʹ����ۡ�
               05  SKYMAIN-FTNMONEY-KOJ    PIC S9(09).
      *
      *�������������
           03  SKYMAIN-SHOKUJIINF.
      *��������
               05  SKYMAIN-SHOKUJINISSU    PIC  9(02).
      *����������
               05  SKYMAIN-SHOKUJIRYOYOHI  PIC  9(09).
      *ɸ����ô��
               05  SKYMAIN-SHOKUJIFTN      PIC  9(09).
      *
      *������ô�⡦����������ɸ����ô�۶�ʬ
           03  SKYMAIN-SHOKUJIFTNKBN       PIC  X(01).
      *
      *���ް�����ô�����
           03  SKYMAIN-YKZFTNINF.
      *���ް�����ô��
               05  SKYMAIN-YKZFTN          PIC  9(05).
      *
      *����۾���ʰ������
           03  SKYMAIN-SKYIRYO-INF.
      *������
               05  SKYMAIN-SKYIRYO-NISSU   PIC 9(02).
      *������
               05  SKYMAIN-SKYIRYO-TOTALTEN
                                           PIC S9(08).
      *������ô
               05  SKYMAIN-SKYIRYO-ICHIBUFTN
                                           PIC S9(09).
      *�ݸ�����ô
               05  SKYMAIN-SKYIRYO-HOKENJA PIC S9(09).
      *������ô
               05  SKYMAIN-SKYIRYO-KOHFTN  PIC S9(09).
      *
      *����۾���ʿ�����
           03  SKYMAIN-SKYSKJ-INF.
      *���
               05  SKYMAIN-SKYSKJ-KAISU     PIC 9(02).
      *�����
               05  SKYMAIN-SKYSKJ-RYOYOHI   PIC 9(09).
      *������ô
               05  SKYMAIN-SKYSKJ-ICHIBUFTN PIC 9(09).
      *�ݸ�����ô
               05  SKYMAIN-SKYSKJ-HOKENJA   PIC 9(09).
      *������ô
               05  SKYMAIN-SKYSKJ-KOHFTN    PIC 9(09).
      *�ݸ��ȹ礻����
           03  SKYMAIN-HKNCOMBI-INF.
               05  SKYMAIN-HKNCOMBI         PIC 9(04)   OCCURS 10.
      *
      *�õ�����
           03  SKYMAIN-TOKKIINF.
               05  SKYMAIN-TOKKI          PIC  X(02)   OCCURS  5.
      *
      *�õ�����ά��
           03  SKYMAIN-TOKKINMINF.
               05  SKYMAIN-TOKKINM        PIC  X(12)   OCCURS  5.
      *
      *�ե饰
           03  SKYMAIN-FLGINF.
               05  SKYMAIN-FLG            PIC  9(01)   OCCURS  5.
      *
      *�ǡ������ꥢ
           03  SKYMAIN-RESERVE-AREA       PIC  X(50).
      *
      *����ؼ��ե饰
           03  SKYMAIN-HENREI-SIJI-FLG    PIC  9(01).
      *ľ������ե饰
           03  SKYMAIN-DIRECT-FLG         PIC X(01).
      *
      *�쥻�ץȵ��ܳ������������ե饰��1:���ܳ�������������
           03  SKYMAIN-ETCCREATEFLG        PIC  9(01).
      *�ݸ���ʬ�ʣ��������ա�ϫ�ҽ��ˡ����������աʷ��ݽ��ˡ�
           03  SKYMAIN-HKNKBN-RSI          PIC X(01).
      *(����ɽ��)
      *���ݸ����ݸ��ֹ�
           03  SKYMAIN-HKNNUM-S            PIC  X(03).
      *�ݸ����ֹ� 
           03  SKYMAIN-HKNJANUM-S          PIC  X(08).
      *��Ͽǯ����
           03  SKYMAIN-CREYMD              PIC  X(08).
      *��Ͽ����
           03  SKYMAIN-CREHMS              PIC  X(06).
      *����ǯ����
           03  SKYMAIN-UPDYMD              PIC  X(08).
      *��������
           03  SKYMAIN-UPDHMS              PIC  X(06).
      *�祭�������ѹ���
           03  SKYMAIN-UPHKNJANUM          PIC X(10).
      *�ݸ����ֹ渡���ѹ���
           03  SKYMAIN-XXPREFNUM           PIC X(02).
      *����ǯ����ѹ���
           03  SKYMAIN-XXSRYYM             PIC 9(06).
      *�о��ݸ����ֹ渡���ѹ��� 
           03  SKYMAIN-XXSTHKNJANUM        PIC X(08).
           03  SKYMAIN-XXEDHKNJANUM        PIC X(08).
      *������ʧ���ظ����ѹ���
           03  SKYMAIN-XXTEISYUTUSAKI      PIC 9(01).

