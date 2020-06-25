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
      *    �������(�������)   COPY (CPSKYKOH.INC)              *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.06.01    NACL-ƣ��    10/10/12  ����ñ�ȤΥ쥻ʬΥ�б�
      *                                     ������kohid �ɲ�
      *  04.06.03    NACL-ƣ��    12/10/26  �õ����ࣰ����Ϸʻ�˵ڤ�
      *                                     ������Ϸ��ˤΥ쥻�ץ��б�
      *
      *  04.07.01    NACL-ƣ��    13/09/20  �軰�Թ԰��б�
      *
      *  04.08.01    NACL-ƣ��    17/04/10  ����ñ�̤Ǥ�������������
      *                                     ��ϫ�ҽ�����Ϸ��ݽ����б�
      *                                     �ݸ���ʬ�����ѹ��ܤ��ɲ�
      ************************************************************
       01  SKYKOH-REC.
      *�ˣţ�
           03  SKYKOH-KEY.
               05  SKYKOH-KEY1.
      *���ŵ��أɣ�
                   07  SKYKOH-HOSPNUM          PIC 9(2).
      *����ǯ��
                   07  SKYKOH-SRYYM            PIC  9(06).
      *������ʬ
                   07  SKYKOH-NYUGAIKBN        PIC  X(01).
      *���ԣɣ�
                   07  SKYKOH-PTID             PIC  9(10).
      *�쥻�ץȲʡʶ���λ������򥻥åȡ�
                   07  SKYKOH-RECEKA           PIC  X(02).
      *������ʧ����
                   07  SKYKOH-TEISYUTUSAKI     PIC  9(01).
      *�쥻�ץȼ���
                   07  SKYKOH-RECESYUBETU      PIC  X(04).
      *�ݸ����ֹ�
                   07  SKYKOH-HKNJANUM         PIC  X(08).
      *�����ʬ�������ݸ��ΤȤ��Τߡ�
                   07  SKYKOH-HOJOKBN-KEY      PIC  X(01).
      *����ɣġʸ���ñ�ȤΤߡ�
                   07  SKYKOH-KOHID-KEY        PIC  9(10).
      *Ŭ�ѳ���ǯ����
                   07  SKYKOH-TEKSTYMD         PIC  X(08).
      *ʬ���ʬ
                   07  SKYKOH-RECEIPT-KBN      PIC  X(10).
      *Ϣ�ֹ�(1��4)
               05  SKYKOH-RENNUM           PIC  9(02).
      *�������
           03  SKYKOH-KOHINF.
      *������ݸ��ֹ�
               05  SKYKOH-KOHNUM           PIC  X(03).
      *�������
           03  SKYKOH-KOHIDINF.
      *����ݸ���ɣ�
               05  SKYKOH-KOHID            PIC  9(10).
      *����������
           03  SKYKOH-JNISSUINF.
      *������
               05  SKYKOH-JNISSU           PIC  9(02).
      *���Ŷ�ʬ����������
           03  SKYKOH-SRYKBNINF           OCCURS  45.
      *������������
               05  SKYKOH-TENSUINF.
      *��������
                   07  SKYKOH-TENSU       PIC  S9(08).
      *�����������
           03  SKYKOH-TOTALTENINF.
      *�������
               05  SKYKOH-TOTALTEN        PIC S9(08).
      *��ô��۾���
           03  SKYKOH-FTNMONEYINF.
      *��ô���
               05  SKYKOH-FTNMONEY        PIC S9(09).
      *��ô��ۡʰ�����ô�ۡ�
               05  SKYKOH-FTNMONEY-FTN    PIC S9(09).
      *��ô��ۡʹ����ۡ�
               05  SKYKOH-FTNMONEY-KOJ    PIC S9(09).
      *
      *�������������
           03  SKYKOH-SHOKUJIINF.
      *��������
               05  SKYKOH-SHOKUJINISSU    PIC  9(02).
      *����������
               05  SKYKOH-SHOKUJIRYOYOHI  PIC  9(09).
      *ɸ����ô��
               05  SKYKOH-SHOKUJIFTN      PIC  9(09).
      *
      *���ް�����ô�����
           03  SKYKOH-YKZFTNINF.
      *���ް�����ô��
               05  SKYKOH-YKZFTN          PIC  9(05).
      *������ʧ���ظ����ѹ���
           03  SKYKOH-XXTEISYUTUSAKI      PIC 9(01).
      *�ݸ��ֹ渡���ѹ���
           03  SKYKOH-XXHKNNUM            PIC X(03).
      *�ݸ���ʬ�����ѹ���
           03  SKYKOH-XXHKNKBN-RSI        PIC X(01).