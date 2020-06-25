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
      *    �������(�쥻�ץȵ��ܳ��������) COPY (CPSKYETC.INC)  *
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
       01  SKYETC-REC.
      *�ˣţ�
           03  SKYETC-KEY.
      *���ŵ��أɣ�
               05  SKYETC-HOSPNUM          PIC 9(2).
      *����ǯ��
               05  SKYETC-SRYYM            PIC  9(06).
      *������ʬ
               05  SKYETC-NYUGAIKBN        PIC  X(01).
      *���ԣɣ�
               05  SKYETC-PTID             PIC  9(10).
      *�쥻�ץȲʡʶ���λ������򥻥åȡ�
               05  SKYETC-RECEKA           PIC  X(02).
      *������ʧ����
               05  SKYETC-TEISYUTUSAKI     PIC  9(01).
      *�쥻�ץȼ���
               05  SKYETC-RECESYUBETU      PIC  X(04).
      *�ݸ����ֹ�
               05  SKYETC-HKNJANUM         PIC  X(08).
      *�����ʬ�������ݸ��ΤȤ��Τߡ�
               05  SKYETC-HOJOKBN-KEY      PIC  X(01).
      *����ɣġʸ���ñ�ȤΤߡ�
               05  SKYETC-KOHID-KEY        PIC  9(10).
      *Ŭ�ѳ���ǯ����
               05  SKYETC-TEKSTYMD         PIC  X(08).
      *ʬ���ʬ
               05  SKYETC-RECEIPT-KBN      PIC X(10).
      *�쥻�ץȵ��ܳ��������
           03  SKYETC-ETC-KOH-G.
      *���ܳ�������ݸ��ֹ�
               05  SKYETC-ETC-KOHINF.
                   07  SKYETC-ETC-KOHNUM   PIC  X(03)   OCCURS  4.
      *���ܳ�����ݼ�����
               05  SKYETC-ETC-JNISSUINF.
                   07  SKYETC-ETC-JNISSU   PIC  9(02)   OCCURS  4.
      *���ܳ�����ݹ������
               05  SKYETC-ETC-TOTALTENINF.
                   07  SKYETC-ETC-TOTALTEN PIC S9(08)   OCCURS  4.
      *���ܳ��������ô���
               05  SKYETC-ETC-FTNMONEYINF               OCCURS  4.
                   07  SKYETC-ETC-FTNMONEY     PIC S9(09).
                   07  SKYETC-ETC-FTNMONEY-FTN PIC S9(09).
                   07  SKYETC-ETC-FTNMONEY-KOJ PIC S9(09).
      *���ܳ���������ް�����ô�����
               05  SKYETC-ETC-YKZFTNINF.
                   07  SKYETC-ETC-YKZFTN   PIC  9(05)   OCCURS  4.
      *���ܳ�����ݿ������������
               05  SKYETC-ETC-SHOKUJIINF       OCCURS  4.
      *��������
                   07  SKYETC-ETC-SHOKUJINISSU PIC  9(02).
      *����������
                   07  SKYETC-ETC-SHOKUJIRYOYOHI
                                               PIC  9(09).
      *ɸ����ô��
                   07  SKYETC-ETC-SHOKUJIFTN   PIC  9(09).
      *������ʧ���ظ����ѹ���
           03  SKYETC-XXTEISYUTUSAKI      PIC 9(01).
      *�ݸ��ֹ渡���ѹ���
           03  SKYETC-XXHKNNUM            PIC X(03).
      *�ݸ���ʬ�����ѹ���
           03  SKYETC-XXHKNKBN-RSI        PIC X(01).